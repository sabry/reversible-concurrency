-- To run with true parallelism, use: ghci +RTS -N -RTS. Of course, you
-- need a machine with at least 2 cores to take advantage of this.

-- However, due to some race conditions which exist right now, it's
-- actually preferablly to test this without parallelism, instead
-- allowing ghc to use it's internal scheduler for concurency.
--
import Control.Concurrent hiding (yield,newChan)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield)
import qualified Control.Concurrent as CC (newChan)
import Control.Monad.State
import Debug.Trace
import qualified Data.Map as Map

-- Honestly, there's no Data.Stack?
-- Quick stack implementation
type Stack a = [a]
empty :: Stack a
empty = []
isEmpty :: Stack a -> Bool
isEmpty = null
push :: a -> Stack a -> Stack a
push = (:)
top :: Stack a -> a
top = head
pop :: Stack a -> (a,Stack a)
pop (s:ss) = (s,ss)


type PID = Int -- ID of a process
-- Speculative tag lets us know if something is involved in a
-- speculative computation or not, i.e., if it appears between a choose
-- and backtrack.
type SpecTag = Bool
-- XXX: This probably needs Chan Msg
type Ch = (Chan Int, SpecTag)

-- KType indicates the type of a continuation, which is needed to figure
-- out what to do when backtracking
-- XXX: This probably needs Msg instead of Int
data KType = Sent Ch Int
           | Recv Ch Int
           | Choose

-- Messages that can be sent over the channels.
-- 1) A send message, the normal kind of message where you send data.
-- 2) Unsend message, telling a thread to pretend it never received a
--    a message on this channel. XXX: I think this needs more info, like
--    a message ID.
-- 3) Backtrack message, telling a thread to pop a continuation.
-- 4) Continue, tells the thread to run the current continuation.
data Msg = Send Int
         | Unsend Int
         | Backtrack
         | Continue

-- Each thread needs to keep track of various information, including if
-- it's speculative, a stack of continuations, and a list of speculative
-- channels.
type ThreadMap r a = Map PID (SpecTag, Stack ((Proc r a), KType), [Ch])
type Comp r a = StateT (PID, ThreadMap r a) a

-- I use the continuation monad here, not for concurrency, but to
-- allow me to wrap final answers in a list, easing the collection of
-- answers from all threads, and allowing nondeterminism to be added
-- more easily.
type Proc r a = Cont (Comp IO [r]) a

-- Some interface methods
getPID :: Proc r PID
getPID = fmap fst get

getThreadMap :: Proc r (ThreadMap r a)
getThreadMap = fmap get snd

-- _getThreadMapVal :: Proc r (...)
-- Get the value portion of the map, given it's key.
_getThreadMapVal pid = fmap getThreadMap (! pid)

-- Given a function similar to fst or snd, over a triple, return that
-- function called on the ThreadMap value.
--_getFThreadMapVal :: PID -> ?
_getFThreadMapVal f = fmap _getThreadMapVal f

getSpecTag :: Proc r SpecTag
getSpecTag pid = fmap (_getThreadMapVal pid) \(x,_,_) -> x

getStack pid = fmap (_getThreadMapVal pid) \(_,x,_) -> x

getSpecChans :: Proc r [Ch]
getSpecChans pid = fmap (_getThreadMapVal pid) \(_,_,x) -> x

-- end of interface

par :: Show r => Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> do
  ppid <- getPID
  map <- getThreadMap
  let b1 = runStateT (c1 k) (ppid+1, map)
  let b2 = runStateT (c2 k) (ppid+2, map)
  liftIO $ runPar b1 b2)

-- Still useful in a concurrent environment, but useless in true
-- parallel environment. 
yield :: Proc r ()
yield = Cont (\k -> do 
  liftIO CC.yield; k ())

newChan :: Proc r (Chan a)
newChan = Cont (\k -> do ch <- liftIO CC.newChan; k ch)

-- <s>TODO</s>: Need to alter so that, when we send on a channel, if the
-- current thread is speculative, we mark the channel as speculative. We
-- must also push a properly tagged continuation onto the stack.
send :: Chan Int -> Int -> Proc r ()
send ch s = callCC $ \cc -> Cont (\k -> do 
  pushMyKStack cc
  specTag <- getMySpecTag
  pid <- getPID
  if specTag 
    then setSpecChan ch
    else nothing
  liftIO $ putStrLn ("PID: " ++ (show pid) ++ " Sending: " ++ (show s))
  liftIO $ writeChan ch s
  k ());
  

-- <s>TODO</s>: Same as send.
recv :: Chan Int -> Proc r Int
recv ch = callCC $ \cc -> Cont (\k -> do 
  pushMyKStack cc 
  i <- liftIO $ readChan ch
  pid <- getPID
  trace ("PID: " ++ show pid ++ " Recv: " ++ (show i)) $ return ()
  k i)

-- <s>TODO</s>: We need to mark the current thread a speculative, and push a
-- Choose continuation onto the stack for this thread.
choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) (Cont c2) = Cont (\k -> do 
  setSpecTag
  st <- get
  b1 <- liftIO $ runStateT (c1 k) st
  b2 <- liftIO $ runStateT (c2 k) st
  liftIO $ return $ b1 ++ b2)

-- TODO: When we reach a backtrack, we need to send a message out on all
-- speculative channels telling them to backtrack. They should
-- immediately pop a continuation, and wait for more messages.
--
-- This thread should begin poping continuation, sending unsend and
-- unrecv messages, until it reaches a choose statement. At the choose
-- statement, it should send a Continue message
backtrack :: Proc r a
backtrack = Cont (\k -> do
  pid <- getPID
  liftIO $ putStrLn ("PID: " ++ (show pid) ++ " backtracking..")
  liftIO $ return [])

wrapProc :: Show r => IO [r] -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  xs <- p
--  putStrLn ("Returning: " ++ (show xs))
  modifyMVar_ mvar (\ls -> return $ xs ++ ls)
  putMVar block ()

forkChild :: MVar [r] -> MVar [MVar ()] -> 
            (MVar () -> MVar [r] -> IO ()) -> IO ThreadId
forkChild results children io = do
  block <- newEmptyMVar 
  modifyMVar_ children (\xs -> return $ block:xs)
  forkIO $ io block results

waitOnChildren :: MVar [MVar ()] -> IO ()
waitOnChildren children = do
  childs <- takeMVar children
  case childs of
    [] -> return ()
    (x:xs) -> do
    putMVar children xs
    takeMVar x
    waitOnChildren children

runPar :: Show r => IO [r] -> IO [r] -> IO [r]
runPar k1 k2 = do
  children <- newMVar []
  results <- newMVar []
  -- We might cause some unintentional ordering by spawning one of these
  -- first, as it gets a head start. But.. I think that's unavoidable.
  forkChild results children $ wrapProc k1
  forkChild results children $ wrapProc k2
  -- Block until all children have returned
  waitOnChildren children
  takeMVar results

runProc :: Show r => Proc r r -> IO [r]
runProc p = runReaderT (runCont p (\r -> return [r])) 0

-- All the tests have data returned in an unexpected order. This make
-- sense in some of the tests, as the parallelism is non-deterministic as
-- to which one will finish first. The tests with explicit yields,
-- however, are slightly more interesting. While in a concurrent
-- environment, without true parallelism, the yield would force one
-- thread to wait, in a true parallel environment, with enough cores for
-- all threads, the yield is useless. 
--
-- Unfortunately, this can cause a race condition in some of the
-- assumptions we make, for instance, in test5. We assume that, having
-- yielded, the second thread will receive the value we sent before we
-- manage to receive. This might not be the case, however, if the first
-- thread is running faster than the second thread.
--
--
-- Expected: [1,2]
-- Result: [2,1] or [1,2]
test1 :: Proc Int Int
test1 = (par (return 1) (return 2))

-- Expected: [3,1,2]
-- Results: [2,1,3]
test2 :: Proc Int Int
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))

-- Expected: [0,3]
-- Results: [0,3]
test3 :: Proc Int Int
test3 = do ch <- newChan
           (par
              (do send ch 2
                  return 0)
              (do x <- recv ch
                  return (x+1)))

-- Expected: [1,0,11]
-- Results: [0,11,1]
test4 :: Proc Int Int
test4 = do ch <- newChan
           (foldr1 par
                 [do x <- recv ch; send ch (x+1); return 0,
                  return 1,
                  do send ch 10; yield; y <- recv ch; return y])

-- Expected: [10, 1]
-- Results: [1,1,10] or [2,0]
--
-- There's a race condition caused by the channel, it seems. We can
-- occasionally pull off the x we sent before the other thread has had
-- a chance to receive it, despite the yield. It's interesting that
-- we're both able to pull the value off the channel simultaneously,
-- however.
--
-- The only solution to this issue I can see is named channels.
--
-- What's more curious is the result we normally get, being [1,1,10]. I
-- haven't figured out why we get two 1's. Maybe it's due to both
-- duplicated continutations and true parallelism.
test5 :: Proc Int Int
test5 = do ch1 <- newChan
           ch2 <- newChan
           par
            (do x <- choose (return 1) (return 2)
                send ch1 x
                yield
                y <- recv ch2
                if y == 0
                  then backtrack
                  else return y)
            (do a <- recv ch1
                if a == 1
                  then do send ch2 0; return 0
                  else do send ch2 1; return 10)
