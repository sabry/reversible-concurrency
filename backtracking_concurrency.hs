-- I've taken up not deleting TODO and XXX tags, as they might be useful as
-- notes if/when I have to write about this. Instead, completed ones are
-- surrounded by HTML strike tags (<s></s>) 
--
-- To run with true parallelism, use: ghci +RTS -N -RTS. Of course, you
-- need a machine with at least 2 cores to take advantage of this.

-- <s>However, due to some race conditions which exist right now, it's
-- actually preferablly to test this without parallelism, instead
-- allowing ghc to use it's internal scheduler for concurency.</s>
--
import Control.Concurrent hiding (yield,newChan)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield,newChan)
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

-- end stack implementation

type Pid = Int -- ID of a process
type MsgId = Int -- ID for a message on a channel.
type ChId = Int -- Take a wild guess.

-- Speculative tag lets us know if something is involved in a
-- speculative computation or not, i.e., if it appears between a choose
-- and backtrack.
type SpecTag = Bool
type Ch = (Chan Msg, Chan Msg, SpecTag, MsgId)

-- KType indicates the type of a continuation, which is needed to figure
-- out what to do when backtracking
data KType = Sent ChId MsgId
           | Recv ChId MsgId
           | Choose

-- Messages that can be sent over the channels.
-- 1) A send message, the normal kind of message where you send data.
-- 2) Unsend message, telling a thread to pretend it never received a
--    a message on this channel. 
-- 3) Backtrack message, telling a thread to pop a continuation.
-- 4) Continue, tells the thread to run the current continuation.
-- 5) Complete tells the thread whichever thread caused the speculation
--    has returned successfully, so it can stop listening for a
--    backtrack message.
-- 6) Acknowledge, which is used for by a receiving thread to
--    the message, and let the sending thread know if it's speculative.
--    Send and acknowledge message ID's should be the same.
-- 7) Unreceive might not be totally necessary, if the sending threads
--    can keep track of sending to speculative threads. We'll see.
data Msg = Send SpecTag MsgId Int
         | Acknowledge SpecTag MsgId
         | Unsend MsgId
         | Unrecv MsgId
         | Backtrack
         | Continue
         -- Complete shouldn't need extra data. It just says 'stop
         -- listening on this channel'
         | Complete 

-- Each thread needs to keep track of various information, including if
-- it's speculative, a stack of continuations, and a list of channels it
-- has received speculative information on, and a list it has sent
-- speculative information on.
--
-- <s>XXX: We need to track channels we sent on and received on
-- seperately. channels we sent on, we need to listen for backtrack
-- messages, ... on second thought, do we? What if we just keep the
-- channels, and then listen on the receiving end. If we get a backtrack
-- message, we backtrack, and keep listening on that channel. If we get
-- an unsend/unrecv on it, we process it. Why do we care if we sent or
-- received on it?</s>
type ThreadMap r a = Map PID (SpecTag, Stack ((Proc r a), KType), [ChId])
-- <s>XXX: This [Ch] should probably be more like Map ChId Ch. We
-- need this to keep track of message id's for each channel.</s> 
type Comp r a = StateT (PID, ThreadMap r a, Map ChId Ch) a

-- Continuations used for backtracking through a stack of continuations.
-- XXX: I'm a little worried about the mutual recursion in these
-- types... I'm pretty sure I can't do that, but we'll wait until I ask
-- the type checker. or until I make a decision about the next XXX
--
-- XXX: Maybe ThreadMap should have a stack of Cont ?? (). That would
-- simplify the interface too.
type Proc r a = Cont (Comp r IO r) a

-- Some interface methods
-- XXX: Most of these are now incorrect. 
-- TODO: Lots of interfaces methods are missing.
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

-- TODO: Should runStateT with the same state, except the current
-- thread's pid incremented.
runChild = undefined

-- end of interface

par :: Show r => Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> do
  let b1 = runChild (c1 k) -- runStateT (c1 k) (ppid+1, map)
  let b2 = runChild (c2 k) --runStateT (c2 k) (ppid+2, map)
  liftIO $ runPar b1 b2)

-- Still useful in a concurrent environment, but useless in true
-- parallel environment. 
yield :: Proc r ()
yield = Cont (\k -> do 
  liftIO CC.yield; k ())

-- TODO: Alter to create a full duplex channel, init message IDs, etc.
-- Should also mark as speculative if the current thread is.
newChan :: Proc r (Chan a)
newChan = Cont (\k -> do ch <- liftIO CC.newChan; k ch)

-- XXX: I'm not sure if callCC works here, in this way. I'm pretty sure
-- it doesn't.
send :: Ch -> Int -> Proc r ()
send ch s = callCC $ \cc -> Cont (\k -> do 
  -- TODO: Lots of undefined methods. At this point, they're pretty much
  -- methods, given the way I'm using them is rather object oriented.
  -- XXX: Double check that these are the right data structure, i.e. the
  -- order of variables.
  msg@(Send spec_s p msgid_s) <- buildMsg ch s
  sendMsg ch msg
  (Acknowledge spec_a msgid_a) <- recvAck ch s
  --liftIO $ putStrLn ("PID: " ++ (show pid) ++ " Sending: " ++ (show s))
  --liftIO $ writeChan ch $ msg
  if spec_s `or` spec_a
    then tagSendCh ch
    else nothing
  -- XXX: Sanity crash
  if msgid_s != msgid_a
    then undefined
    else nothing
  pushKStack cc $ Send msgid_s
  k ());
  

-- TODO: Same as send.
recv :: Ch -> Proc r Int
recv ch = callCC $ \cc -> Cont (\k -> do 
  (Send spec_s p msgid_s) <- recvMsg ch
  ack@(Acknowledge spec_a msgid_a) <- buildAck ch 
  sendAck ack ch
  if spec_s `or` spec_a
    then tagRecvCh ch
    else nothing
  -- XXX: Sanity crash
  if msgid_s != msgid_a
    then undefined
    else nothing
  pushKStack cc $ Recv msgid_a
  k i)

-- XXX: Same note as send for callCC and undefined methods
choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) k2@(Cont c2) = callCC $ \cc -> Cont (\k -> do 
  -- XXX: This seems a little naive. We store the next choice, and
  -- always take the first one first. There feels like a little room for
  -- abstraction here.
  -- It also hides the fact that the second choice is ever used... I
  -- don't like this solution.
  pushKStack cc $ Choose k2
  tagThread
  c1 k)
--  st <- get
--  b1 <- liftIO $ runStateT (c1 k) st
--  liftIO $ return b1)

-- TODO: Implement
--backToChoice :: ???
-- backToChoice = do
--   pop a continuation.
--   case:
--      if send then sendUnsend; backToChoice
--      if recv then sendUnrecv; backToChoice
--      if choose then return 
  
-- TODO: Implement
-- sendBackTrack :: ??
-- sendBackTrack = do
--   get speculative (probably all?) channels
--   send backtrack message
--   do we need the acknowledgment?

-- When we reach a backtrack, we need to send a message out on all
-- speculative channels telling them to backtrack. They should
-- immediately pop a continuation, and wait for more messages.
--
-- This thread should begin popping continuation, sending unsend and
-- unrecv messages, until it reaches a choose statement. At the choose
-- statement, it should send a Continue message
backtrack :: Proc r a
backtrack = Cont (\_ -> do
  sendBacktrack
  k <- backToChoice
  k ())
  --pid <- getPID
  --liftIO $ putStrLn ("PID: " ++ (show pid) ++ " backtracking..")
  --liftIO $ return [])

-- TODO: Change so we only return a single value. When run in parallel,
-- the second task should return the value, while the first can just
-- print it out. or do whatever. I don't really care.
-- XXX: I wonder if I can now make use of the built in par. The only
-- concern there is I won't be guaranteed to have them run in parallel.
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
