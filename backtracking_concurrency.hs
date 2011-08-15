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
import Control.Monad.Reader
import Debug.Trace

type PID = Int
type Comp a = ReaderT PID a

-- I use the continuation monad here, not for concurrency, but to
-- allow me to wrap final answers in a list, easing the collection of
-- answers from all threads, and allowing nondeterminism to be added
-- more easily.
type Proc r a = Cont (Comp IO [r]) a

par :: Show r => Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> do
  ppid <- ask
  let b1 = runReaderT (c1 k) (ppid+1)
  let b2 = runReaderT (c2 k) (ppid+2)
  liftIO $ runPar b1 b2)

-- Still useful in a concurrent environment, but useless in true
-- parallel environment. 
yield :: Proc r ()
yield = Cont (\k -> do 
  liftIO CC.yield; k ())

newChan :: Proc r (Chan a)
newChan = Cont (\k -> do ch <- liftIO CC.newChan; k ch)

send :: Chan Int -> Int -> Proc r ()
send ch s = Cont (\k -> do 
  liftIO $ writeChan ch s
  k ())

recv :: Chan Int -> Proc r Int
recv ch = Cont (\k -> do 
  -- Since we're using real threads, and the channel
  -- is blocking, I shouldn't need to manually yield
  -- here... right?
  i <- liftIO $ readChan ch
  pid <- ask
  trace ("PID: " ++ show pid ++ " Recv: " ++ (show i)) $ return ()
  k i)

choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) (Cont c2) = Cont (\k -> do 
  pid <- ask
  let b1 = runReaderT (c1 k) pid
  let b2 = runReaderT (c2 k) pid
  ls1 <- liftIO b1
  ls2 <- liftIO b2
  liftIO $ return $ ls1 ++ ls2)

backtrack :: Proc r a
backtrack = Cont (\k -> return [])

wrapProc :: Show r => IO [r] -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  xs <- p
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
-- Results: [3,0]
test3 :: Proc Int Int
test3 = do ch <- newChan
           (par
              (do send ch 2
                  return 0)
              (do x <- recv ch
                  return (x+1)))

-- Expected: [1,0,11]
-- Results: [11,1,0]
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
