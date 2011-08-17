-- To run with true parallelism, use: ghci +RTS -N -RTS. Of course, you
-- need a machine with at least 2 cores to take advantage of this.

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
  pid <- ask
  liftIO $ putStrLn ("PID: " ++ (show pid) ++ " Sending: " ++ (show s))
  liftIO $ writeChan ch s
  k ())

recv :: Chan Int -> Proc r Int
recv ch = Cont (\k -> do 
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
backtrack = Cont (\k -> liftIO $ return [])

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
-- Results: [0,1,11]
test4 :: Proc Int Int
test4 = do ch <- newChan
           (foldr1 par
                 [do x <- recv ch; send ch (x+1); return 0,
                  return 1,
                  do send ch 10; yield; y <- recv ch; return y])

-- Expected: [10, 1]
-- Results: diverge
--
-- It seems after backtracking, this waits forever in the second
-- process.
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
