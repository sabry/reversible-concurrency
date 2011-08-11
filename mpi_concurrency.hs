import Control.Concurrent hiding (yield)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield)
import Control.Monad.Reader
import Debug.Trace

-- I use the continuation monad here, not for concurrency, but for to
-- allow me to wrap final answers in a list. This further
-- allows me to do other things more easily, and hopefully allows for
-- easier abstraction later. 
type Comp a = ReaderT (Chan Int) a

type Proc r a = Cont (Comp IO [r]) a

par :: Show r => Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k ->
  do ch <- ask
     liftIO $ runPar (runReaderT (c1 k) ch) (runReaderT (c2 k) ch))

yield :: Proc r ()
yield = Cont (\k -> do liftIO $ CC.yield; k ())

send :: Int -> Proc r ()
send s = Cont (\k -> do ch <- ask
                        liftIO $ writeChan ch s
                        k ())

recv :: Proc r Int
recv = Cont (\k -> do ch <- ask
                      -- Since we're using real threads, I shouldn't
                      -- need to manually yield here.
                      i <- liftIO $ readChan ch
                      k i)
wrapProc :: Show r => IO [r] -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  xs <- p
  return $ trace (show xs) ()
  modifyMVar_ mvar (\ls -> return $ xs ++ ls)
  putMVar block ()

forkChild :: MVar [r] -> MVar [MVar ()] -> 
            (MVar () -> MVar [r] -> IO ()) -> IO ThreadId
forkChild results children io = do
  block <- newEmptyMVar 
  modifyMVar_ children (\xs -> return (block:xs))
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
  forkChild results children $ wrapProc k1
  forkChild results children $ wrapProc k2
  -- Block until all children have returned
  waitOnChildren children
  res <- takeMVar results
  return res

runProc :: Show r => Proc r r -> IO [r]
runProc p = do
  ch <- newChan
  runReaderT (runCont p (\r -> return [r])) ch

test1 :: Proc Int Int
test1 = (par (return 1) (return 2))
test2 :: Proc Int Int
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))
test3 :: Proc Int Int
test3 = (par
         (do send 2
             return 0)
         (do x <- recv
             return (x+1)))
test4 :: Proc Int Int
test4 = (foldr1 par
                  [do x <- recv; send (x+1); return 0,
                   return 1,
                   do send 10; yield; y <- recv; return y])
