import Control.Concurrent hiding (yield)
import qualified Control.Concurrent as CC (yield)
import Control.Monad.List
import Control.Monad.Reader
import Debug.Trace

data Action r = Res r
              | Susp (() -> Action r) 
              | Par (Action r) (Action r)

type Proc r = ReaderT (MVar [r]) IO [r]

wrapProc :: Show r => Proc r -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  xs <- runProc p
  trace (show xs) (return ())
  liftIO $ modifyMVar_ mvar (\ls -> trace (show ls) $ return $ xs ++ ls)
  liftIO $ putMVar block ()

forkChild :: MVar [r] -> MVar [MVar ()] -> (MVar () -> MVar [r] -> IO ()) -> IO ThreadId
forkChild results children io = do
  -- Add a result for the child
  block <- liftIO $ newEmptyMVar 
  liftIO $ modifyMVar_ children (\xs -> return (block:xs))
  forkIO $ io block results

par :: Show r => Proc r -> Proc r -> Proc r
par p1 p2 = do
  children <- liftIO $ newMVar []
  results <- ask
  liftIO $ forkChild results children $ wrapProc p1
  liftIO $ forkChild results children $ wrapProc p2
  yield
  xs <- liftIO $ takeMVar children
  return $ map takeMVar xs
  liftIO $ takeMVar results

yield :: Proc ()
yield = do x <- liftIO $ CC.yield; f_return x

runProc :: Proc r -> IO [r]
runProc = undefined 

f_return x = return [x]

test1 = (par (f_return 1) (f_return 2))
test2 = (par (do yield; (par (f_return 1) (f_return 2)))
             (f_return 3))
