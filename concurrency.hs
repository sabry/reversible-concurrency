import Control.Concurrent hiding (yield)
import qualified Control.Concurrent as CC (yield)
import Control.Monad.Trans.List
import Control.Exception
import Control.Monad.Reader 
import Debug.Trace

data Action r = Res r |
                Susp (() -> Action r) |
                Par (Action r) (Action r)

type Proc r a = ReaderT (MVar [r]) (ListT IO) a

waitForChildren :: MVar [MVar ()] -> IO ()
waitForChildren children = do
  xs <- takeMVar children
  case xs of
       [] -> return ()
       (x:xs) -> do
          putMVar children xs
          takeMVar x
          waitForChildren children

forkChild :: MVar [MVar ()] -> (MVar () -> IO ()) -> Proc r ThreadId
forkChild children io = do
  block <- liftIO $ newEmptyMVar
  liftIO $ modifyMVar_ children (\xs -> return (block:xs)) 
  liftIO . forkIO $ io block

wrapProc :: Show r => Proc r r -> Proc r (MVar () -> IO ())
wrapProc p = do
  mres <- ask
  return $ \block -> do 
    res <- runProc p 
    modifyMVar_ mres (\mres -> return (res ++ mres))
    putMVar block ()


par :: Show r => Proc r r -> Proc r r -> Proc r r
par p1 p2 = do 
  children <- liftIO . newMVar $ []
  io1 <- wrapProc p1
  io2 <- wrapProc p2
  forkChild children $ io1 
  forkChild children $ io2 
  liftIO $ waitForChildren children
  -- Well, since the state has been updated, what do we return? The
  -- return doesn't matter much so...
  p2

runProc :: Proc r r -> IO [r]
runProc p = do
  mvar <- liftIO $ newMVar []
  runListT $ runReaderT p mvar
  takeMVar mvar


yield :: Proc r ()
yield = liftIO $ CC.yield 

test1 :: Num r => Proc r r
test1 = (par (return 1) (return 2))
test2 :: Num r => Proc r r
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))
