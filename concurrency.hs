import Control.Concurrent
import Control.Monad.Trans.List
import Control.Exception
import Control.Monad.Reader 

type Proc r a = ReaderT (MVar [r], MVar [MVar ()]) IO a

wrapProc :: Proc r r -> MVar () -> Proc r () 
wrapProc p mvar =
  do st <- ask
     res <- liftIO $ runReaderT p st
     xs <- liftIO $ takeMVar $ fst st
     liftIO $ putMVar (fst st) (res : xs)
     liftIO $ putMVar mvar ()

forkChild :: (MVar () -> Proc r ()) -> Proc r ThreadId
forkChild exp = do st <- ask
                   mvar <- liftIO $ newEmptyMVar 
                   liftIO $ modifyMVar_ (snd st) (\xs -> return (mvar:xs))
                   liftIO $ forkIO $ runReaderT (exp mvar) st

waitForChildren :: MVar [MVar ()] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
       putMVar children ms
       takeMVar m
       waitForChildren children

par :: Proc r a -> Proc r a -> Proc r a
par c1 c2 = 
  -- Run the expressions in the background, giving them the mvar to
  -- store their results in
  do children <- liftIO $ newMVar []
     local (\st -> (fst st, children)) $ forkChild $ wrapProc c1
     local (\st -> (fst st, children)) $ forkChild $ wrapProc c2
     --Ensure we switched to one of the new threads
     liftIO $ yield
     -- Wait for them to complete, then clear the children list, and
     -- return the childrens results
     liftIO $ waitForChildren children

f_return :: a -> Proc r a
f_return e = liftIO $ return e

f_yield = liftIO $ yield

test1 = (par (f_return 1) (f_return 2))
test2 = (par (do f_yield; (par (f_return 1) (f_return 2)))
             (f_return 3))

