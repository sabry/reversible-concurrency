import Control.Concurrent hiding (yield)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield)

data Action r = Res r
              | Susp (() -> Action r) 
              | Par (Action r) (Action r)

type Proc r a = Cont (Action r) a

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> return $ Par (c1 k) (c2 k))

yield :: Proc r ()
yield = Cont (\k -> return (Susp k))

wrapProc :: Show r => IO [r] -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  xs <- p
  modifyMVar_ mvar (\ls -> return $ xs ++ ls)
  putMVar block ()

forkChild :: MVar [r] -> MVar [MVar ()] -> (MVar () -> MVar [r] -> IO ()) -> IO ThreadId
forkChild results children io = do
  block <- newEmptyMVar 
  modifyMVar_ children (\xs -> return (block:xs))
  forkIO $ io block results

runProc :: Proc r r -> IO [r]
runProc p = sched $ [runCont p (\r -> return (Res r))] where
  sched [] = return []
  sched (x:xs) = 
    case x of 
         Res r -> (return r) ++ (sched xs)
         Susp k -> do CC.yield; (sched (xs ++ [k ()]))
         Par k1 k2 -> do
          children <- newMVar []
          results <- newMVar []
          forkChild results children $ wrapProc (sched [k1])
          forkChild results children $ wrapProc (sched [k2])
          childs <- takeMVar children
          map takeMVar childs
          res <- takeMVar results
          return (res ++ (sched xs))

test1 = (par (return 1) (return 2))
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))
{-
wrapProc :: Show r => Proc r -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  liftIO $ modifyMVar_ mvar (\ls -> return $ xs ++ ls)
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
  results <- liftIO $ newMVar []
  liftIO $ forkChild results children $ wrapProc p1
  liftIO $ forkChild results children $ wrapProc p2
  yield
  xs <- liftIO $ takeMVar children
  return $ map (return . takeMVar) xs
  foldr (\r -> \xs -> (return r):xs) [] takeMVar results

yield :: Proc ()
yield = return CC.yield

runProc :: Proc r -> IO [r]
runProc = undefined 

f_return :: a -> [IO a]
f_return x = return . return

test1 = (par (f_return 1) (f_return 2))
test2 = (par (do yield; (par (f_return 1) (f_return 2)))
             (f_return 3))
-}
