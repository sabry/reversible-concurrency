import Control.Monad.Reader
import Control.Concurrent.Chan
import Control.Concurrent hiding (yield)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield)
import Debug.Trace

type Comp a = Reader (Chan Int) a

data Action r = Res r
              | Susp (() -> Comp (Action r))
              | Par (Comp (Action r)) (Comp (Action r))

type Proc r a = Cont (Comp (Action r)) a

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> Par (c1 k) (c2 k))

yield :: Proc r ()
yield = Cont (\k -> (Susp k))

send :: Int -> Proc r ()
send s = Cont (\k -> do ch <- ask
                        put (ch ++ [s])
                        k ())

recv :: Proc r Int
recv = Cont (\k -> do ch <- get
                      case ch of
                        [] -> runCont (do yield; recv) k
                        (s:ss) -> do put ss; k s)

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

runPar :: Show r => Action r -> Action r -> IO [r]
runPar k1 k2 = do
  children <- newMVar []
  results <- newMVar []
  forkChild results children $ wrapProc (sched k1)
  forkChild results children $ wrapProc (sched k2)
  -- Block until all children have returned
  waitOnChildren children
  res <- takeMVar results
  return res

sched :: Show r => Action r -> IO [r]
sched x = 
  case x of 
       (Res r) -> return [r]
       (Susp k) -> do CC.yield; sched $ k ()
       (Par k1 k2) -> runPar k1 k2

runProc :: Show r => Proc r r -> IO [r]
runProc p = sched $ runCont p (\r -> (Res r))


test3 = (par
         (do send 2
             return 0)
         (do x <- recv
             return (x+1)))
test4 = (foldr1 par
                  [do x <- recv; send (x+1); return 0,
                   return 1,
                   do send 10; yield; y <- recv; return y])
