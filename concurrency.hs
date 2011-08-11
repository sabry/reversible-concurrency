import Control.Concurrent hiding (yield)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield)
import Debug.Trace

data Action r = Res r
              | Susp (() -> Action r) 
              | Par (Action r) (Action r)

-- Sample runs. Multiple runs to ensure the concurrency is actually
-- causing some amount of nondeterminism in the order of the answers
-- 
-- *Main> runProc test1
-- [2,1]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [2,1,3]
-- *Main> runProc test2
-- [3,2,1]

-- I use the continuation monad here, not for concurrency, but for to
-- allow me to wrap final answers in a Res constructor. This further
-- allows me to do other things more easily, and hopefully allows for
-- easier abstraction later. 
type Proc r a = Cont (Action r) a

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> Par (c1 k) (c2 k))

yield :: Proc r ()
yield = Cont (\k -> (Susp k))

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

test1 = (par (return 1) (return 2))
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))
