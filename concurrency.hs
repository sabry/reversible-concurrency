import Control.Monad.Cont
import Control.Concurrent hiding (yield)
import qualified Control.Concurrent as CC (yield)

data Action r = Res r
              | Susp (() -> Action r)
              | Par (Action r) (Action r)

type Proc r a = Cont (Action r) a

yield :: Proc r ()
yield = Cont (\k -> Susp k)

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> Par (c1 k) (c2 k))

wrapProc = undefined
forkChild = undefined 

runProc :: Proc r r -> IO [r]
runProc p = sched $ [runCont p (\r -> return (Res r))] where
  sched [] = return []
  sched (x:xs) = 
    case x of 
         (Res r) -> do ls <- (sched xs); return (r:ls)
         (Susp k) -> do CC.yield; (sched (xs ++ [k ()]))
         (Par k1 k2) -> do
            children <- newMVar []
            results <- newMVar []
            forkChild results children $ wrapProc (sched [k1])
            forkChild results children $ wrapProc (sched [k2])
            CC.yield
            childs <- takeMVar children
            return $ map takeMVar childs
            res <- takeMVar results
            ls <- sched xs
            return (res ++ ls)

test1 = (par (return 1) (return 2))
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))

