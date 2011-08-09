import Control.Monad.Cont
import Control.Concurency hiding (yield)

data Action r = Res r
              | Susp (() -> Action r)
              | Par (Action r) (Action r)

type Proc r a = Action (IO r) -- Cont (Action r) a

yield :: IO () -- Proc r ()
yield = Susp (\_ -> Control,Concurrency.yield)

--par :: Proc r a -> Proc r a -> Proc r a
--par (Cont c1) (Cont c2) = Cont (\k -> Par (c1 k) (c2 k))
par :: IO r -> IO r -> IO r
par c1 c2 = Par c1 c2

runProc :: Proc r r -> [r]
runProc c = sched [(runCont c (\i -> (Res i)))] where
  sched [] = []
  sched (x:xs) = 
    case x of 
         Res r -> [r] ++ (sched xs)
         Susp c -> sched (xs ++ [c ()])
         Par c1 c2 -> sched (xs ++ [c1,c2])

test1 = (par (return 1) (return 2))
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))

