import Control.Monad.Cont

data Action r = Res r
              | Susp (() -> Action r)
              | Par (Action r) (Action r)

type Proc r a = Cont (Action r) a

yield :: Proc r ()
yield = Cont (\k -> Susp k)

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> Par (c1 k) (c2 k))

processNext :: [Action r] -> [Action r]-> [r]
processNext [] [] = []
processNext [] (x:xs)= processAction x xs
processNext (x:xs) xs' = processAction x (xs++xs')

processAction :: Action r -> [Action r] -> [r]
processAction (Res res) xs = [res] ++ processNext xs []
processAction (Susp a) xs = processNext xs [(a ())]
processAction (Par a1 a2) xs = processAction a1 (a2:xs)

runProc :: Proc r r -> [r]
runProc c = processAction (runCont c (\i -> (Res i))) []

test1 = (par (return 1) (return 2))
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))

