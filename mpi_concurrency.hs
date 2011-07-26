import Control.Monad.State
import Control.Monad.Cont


type Chan = [Int]

type Comp a = State Chan a

data Action r = Res r
              | Susp (() -> Comp (Action r))
              | Par (Comp (Action r)) (Comp (Action r))

type Proc r a = Cont (Comp (Action r)) a

yield :: Proc r ()
yield = Cont (\k -> return (Susp k))

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> return (Par (c1 k) (c2 k)))

send :: Int -> Proc r ()
send s = Cont (\k -> do ch <- get
                        put (ch ++ [s])
                        k ())

recv :: Proc r Int
recv = Cont (\k -> do ch <- get
                      case ch of
                        [] -> runCont (do yield; recv) k
                        (s:ss) -> do put ss; k s)


processAction :: Action r -> [Comp (Action r)] -> Chan -> [r]
processAction (Res res) xs ch = [res] ++ processNextC xs [] ch
processAction (Susp a) xs ch = processNextC xs [(a ())] ch
processAction (Par a1 a2) xs ch = processComp a1 (a2:xs) ch

processNextC :: [Comp (Action r)] -> [Comp (Action r)] -> Chan -> [r]
processNextC [] [] ch = []
processNextC [] (x:xs) ch = processComp x xs ch
processNextC (x:xs) xs' ch = processComp x (xs++xs') ch

processComp :: Comp (Action r) -> [Comp (Action r)] -> Chan -> [r]
processComp s xs ch = (case (runState s ch) of
                     (a,s) -> processAction a xs s)

runProc :: Proc r r -> [r]
runProc c = processComp (runCont c (\i -> return (Res i))) [] []

test3 = (par
         (do send 2
             return 0)
         (do x <- recv
             return (x+1)))
test4 = (foldr1 par
                  [do x <- recv; send (x+1); return 0,
                   return 1,
                   do send 10; yield; y <- recv; return y])
