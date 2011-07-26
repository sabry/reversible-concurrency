import Control.Monad.State
import Control.Monad.Cont


type Chan = [Int]

type Comp a = StateT Chan [] a

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

choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) (Cont c2) =
  Cont (\k -> StateT (\ch ->
    let b1 = runStateT (c1 k) ch
        b2 = runStateT (c1 k) ch
    in b1 ++ b2))

backtrack :: Proc r a
backtrack = Cont (\k -> StateT (\ch -> []))

processAction :: Action r -> [Comp (Action r)] -> Chan -> [r]
processAction (Res res) xs ch = [res] ++ processNextC xs [] ch
processAction (Susp a) xs ch = processNextC xs [(a ())] ch
processAction (Par a1 a2) xs ch = processComp a1 (a2:xs) ch

processNextC :: [Comp (Action r)] -> [Comp (Action r)] -> Chan -> [r]
processNextC [] [] ch = []
processNextC [] (x:xs) ch = processComp x xs ch
processNextC (x:xs) xs' ch = processComp x (xs++xs') ch

processComp :: Comp (Action r) -> [Comp (Action r)] -> Chan -> [r]
processComp s xs ch = do (a,s) <- (runStateT s ch)
                         processAction a xs s

runProc :: Proc r r -> [r]
runProc c = processComp (runCont c (\i -> return (Res i))) [] []

test5 = par
          (do x <- choose (return 1) (return 2)
              send x
              yield
              y <- recv
              if y == 0
                then backtrack
                else return y)
          (do a <- recv
              if a == 1
                then do send 0; return 0
                else do send 1; return 10)
