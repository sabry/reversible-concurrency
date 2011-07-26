{-# OPTIONS_GHC -XFlexibleInstances#-}
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Cont.Class

type Chan = [Int]

type Comp a = State Chan a

data Action r = Res r
              | Par (Comp (Action r)) (Comp (Action r))
              | Susp (() -> Comp (Action r))

type Proc r a = Cont (Comp (Action r)) a

--choose :: Proc r a -> Proc r a -> Proc r a
--choose (Cont c1) (Cont c2) = 
--  Cont (\k -> StateT (\ch ->
--    let b1 = runStateT (c1 k) ch
--        b2 = runStateT (c2 k) ch
--    in b1 ++ b2))
--
--backtrack :: Proc r a
--backtrack = Cont (\k -> StateT (\ch -> []))

send :: Int -> Proc r ()
send s = Cont (\k -> do ch <- get
                        put (ch ++ [s])
                        k ())
recv :: Proc r Int
recv = Cont (\k -> do ch <- get
                      case ch of 
                        [] -> runCont (do yield; recv) k
                        (s:ss) -> do put ss; k s)
yield :: Proc r ()
yield = Cont (\k -> return (Susp k))

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> return (Par (c1 k) (c2 k)))

--runProc :: Proc r a -> [r]
test1 = (par (return 1) (return 2))

--the_example = par 
--                (do x <- choose (return 1) (return 2)
--                    send x
--                    yield
--                    y <- recv
--                    if y == 0
--                       then backtrack
--                       else return y)
--                (do a <- recv
--                    if a == 1
--                       then do send 0; return 0
--                       else do send 1; return 10)
