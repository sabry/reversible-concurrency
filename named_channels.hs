-- Not yet modified to use real concurrency
import Control.Monad.State
import Control.Monad.Cont

type Name = String

type Chan = [Int]

type NamedChan = (Name, Chan)

-- Now the state is a list of named channels, rather than just a single
-- channel.
type Comp a = StateT [NamedChan] [] a

data Action r = Res r
              | Susp (() -> Comp (Action r))
              | Par (Comp (Action r)) (Comp (Action r))

type Proc r a = Cont (Comp (Action r)) a

yield :: Proc r ()
yield = Cont (\k -> return (Susp k))

par :: Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> return (Par (c1 k) (c2 k)))

-- Of course, one might actually want a function that generates a name
-- in a production settings, but this is a start.
-- Note we can easily have multiple channels with the same name, as we
-- do no such error checking. The result of this is undefined on both
-- send and recv, so don't do it.
newchan :: String -> Proc r ()
newchan s = Cont (\k -> do nchans <- get
                           put $ (s, []) : nchans
                           k ())

-- First we get the state: a list of named channels. Take advantage of
-- the list monad to figure out which channel has the correct name, and
-- return the entire modified state, with the message on the correct
-- channel.
send :: String -> Int -> Proc r ()
send name s = Cont (\k -> do nchans <- get
                             put $ do nch <- nchans
                                      case nch of
                                           (n, ch) | n == name 
                                             -> return (n, (ch ++ [s]))
                                           x -> return x
                             k ())

-- Overdocumented! Perhaps better than underdocumented
--
-- From the list monad, we return a zipped list of lists of ints, and
-- named channels, which have been modified during receiving. The type
-- then is: [([Int], NamedChan)]. However, all these pairs except the
-- one with the named channel we want will have an empty list with it.
-- With a little functional magic, we can easily reconstruct the value
-- we're looking for, and the modified list of named channels. 
--
-- It might have been simpler to just use an Int in the zip list, and
-- return 0, using sum to merge them, but a list is more
-- extensible.
--
-- It's not the prettiest Haskell code ever written, and I have a
-- feeling.. it's a little hacky. Namely this zip list, it seems like a
-- hack to export the result from far inside the monad, and using the
-- list to keep the type checker happy.
recv :: String -> Proc r Int
recv name = Cont (\k -> do nchans <- get
                           let (xs, mnchans) = 
                                -- unzip the zip list, and get rid of
                                -- the extra nil lists.
                                (\(xs, ms) -> 
                                  ((foldl (++) [] xs), ms)) . unzip $
                                 -- For each named channel...
                                 do nch <- nchans
                                    let rcv n ch =
                                         case ch of 
                                              [] -> ([], (n, []))
                                              (s:ss) -> ([s], (n, ss))
                                        -- If the name is correct, recv
                                        -- the value, and modify the
                                        -- channel. Otherwise, leave it
                                        -- alone, and return an empty
                                        -- list (for type checking)
                                        in case nch of
                                                (n, ch) | (n == name)
                                                  -> return $ rcv n ch
                                                x -> return ([], x)
                               in case xs of
                                       -- No value returned in the zip
                                       -- list; channel was empty
                                       [] -> runCont 
                                              (do yield; recv name) 
                                              k 
                                       -- Otherwise, the only value
                                       -- there is the one we want.
                                       (x:_) -> do put mnchans; (k x))

choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) (Cont c2) =
  Cont (\k -> StateT (\ch ->
    let b1 = runStateT (c1 k) ch
        b2 = runStateT (c2 k) ch
    in b1 ++ b2))

backtrack :: Proc r a
backtrack = Cont (\k -> StateT (\ch -> []))

runProc :: Proc r r -> [[r]]
runProc c =  evalStateT (sched [runCont c (\i -> return (Res i))]) []
  where sched [] = return []
        sched (x : xs) = 
          do a <- x
             case a of 
                  (Res r) -> do rs <- sched xs; return (r : rs)
                  (Susp c) -> sched (xs ++ [c ()])
                  (Par c1 c2) -> sched (xs ++ [c1,c2])
test5 = par
          (do x <- choose (return 1) (return 2)
              newchan "a"
              send "a" x
              yield
              y <- recv "b"
              if y == 0
                then backtrack
                else return y)
          (do a <- recv "a"
              newchan "b"
              if a == 1
                then do send "b" 0; return 0
                else do send "b" 1; return 10)
-- This should hang forever, since they're not using the correct
-- channels to communicate with each other.
test6 = par
          (do newchan "a"
              send "a" 1
              yield
              y <- recv "c"
              return y)
          (do x <- recv "c"
              return x)

