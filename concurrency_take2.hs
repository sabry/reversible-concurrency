import Control.Parallel

--yield :: Proc r -- Proc r ()
--yield = Susp (\_ -> Res Control.Concurrency.yield)

-- Wrap an expression so it can be run in a background thread, and it's
-- return value accessed via a given MVar.
wrapIO :: IO r -> (MVar r -> IO ())
wrapIO c = (\mvar -> do r <- c
                        putMVar mvar r)

-- Take an expression to be run in the background, whose return value is
-- type r. Then, in a seperate thread, run the expression, putting the
-- result in an MVar. Return the MVar to the caller so they have access
-- to the result when the thread completes.
bg :: IO r -> IO (MVar r)
bg a = do t <- newEmptyMVar
          forkIO (wrapIO a t)
          return t  

--par :: Proc r a -> Proc r a -> Proc r a
--par (Cont c1) (Cont c2) = Cont (\k -> Par (c1 k) (c2 k))
--par :: Proc r -> Proc r -> Proc r 
--par c1 c2 = Par c1 c2

-- Take two expressions, and run them in the background, returning their
-- results in a list. These expression, obviously, must be of the same
-- type, as lists must be homogeneous
par :: IO [r] -> IO [r] -> IO [r]
par c1 c2 = 
  -- Run the expressions in the background
  do m1 <- bg c1
     m2 <- bg c2 
     --Ensure we switched to one of the new threads
     yield
     -- Wait for their results
     r1 <- takeMVar m1
     r2 <- takeMVar m2
     -- And combine
     return $ r1 ++ r2


-- runProc :: Proc r -> IO [r]
-- runProc c = sched [c] where
--   sched [] = return []
--   sched (x:xs) = 
--     case x of 
--          Res r -> do t <- (sched xs)
--                     return $ [r] ++ t
--          Susp c -> sched (xs ++ [c ()])
--          Par c1 c2 -> sched (xs ++ [c1,c2])
runProc = runListT 

-- We'd really like to be able to just write return 1 etc, instead of
-- return [1], but for now...
test1 = (par (return [1]) (return [2]))
test2 = (par (do yield; (par (return [1]) (return [2])))
             (return [3]))

