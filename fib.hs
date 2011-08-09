import Control.Concurrent
import Control.Parallel


fib :: Integer -> Integer
fib n = case n of
             n | n < 2 -> n
             n -> (fib $ n-1) + (fib $ n-2) 

forkReturn :: (MVar a -> IO ()) -> IO (MVar a)
forkReturn a = do t <- newEmptyMVar
                  forkIO (a t)
                  return t  

fibTH :: Integer -> MVar Integer -> IO ()
fibTH n mvar =
  case n of
       n | n < 2 -> putMVar mvar n 
       -- First, spawn off two threads to do the work
       n -> do r1 <- forkReturn (fibTH (n-1))
               r2 <- forkReturn (fibTH (n-2))
               -- Then, wait for their results
               yield
               x <- takeMVar r1
               y <- takeMVar r2
               putMVar mvar (x + y)

fibT :: Integer -> IO (Integer)
fibT n = do x <- forkReturn (fibTH n); takeMVar x

fibP :: Integer -> Integer
fibP n | n < 2 = n
       | otherwise = par n1 (pseq n2 (n1 + n2))
                     where n1 = fibP (n-1)
                           n2 = fibP (n-2)
