{-# OPTIONS_GHC -XFlexibleInstances#-}
import Control.Monad.State

class Monad m => Backtrack m where
  backtrack :: m a
  choose    :: [a] -> m a

-- So really, this backtracking monad is the list monad. Or it's using
-- the list monad to do all the work, anyway.
instance Backtrack (StateT s []) where
  backtrack = StateT (\s -> [])
  choose xs = StateT (\s -> map (\a -> (a,s)) xs)

findNumsS:: StateT Int [] (Int,Int,Int)
findNumsS = 
  do a <- choose [2..8]
     add a
     b <- choose [3..7]
     add b
     c <- choose [5..9]
     add c
     if a*b+c == 55
       then return (a,b,c)
       else backtrack
  where add a = do s <- get; put (s+a)
