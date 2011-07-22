class Monad m => Backtrack m where
  backtrack :: m a
  choose    :: [a] -> m a

-- So really, this backtracking monad is the list monad. Or it's using
-- the list monad to do all the work, anyway.
instance Backtrack [] where
  backtrack = []
  choose xs = xs

findNums :: Backtrack m => m (Int,Int,Int)
findNums = 
  do a <- choose [2..8]
     b <- choose [3..7]
     c <- choose [5..9]
     if a*b+c == 55
       then return (a,b,c)
       else backtrack
