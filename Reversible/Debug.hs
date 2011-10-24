module Reversible.Debug (
  traceM,
  traceG,
  trace
  ) where

  import Debug.Trace as D
  
#ifndef DEBUG_LEVEL
#define DEBUG_LEVEL 0
#endif

#ifndef DEBUG
#define DEBUG False
#endif 

  trace_ :: a -> Int -> String -> a
  trace_ r l msg = if (and [DEBUG, l < DEBUG_LEVEL])
                      then r
                      else D.trace msg r

  traceM :: Monad m => Int -> String -> m ()
  traceM l msg = trace_ (return ()) l msg

  traceG :: Int -> String -> Bool
  traceG l msg = trace_ (False) l msg

  trace :: Int -> String -> a -> a
  trace l msg r = trace_ r l msg
