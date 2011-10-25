module Reversible.Debug (
  traceM,
  traceG,
  trace
  ) where

  import qualified Debug.Trace as D
  
-- #ifndef DEBUG_LEVEL
-- #define DEBUG_LEVEL 0
-- #else
-- #define DEBUG True
-- #endif
-- 
-- #ifndef DEBUG
-- #define DEBUG False
-- #endif 

  debug = True
  debug_level = 2

  trace_ :: a -> Int -> String -> a
  trace_ r l msg = if (and [debug, l <= debug_level])
                      then D.trace msg r
                      else r

  traceM :: Monad m => Int -> String -> m ()
  traceM l msg = trace_ (return ()) l msg

  traceG :: Int -> String -> Bool
  traceG l msg = trace_ (False) l msg

  trace :: Int -> String -> a -> a
  trace l msg r = trace_ r l msg
