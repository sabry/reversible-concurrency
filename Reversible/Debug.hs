{-# LANGUAGE CPP #-}
module Reversible.Debug (
  traceM,
  traceG,
  trace
  ) where

  import qualified Debug.Trace as D
  
#ifndef debug_level
  debug_level = 0
#endif

#ifndef DEBUG
  debug = False
#else
  debug = True
#endif 

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
