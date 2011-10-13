-- Defines a logical time system.
module Reversible.LogicalTime (
  Time,
  baseTime,
  incTime
  ) where

  data Time = Zero 
            | T Int
            | Max deriving (Ord, Eq, Show, Read)
  
  baseTime :: Time
  baseTime = Zero

  incTime :: Time -> Time
  incTime Zero = T 0
  incTime (T t) = T (t+1)

  maxTime :: Time
  maxTime = Max
