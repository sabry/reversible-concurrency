-- Defines a logical time system.
module Reversible.LogicalTime (
  Time,
  baseTime,
  incTime
  ) where

  type Time = Int
  
  baseTime :: Time
  baseTime = -1

  incTime :: Time -> Time
  incTime = (+1)
