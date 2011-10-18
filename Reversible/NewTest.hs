{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE BangPatterns #-}
-- Test module. Exports test several test suites, each testing various
-- levels of functionality.
module Reversible.NewTest (markSub0, mark0) where

import Reversible.NewBase
import Control.Monad

runTest :: Show a => Proc a a -> IO ()
runTest p = print =<< (runProc p)

runTests :: Show a => [Proc a a] -> IO ()
runTests ls = foldM_ (\ _ p  -> runTest p) () ls

-- Mark sub0 tests basic parallel and communication operations
--
markSub0 :: IO ()
markSub0 = runTests [test1, test2, test3, test4]

-- Mark 0 tests the ability for threads to backtrack on their own,
-- without any communication in a stable region (i.e. between choose
-- and backtrack)
--
mark0 = runTests [testm0_1, testm0_2]

-- Mark 1 tests the ability for threads to backtrack with a single
-- choose point globally.
--
--mark1 = runTests [testm1_1, testm1_2]

-- The following is no longer the case for most of these tests, as we
-- have synchronous communication.
-- <s>All the tests have data returned in an unexpected order. This make
-- sense in some of the tests, as the parallelism is non-deterministic as
-- to which one will finish first. The tests with explicit yields,
-- however, are slightly more interesting. While in a concurrent
-- environment, without true parallelism, the yield would force one
-- thread to wait, in a true parallel environment, with enough cores for
-- all threads, the yield is useless.</s> 
--
-- Most of these race conditions are addressed, or being
-- addressed.
-- <s>Unfortunately, this can cause a race condition in some of the
-- assumptions we make, for instance, in test5. We assume that, having
-- yielded, the second thread will receive the value we sent before we
-- manage to receive. This might not be the case, however, if the first
-- thread is running faster than the second thread.</s>
--
-- Expected: [1,2]
-- Result: [1,2]
test1 :: Proc Int Int
test1 = (par (return 1) (return 2))

-- Expected: [3,1,2]
-- Results: [3,1,2]
test2 :: Proc Int Int
test2 = (par (do yield; (par (return 1) (return 2)))
             (return 3))

-- Expected: [0,3]
-- Results: [0,3]
test3 :: Proc Int Int
test3 = do ch <- newChan
           (par
              (do send ch 2
                  return 0)
              (do x <- recv ch
                  return (x+1)))

-- Expected: [1,0,11]
-- Results: [1,0,11]
test4 :: Proc Int Int
test4 = do ch <- newChan
           ch2 <- newChan
           (foldr1 par
                 [do x <- recv ch; send ch2 (x+1); return 0,
                  return 1,
                  do send ch 10; yield; y <- recv ch2; return y])


--Expected result: 4
--Reult: 4
testm0_1 :: Proc Int Int
testm0_1 = do 
  x <- choose (return 1) (return 2)
  case x of 
    2 -> do 
      y <- choose (return 3) (return 4)
      case y of
        3 -> backtrack
        4 -> return y
    1 -> backtrack 
  
-- Expected result: [4,2]
-- Result: [4,2]
testm0_2 :: Proc Int Int
testm0_2 = par
  (do x <- choose (return 1) (return 2)
      case x of 
        2 -> do
          y <- choose (return 3) (return 4)
          case y of
            3 -> backtrack
            4 -> return y
        1 -> backtrack) 
  (do x <- choose (return 3) (return 4)
      case x of 
        4 -> do 
          y <- choose (return 1) (return 2)
          case y of
            1 -> backtrack
            2 -> return y
        3 -> backtrack)

-- Expected: [10, 1]
-- Results: [1, 10]
--
-- <s>There's a race condition caused by the channel, it seems. We can
-- occasionally pull off the x we sent before the other thread has had
-- a chance to receive it, despite the yield. It's interesting that
-- we're both able to pull the value off the channel simultaneously,
-- however.
--
-- The only solution to this issue I can see is named channels.
--
-- What's more curious is the result we normally get, being [1,1,10]. I
-- haven't figured out why we get two 1's. Maybe it's due to both
-- duplicated continutations and true parallelism.</s>
--
-- XXX: I can't figure out how to get the Cont monad's return to do what
-- I want, as I have to give a function of type r -> IO r, but I need a
-- function r -> Proc r a. So... we return with endProcess, and return
-- from there after checking for backtracking messages and such
-- testm1_1 :: Proc Int Int
-- testm1_1 = do ch1 <- newChan
--               ch2 <- newChan
--               par
--                 (do x <- choose (return 1) (return 2)
--                     send ch1 x
--                     yield
--                     y <- recv ch2
--                     if y == 0
--                       then backtrack
--                       else endChoice y)
--                 (do a <- recv ch1
--                     if a == 1
--                       then do send ch2 0; endProcess 0
--                       else do send ch2 1; endProcess 10)
-- 
-- testm1_2 :: Proc Int Int
-- testm1_2 = do
--   ch1 <- newChan
--   ch2 <- newChan
--   ch3 <- newChan
--   ch4 <- newChan
--   (foldr1 par
--    [do x <- choose (return 1) (return 2)
--        send ch1 x
--        send ch3 x
--        y <- recv ch2
--        z <- recv ch4
--        if z == y
--         then endProcess y
--         else backtrack,
--    do x <- recv ch1
--       if x == 1
--         then do send 2 ch2; endProcess x
--         else do send 0 ch2; endProcess x,
--    do x <- recv ch3
--       if x == 2
--         then do send 0 ch4; endProcess x
--         else do send 5 ch4; endProcess x])
