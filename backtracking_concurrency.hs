-- Note:
--   TODO tags indicate (shockingly) something that needs to be done
--   XXX tags indicate comments, notes to self, observations, or
--   thoughts that are not tasks, but things that need to be considered
--   or things to be aware of.
--
-- I've taken up not deleting TODO and XXX tags, as they might be useful as
-- notes if/when I have to write about this. Instead, completed ones are
-- surrounded by HTML strike tags (<s></s>) 
--
-- To run with true parallelism, use: ghci +RTS -N -RTS. Of course, you
-- need a machine with at least 2 cores to take advantage of this.

-- <s>However, due to some race conditions which exist right now, it's
-- actually preferablly to test this without parallelism, instead
-- allowing ghc to use it's internal scheduler for concurency.</s>
--
import Control.Concurrent hiding (yield,newChan)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield,newChan)
import Control.Monad.State
import Debug.Trace
import qualified Data.Map as M 
import Data.Map (Map, adjust, insert, (!))

-- Honestly, there's no Data.Stack?
-- Quick stack implementation
type Stack a = [a]
empty :: Stack a
empty = []
isEmpty :: Stack a -> Bool
isEmpty = null
push :: a -> Stack a -> Stack a
push = (:)
top :: Stack a -> a
top = head
pop :: Stack a -> (a,Stack a)
pop (s:ss) = (s,ss)

-- end stack implementation

type Pid = Int -- ID of a process
type Msgid = Int -- ID for a message on a channel.
type Chid = Int -- Take a wild guess.

-- Speculative tag lets us know if something is involved in a
-- speculative computation or not, i.e., if it appears between a choose
-- and backtrack.
type SpecTag = Bool
type Ch = (Chan Msg, Chan Msg, SpecTag, Msgid)

-- KType indicates the type of a continuation, which is needed to figure
-- out what to do when backtracking
data KType = Sent Chid Msgid
           | Choose

-- Messages that can be sent over the channels.
-- 1) A send message, the normal kind of message where you send data.
-- 2) Unsend message, telling a thread to pretend it never received a
--    a message on this channel. 
-- 3) Backtrack message, telling a thread to pop a continuation.
-- 4) Continue, tells the thread to run the current continuation.
-- 5) Complete tells the thread whichever thread caused the speculation
--    has returned successfully, so it can stop listening for a
--    backtrack message.
-- 6) Acknowledge, which is used for by a receiving thread to
--    the message, and let the sending thread know if it's speculative.
--    Send and acknowledge message ID's should be the same.
-- 7) Unreceive might not be totally necessary, if the sending threads
--    can keep track of sending to speculative threads. We'll see.
data Msg = Send SpecTag Msgid Int
         | Acknowledge SpecTag Msgid
         | Unsend Msgid
         | Backtrack
         | Continue

-- Each thread needs to keep track of various information, including if
-- it's speculative, a stack of continuations, and a list of channels it
-- has received speculative information on, and a list it has sent
-- speculative information on.
--
-- <s>XXX: We need to track channels we sent on and received on
-- seperately. channels we sent on, we need to listen for backtrack
-- messages, ... on second thought, do we? What if we just keep the
-- channels, and then listen on the receiving end. If we get a backtrack
-- message, we backtrack, and keep listening on that channel. If we get
-- an unsend/unrecv on it, we process it. Why do we care if we sent or
-- received on it?</s>
type ContStack r = Stack (() -> r, KType)
type ThreadState r = (SpecTag, ContStack r, [Chid])
type ThreadMap r = Map Pid (ThreadState r) 

-- <s>XXX: This [Ch] should probably be more like Map Chid Ch. We
-- need this to keep track of message id's for each channel.</s> 
type GlobalState r = (Pid, Chid, ThreadMap r, Map Chid Ch)
type Comp r a = StateT (GlobalState r) IO a

-- Continuations used for backtracking through a stack of continuations.
-- XXX: I'm a little worried about the mutual recursion in these
-- types... I'm pretty sure I can't do that, but we'll wait until I ask
-- the type checker. or until I make a decision about the next XXX
--
-- XXX: Turns out I was right about that cycle. :/
--
-- XXX: Maybe ThreadMap should have a stack of Cont ?? (). That would
-- simplify the interface too.
type Proc r a = Cont (Comp r r) a

-- Interface methods for tuples
fst3 :: (a, b, c) -> a
fst3 (e,_,_) = e

snd3 :: (a, b, c) -> b
snd3 (_,e,_) = e

thrd3 :: (a ,b, c) -> c
thrd3 (_,_,e) = e

fst4 :: (a, b, c, d) -> a
fst4 (e,_,_,_) = e

snd4 :: (a, b, c, d) -> b
snd4 (_,e,_,_) = e

thrd4 :: (a ,b, c, d) -> c
thrd4 (_,_,e,_) = e

frth4 :: (a, b, c, d) -> d
frth4 (_,_,_,e) = e

-- end tuples

--(!) :: Ord r => r -> Map r a -> a
--(!) = flip (M.!)

-- Some interface methods for the thread state, and global state.
-- <s>TODO: Lots of interfaces methods are missing. Most of these are
-- broken now.</s>
getPid :: Comp r Pid
getPid = fmap fst4 get

getChid :: Comp r Chid 
getChid = fmap snd4 get

incChid :: Comp r Chid
incChid = do
  (a, b, c, d) <- get
  let chid = b+1
  put (a, chid, c, d)
  return chid

getThreadMap :: Comp r (ThreadMap r)
getThreadMap = fmap thrd4 get 

getChanMap :: Comp r (Map Chid Ch)
getChanMap = fmap frth4 get 

putChanMap :: Map Chid Ch -> Comp r ()
putChanMap d = do
  (a, b, c, _) <- get
  put (a, b, c, d)

-- Get the message id for a channel.
getMsgid :: Chid -> Comp r Msgid
getMsgid chid = fmap frth4 $ _getCh chid

-- Increment the message id for the channel, and return the new id.
incMsgid :: Chid -> Comp r Msgid
incMsgid chid = do
  map <- getChanMap
  putChanMap $ adjust (\(a,b,c,d) -> (a,b,c,(1+d))) chid map
  getMsgid chid

-- Methods prefixed with _ should be treated 'private'. More convenient 
-- version are available that give a public interface.
-- 
-- Given a function from a Pid to something out of the ThreadState, 
-- calls it on this thread.
-- XXX: Not really sure how I did this, but I created an object-oriented
-- like model using the state monad...
_this :: (Pid -> Comp r a) -> Comp r a
_this f = do 
  pid <- getPid
  f pid

-- (a -> b) -> f a -> f b

-- Get the thread state, stored in the global ThreadMap, by it's Pid
_getThreadState :: Pid -> Comp r (ThreadState a)
_getThreadState pid = fmap (! pid) getThreadMap 

_putThreadState :: Pid -> ThreadState a -> Comp r ()
_putThreadState pid st = do
  (a,b,map,d) <- get
  put (a,b, adjust (\_ -> st) pid map, d)

-- Get an element out of the ThreadState triple.
_getThreadStatePos :: Pid -> ((a,b,c) -> d) -> Comp r d
_getThreadStatePos pid f = fmap f $ _getThreadState pid

_getSpecTag :: Pid -> Comp r SpecTag
_getSpecTag pid = fmap fst3 $ _getThreadState pid

_setSpecTag :: Bool -> Pid -> Comp r ()
_setSpecTag b pid = do
  (_, a, b) <- _getThreadState pid
  _putThreadState pid (b, a, b)

getSpecTag :: Comp r SpecTag
getSpecTag = _this _getSpecTag 

setSpecTag :: Bool -> Comp r ()
setSpecTag b = _this $ _setSpecTag 

_getContStack :: Pid -> Comp r (ContStack r)
_getContStack pid = fmap snd3 $ _getThreadState pid

_putContStack :: ContStack r -> Pid -> Comp r ()
_putContStack st pid = do
  (a, _, b) <- _getThreadState pid 
  _putThreadState pid (a, st, b)

getContStack :: Comp r (ContStack r)
getContStack = _this _getContStack

putContStack :: ContStack r -> Comp r ()
putContStack st = _this $ _putContStack st

_getSpecChans :: Pid -> Comp r [Chid]
_getSpecChans pid = fmap thrd3 $ _getThreadState pid

getSpecChans :: Comp r [Chid]
getSpecChans = _this _getSpecChans

-- Add a new full duplex channel to the map, creating a channel id for
-- it, and returning that id.
addCh :: (Chan Msg, Chan Msg, SpecTag, Msgid) -> Comp r Chid
addCh ch = do 
  map <- getChanMap
  chid <- incChid 
  let map' = insert chid ch map
  putChanMap map'
  chid

-- Build a send message, incrementing the message Id.
buildSendMsg :: Chid -> Int -> Comp r Msg
buildSendMsg chid s = do
  msgid  <- incMsgid chid
  spec <- getSpecTag 
  return $ Send spec msgid s

buildAckMsg :: Chid -> Comp r Msg
buildAckMsg chid = do
  msgid <- getMsgid chid
  spec <- getSpecTag 
  return $ Acknowledge spec msgid

buildUnsendMsg :: Msgid -> Comp r Msg
buildUnsendMsg msgid = return $ Unsend msgid

-- Get the Ch tuple out of the map
_getCh :: Chid -> Comp r Ch
_getCh ch = fmap (! ch) getChanMap

-- Get the send channel out of the Ch tuple out of the map
_getSendCh :: Chid -> Comp r (Chan Msg)
_getSendCh ch = fmap fst4 (_getCh ch)

_getRecvCh :: Chid -> Comp r (Chan Msg)
_getRecvCh ch = fmap snd4 (_getCh ch)

-- Tag a channel as speculative
setChTag :: Bool -> Chid -> Comp r ()
setChTag bool chid = do
  map <- getChanMap
  putChanMap $ adjust (\(a,b,_,c) -> (a,b,bool,c)) chid map

-- Send a message. On the send channel.
sendMsg :: Chid -> Msg -> Comp r ()
sendMsg chid msg = do
  ch <- _getSendCh chid
  liftIO $ writeChan ch msg

sendAck :: Chid -> Msg -> Comp r ()
sendAck chid msg = do
  ch <- _getRecvCh chid
  liftIO $ writeChan ch msg

recvMsg :: Chid -> Comp r Msg
recvMsg chid = do
  ch <- _getRecvCh chid
  liftIO $ readChan ch 

nbGetMsg :: Chid -> Maybe (Comp r Msg)
nbGetMsg chid = do
  ch <- _getRecvCh chid
  when (isEmptyChan ch) $ return Nothing
  liftIO $ Just $ readChan ch

-- Receive an acknowlegement, on the receive channel.
recvAck :: Chid -> Comp r Msg
recvAck chid = do
  ch <- _getRecvCh chid 
  liftIO $ readChan ch

-- Push a continuation onto the stack.
pushCont :: (() -> r) -> KType -> Comp r ()
pushCont k ktype = do
  stack <- getContStack
  putContStack $ push (k, ktype) stack

popCont :: Comp r (() -> r)
popCont = do
  st <- getContStack
  let (h, st) = pop st
  putContStack st
  return h

-- <s>TODO: Should runStateT with the same state, except the current
-- thread's pid incremented.</s>
runChild :: Comp r a -> Comp r (IO a)
runChild c = do 
  (pid, a, b, c) <- get
  return $ runStateT c (pid+1, a, b, c)

-- end of interface

par :: Show r => Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> do
  let b1 = runChild (c1 k) -- runStateT (c1 k) (ppid+1, map)
  let b2 = runChild (c2 k) --runStateT (c2 k) (ppid+2, map)
  liftIO $ runPar b1 b2)

-- Still useful in a concurrent environment, but useless in true
-- parallel environment. 
yield :: Proc r ()
yield = Cont (\k -> do 
  liftIO CC.yield; k ())

-- <s>TODO: Alter to create a full duplex channel, init message IDs, etc.
-- Should also mark as speculative if the current thread is.</s>
newChan :: Proc r (Chid)
newChan = Cont (\k -> do 
  sch <- liftIO CC.newChan
  rch <- liftIO CC.newChan
  chid <- addCh (sch, rch, False, 0)
  k chid)

-- XXX: I think callCC will work this way, but it might be interesting
-- to look at how stabilizers could play a role here.
send :: Chid -> Int -> Proc r ()
send ch s = callCC $ \cc -> Cont (\k -> do 
  -- <s>TODO: Lots of undefined methods. At this point, they're pretty much
  -- methods, given the way I'm using them is rather object oriented.
  -- TODO: Double check that these are the right data structure, i.e. the
  -- order of variables.</s>
  msg@(Send spec_s msgid_s p) <- buildSendMsg ch s
  sendMsg ch msg
  (Acknowledge spec_a msgid_a) <- recvAck ch
  --liftIO $ putStrLn ("Pid: " ++ (show pid) ++ " Sending: " ++ (show s))
  --liftIO $ writeChan ch $ msg
  when (spec_s `or` spec_a) $ setChTag True ch
  -- XXX: Sanity crash
  when (msgid_s /= msgid_a) $ undefined
  pushCont (\_ -> runProc (do send ch s; cc ())) $ Sent ch msgid_s
  k ());
  
-- <s>TODO: Same as send.</s>
recv :: Ch -> Proc r Int
recv ch = callCC $ \cc -> Cont (\k -> do 
  (Send spec_s msgid_s p) <- recvMsg ch
  ack@(Acknowledge spec_a msgid_a) <- buildAckMsg ch 
  sendAck ack ch
  when (spec_s `or` spec_a) setChTag True ch
  -- XXX: Sanity crash
  when (msgid_s /= msgid_a) $ undefined
  pushCont (\_ -> runProc $  do r <- recv ch; cc r) $ Sent ch msgid_a
  k p)

choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) k2@(Cont c2) = callCC $ \cc -> Cont (\k -> do 
  -- XXX: This seems a little naive. We store the next choice, and
  -- always take the first one first. There feels like a little room for
  -- abstraction here.
  -- Unexpected behavior if we try to backtrack after the second choice.
  pushCont (\_ -> runProc $ do r <- c2 k; cc r) $ Choose
  setSpecTag True
  c1 k)
--  st <- get
--  b1 <- liftIO $ runStateT (c1 k) st
--  liftIO $ return b1)

-- <s>TODO: Implement</s>
backToChoice :: Proc r (() -> r)
backToChoice = do
  (k, ktype) <- popCont
  case ktype of
    -- <s>TODO: Ensure var order of contructors</s>
    -- <s>TODO: More helpers to build</s>
    Sent chid msgid -> do 
      msg <- buildUnsendMsg msgid
      sendMsg chid msg
    Choose -> return k
-- backToChoice = do
--   pop a continuation.
--   case:
--      if send then sendUnsend; backToChoice
--      if recv then sendUnrecv; backToChoice
--      if choose then return 
  
-- When we reach a backtrack, we should start popping continuations, and
-- sending unsend/unrecv messages accordingly. At the choose statement,
-- it should send a Continue message
--
backtrack :: Proc r a
backtrack = Cont (\_ -> do
  (Cont c) <- backToChoice
  c ())
  --pid <- getPid
  --liftIO $ putStrLn ("Pid: " ++ (show pid) ++ " backtracking..")
  --liftIO $ return [])
  
-- Pop continutations until we find the one with the message id we're
-- looking for.
backToMsgid :: Msgid -> Proc r (() -> r)
backToMsgid msgid = do
  (c, ktype) <- popCont
  case ktype of
    Send _ msgid_ | msgid == msgid_ -> return c
    _ -> backToMsgid msgid

-- Wait on a particular channel for more Unsend or a Continue
waitForMsg :: Chid -> Proc r () -> Proc r ()
waitForMsg chid c = do
  msg <- recvMsg chid 
  case msg of
    Continue -> c ()
    Unsend msgid  -> do
      c <- backToMsgid msgid
      waitForMsg chid c

-- Listen on all our speculative channels for either Continue or Unsend
-- messages.
listenForMsg :: [Chid] -> Proc r ()
listenForMsg [] = return ()
listenForMsg (x:xs) = do
  msg <- nbGetMsg x
  case msg of
    Just msg ->
      case msg of
        Continue -> setChTag False x
        Unsend msgid -> do
          c <- backToMsgid msgid
          waitForMsg x c
    Nothing -> listenForMsg xs

endProcess :: r -> Proc r a
endProcess r = do
  xs <- getSpecChans
  when (null xs) $ return r
  (Cont c1) <- listenForMsg xs
  c1 ()
  endProcess r
  -- for each chid in our list of speculative channels:
  --   nonblocking check for a message:
  --   case:
  --     continue: remove chid from speculative list
  --     unsend: pop a continuation until we find a send message with
  --       the same msg id
  --     unrecv: same as unsend, but looking at recv ktypes.
  -- no chid's: return r     

-- XXX: This needs to be r -> IO r. But, we also need Proc r a in order to
-- access the state/continutations and such.
-- Could we make it r -> Proc r a and just recursively call runProc,
-- giving it a base case when we receive a Complete?
-- waitForMessage :: r -> Proc r a
-- waitForMessage res = Cont (\k -> do
--   chids <- getSpecChans
--   if null chids
--     then return res
--     else map completeOrBacktrack chids
--          waitForMessage res)

-- <s>TODO: Change so we only return a single value. When run in parallel,
-- the second task should return the value, while the first can just
-- print it out. or do whatever. I don't really care.</s>
-- XXX: I wonder if I can now make use of the built in par. The only
-- concern there is I won't be guaranteed to have them run in parallel.
wrapProc :: Show r => IO [r] -> MVar () -> MVar [r] -> IO ()
wrapProc p block mvar = do
  xs <- p
--  putStrLn ("Returning: " ++ (show xs))
  modifyMVar_ mvar (\ls -> return $ xs ++ ls)
  putMVar block ()

wrapProcPrint :: Show r => IO r -> IO ()
wrapProcPrint p = do
  r <- p
  putStrLn r

wrapProcRes :: IO r -> MVar r -> IO ()
wrapProcRes p res = do
  r <- p
  putMVar res r

forkChild :: MVar [r] -> MVar [MVar ()] -> 
            (MVar () -> MVar [r] -> IO ()) -> IO ThreadId
forkChild results children io = do
  block <- newEmptyMVar 
  modifyMVar_ children (\xs -> return $ block:xs)
  forkIO $ io block results

waitOnChildren :: MVar [MVar ()] -> IO ()
waitOnChildren children = do
  childs <- takeMVar children
  case childs of
    [] -> return ()
    (x:xs) -> do
    putMVar children xs
    takeMVar x
    waitOnChildren children

runPar :: Show r => IO r -> IO r -> IO r
runPar k1 k2 = do
  --children <- newMVar []
  --results <- newMVar []
  result <- newEmptyMVar
  -- We might cause some unintentional ordering by spawning one of these
  -- first, as it gets a head start. But.. I think that's unavoidable.
  --forkChild results children $ wrapProc k1
  --forkChild results children $ wrapProc k2
  forkIO $ wrapProcPrint k1
  forkIO $ wrapProcRes k2 result
  -- Block until all children have returned
  --waitOnChildren children
  --takeMVar results
  takeMVar result

runProc :: Show r => Proc r r -> IO r
runProc p = runStateT (runCont p (\r -> return r)) 0

-- All the tests have data returned in an unexpected order. This make
-- sense in some of the tests, as the parallelism is non-deterministic as
-- to which one will finish first. The tests with explicit yields,
-- however, are slightly more interesting. While in a concurrent
-- environment, without true parallelism, the yield would force one
-- thread to wait, in a true parallel environment, with enough cores for
-- all threads, the yield is useless. 
--
-- Unfortunately, this can cause a race condition in some of the
-- assumptions we make, for instance, in test5. We assume that, having
-- yielded, the second thread will receive the value we sent before we
-- manage to receive. This might not be the case, however, if the first
-- thread is running faster than the second thread.
--
--
-- Expected: [1,2]
-- Result: [2,1] or [1,2]
test1 :: Proc Int Int
test1 = (par (return 1) (return 2))

-- Expected: [3,1,2]
-- Results: [2,1,3]
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
-- Results: [0,11,1]
test4 :: Proc Int Int
test4 = do ch <- newChan
           (foldr1 par
                 [do x <- recv ch; send ch (x+1); return 0,
                  return 1,
                  do send ch 10; yield; y <- recv ch; return y])

-- Expected: [10, 1]
-- Results: [1,1,10] or [2,0]
--
-- There's a race condition caused by the channel, it seems. We can
-- occasionally pull off the x we sent before the other thread has had
-- a chance to receive it, despite the yield. It's interesting that
-- we're both able to pull the value off the channel simultaneously,
-- however.
--
-- The only solution to this issue I can see is named channels.
--
-- What's more curious is the result we normally get, being [1,1,10]. I
-- haven't figured out why we get two 1's. Maybe it's due to both
-- duplicated continutations and true parallelism.
--
-- XXX: I can't figure out how to get the Cont monad's return to do what
-- I want, as I have to give a function of type r -> IO r, but I need a
-- function r -> Proc r a. So... we return with endProcess, and return
-- from there after checking for backtracking messages and such
test5 :: Proc Int Int
test5 = do ch1 <- newChan
           ch2 <- newChan
           par
            (do x <- choose (return 1) (return 2)
                send ch1 x
                yield
                y <- recv ch2
                if y == 0
                  then backtrack
                  else endProcess y)
            (do a <- recv ch1
                if a == 1
                  then do send ch2 0; endProcess 0
                  else do send ch2 1; endProcess 10)
