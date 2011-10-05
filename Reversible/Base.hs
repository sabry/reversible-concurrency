{-# LANGUAGE CPP #-}

-- Note:
--   TODO tags indicate (shockingly) something that needs to be done
--   XXX tags indicate comments, notes to self, observations, or
--   thoughts that are not tasks, but things that need to be considered
--   or things to be aware of.
--
-- I've taken up not deleting pase comments, as they might be useful as
-- notes if/when I have to write about this. Instead, completed ones are
-- surrounded by HTML strike tags (<s></s>) 
--
-- To run with true parallelism, use: ghci +RTS -N -RTS. Of course, you
-- need a machine with at least 2 cores to take advantage of this.

-- <s>However, due to some race conditions which exist right now, it's
-- actually preferably to test this without parallelism, instead
-- allowing ghc to use it's internal scheduler for concurrency.</s>
--
module Reversible.Base (
    par, 
    send, 
    recv, 
    newChan, 
    runProc, 
    choose, 
    backtrack, 
    endProcess,
    endChoice,
    yield,
    Proc
  ) where

import Control.Concurrent hiding (yield,newChan)
import Control.Monad.Cont
import qualified Control.Concurrent as CC (yield,newChan)
import Control.Monad.State

#ifdef DEBUG

import Debug.Trace

#endif

import qualified Data.Map as M 
import Data.Map (Map, adjust, insert)
import List (delete)

#ifndef DEBUG

trace _ r = r

#endif

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
           | Choose deriving (Show)

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
         | Continue deriving (Show)

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
type FakeCont r = () -> IO r
type ContStack r = Stack (FakeCont r, KType)
type ThreadState r = (SpecTag, ContStack r, [Chid])
--type ThreadMap r = Map Pid (ThreadState r) 

-- <s>XXX: This [Ch] should probably be more like Map Chid Ch. We
-- need this to keep track of message id's for each channel.</s> 
type GlobalState r = (Pid, Chid, ThreadState r, Map Chid Ch)
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

--(!) :: Show r => Ord r => Map r a -> r -> a
--(!) m r =  trace (show r) $ (m M.! r)
(!) = (M.!)

-- Some interface methods for the thread state, and global state.
-- <s>TODO: Lots of interfaces methods are missing. Most of these are
-- broken now.</s>
getPid :: Comp r Pid
getPid = fmap fst4 get

modifyPid :: (Int -> Int) -> Comp r ()
modifyPid f = do 
  (pid,a,b,c) <- get
  put (f pid,a,b,c)

getChid :: Comp r Chid 
getChid = fmap snd4 get

incChid :: Comp r Chid
incChid = do
  (a, b, c, d) <- get
  let chid = b+1
  put (a, chid, c, d)
  return b

getThreadState :: Comp r (ThreadState r)
getThreadState = fmap thrd4 get 

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
-- _this :: (Pid -> Comp r a) -> Comp r a
-- _this f = do 
--   pid <- getPid
--   trace ("This Pid: " ++ (show pid)) $ return ()
--   f pid

-- (a -> b) -> f a -> f b

-- XXX: I'm stupid, we can run evalStateT for each new thread, so they
-- only need to keep track of their own state.
-- Get the thread state, stored in the global ThreadMap, by it's Pid
--_getThreadState :: Pid -> Comp r (ThreadState r)
--_getThreadState pid = fmap (! pid) getThreadMap 

putThreadState :: ThreadState r -> Comp r ()
putThreadState st = do
  (a,b,map,d) <- get
  put (a,b,st,d)

-- Get an element out of the ThreadState triple.
_getThreadStatePos :: (ThreadState r -> d) -> Comp r d
_getThreadStatePos f = fmap f $ getThreadState

getSpecTag :: Comp r SpecTag
getSpecTag = fmap fst3 $ getThreadState

setSpecTag :: Bool -> Comp r ()
setSpecTag t = do
  (_, a, b) <- getThreadState 
  putThreadState (t, a, b)

getContStack :: Comp r (ContStack r)
getContStack = fmap snd3 $ getThreadState

putContStack :: ContStack r -> Comp r ()
putContStack st = do
  (a, _, b) <- getThreadState 
  putThreadState (a, st, b)

getSpecChans :: Comp r [Chid]
getSpecChans = fmap thrd3 $ getThreadState 

putSpecChans :: [Chid] ->  Comp r ()
putSpecChans c = do
  (a,b,_) <- getThreadState
  putThreadState (a,b,c)

-- Add a new full duplex channel to the map, creating a channel id for
-- it, and returning that id.
addCh :: (Chan Msg, Chan Msg, SpecTag, Msgid) -> Comp r Chid
addCh ch = do 
  map <- getChanMap
  chid <- incChid 
  let map' = insert chid ch map
  putChanMap map'
  return chid

addSpecCh :: Chid -> Comp r ()
addSpecCh ch = do
  chxs <- getSpecChans
  if (elem ch chxs)
    then return ()
    else putSpecChans $ ch:chxs
  --trace ("Spec Chans: " ++ (show $ ch:chxs)) $ return ()

remSpecCh :: Chid -> Comp r ()
remSpecCh ch = do
  chxs <- getSpecChans
  putSpecChans $ delete ch chxs

-- Build a send message, incrementing the message Id.
buildSendMsg :: Chid -> Int -> Comp r Msg
buildSendMsg chid s = do
  msgid  <- getMsgid chid
  spec <- getSpecTag 
  return $ Send spec msgid s

buildAckMsg :: Chid -> Comp r Msg
buildAckMsg chid = do
  msgid <- getMsgid chid
  spec <- getSpecTag 
  return $ Acknowledge spec msgid

buildUnsendMsg :: Msgid -> Comp r Msg
buildUnsendMsg msgid = return $ Unsend msgid

buildContinueMsg :: Comp r Msg
buildContinueMsg = return $ Continue

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
  --trace "Getting chan map" $ return ()
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
  ch <- _getSendCh chid
  liftIO $ readChan ch 

nbRecvMsg :: Chid -> Comp r (Maybe Msg)
nbRecvMsg chid = do
  ch <- _getSendCh chid
  t <- liftIO $ isEmptyChan ch
  if t 
    then return Nothing
    else fmap Just $ liftIO $ readChan ch

-- Receive an acknowlegement, on the receive channel.
recvAck :: Chid -> Comp r Msg
recvAck chid = do
  ch <- _getRecvCh chid 
  _ <- incMsgid chid
  liftIO $ readChan ch

-- Push a continuation onto the stack.
pushCont :: (FakeCont r) -> KType -> Comp r ()
pushCont k ktype = do
  stack <- getContStack
  putContStack $ push (k, ktype) stack
  --trace ("Pushing continuation... " ++ (show $ length stack)) $ return ()

popCont :: Comp r (FakeCont r, KType)
popCont = do
  --trace "Popping continuation... " $ return ()
  st <- getContStack
  --trace ("pop stack lenght: " ++ (show $ length st)) $ return ()
  let (h, nst) = pop st;
  --trace ("Popped a k.. " ++ (show $ length nst)) $ return ()
  putContStack nst
  --trace "Returning with k" $ return ()
  return h

-- <s>TODO: Should runStateT with the same state, except the current
-- thread's pid incremented.</s>
runChild :: GlobalState r -> (Int -> Int) -> Comp r r -> IO r
runChild (pid, a,b,c) f k = evalStateT k 
  (f pid, a, b, c)

-- end of interface

-- XXX: If a parent continues past a par, then Pids can repeat in the
-- parent and children.
par :: Show r => Proc r a -> Proc r a -> Proc r a
par (Cont c1) (Cont c2) = Cont (\k -> do
  st <- get
  let b1 = runChild st (+0) (c1 k) -- runStateT (c1 k) (ppid+1, map)
  let b2 = runChild st (+0) (c2 k) --runStateT (c2 k) (ppid+2, map)
  --modifyPid (+2)
  liftIO $ runPar b1 b2)

-- Still useful in a concurrent environment, but useless in true
-- parallel environment. 
yield :: Proc r ()
yield = Cont (\k -> do 
  liftIO CC.yield; k ())

-- <s>TODO: Alter to create a full duplex channel, init message IDs, etc.
-- Should also mark as speculative if the current thread is.</s>
newChan :: Proc r Chid
newChan = Cont (\k -> do 
  sch <- liftIO CC.newChan
  rch <- liftIO CC.newChan
  chid <- addCh (sch, rch, False, 0)
  k chid)

makeSpec :: Chid -> Comp r ()
makeSpec ch = do
  setChTag True ch
  setSpecTag True
  addSpecCh ch

-- XXX: I think callCC will work this way, but it might be interesting
-- to look at how stabilizers could play a role here.
send :: Chid -> Int -> Proc r ()
send ch s = Cont (\k -> do 
  -- <s>TODO: Lots of undefined methods. At this point, they're pretty much
  -- methods, given the way I'm using them is rather object oriented.
  -- TODO: Double check that these are the right data structure, i.e. the
  -- order of variables.</s>
  --trace ("Sending " ++ (show s) ++ "...") $ return ()
  msg@(Send spec_s msgid_s p) <- buildSendMsg ch s
  sendMsg ch msg
  --trace ("Sent!" ++ show spec_s ++ " Getting acknowledgement ...") $ return ()
  (Acknowledge spec_a msgid_a) <- recvAck ch
  --trace ("Acknowledged! " ++ show spec_a) $ return ()
  --liftIO $ putStrLn ("Pid: " ++ (show pid) ++ " Sending: " ++ (show s))
  --liftIO $ writeChan ch $ msg
  -- <s>XXX: Somehow these boolean tests are causing 'map.find: element not
  -- in map' errors?</s>
  --trace (show (spec_a || spec_s)) $ return ()
  when (spec_a || spec_s) $ makeSpec ch
  --trace "Passed checks" $ return ()
  -- XXX: Sanity crash
  when (msgid_s /= msgid_a) $ undefined
  st <- get
  pushCont (fakeCont st (runCont (send ch s) k)) $ Sent ch msgid_s
  k ());
  
-- <s>TODO: Same as send.</s>
recv :: Chid -> Proc r Int
recv ch = Cont (\k -> do 
  (Send spec_s msgid_s p) <- recvMsg ch
  --trace ("Received " ++ show p ++ " " ++ show spec_s) $ return ()
  ack@(Acknowledge spec_a msgid_a) <- buildAckMsg ch 
  sendAck ch ack
  --trace ("Sent acknowledgment " ++ show spec_a) $ return ()
  --trace ("IDS: " ++ (show msgid_s) ++ " " ++ (show msgid_a)) $ return ()
  --trace (show (spec_a || spec_s)) $ return ()
  when (spec_a || spec_s) $ makeSpec ch
  --trace "Passed check" $ return ()
  -- XXX: Sanity crash
  when (msgid_s /= msgid_a) $ undefined
  st <- get
  pushCont (fakeCont st (runCont (recv ch) k)) $ Sent ch msgid_a
  k p)

fakeCont :: GlobalState r -> Comp r r -> (() -> IO r)
fakeCont st exp =  (\_ -> evalStateT exp st)

choose :: Proc r a -> Proc r a -> Proc r a
choose (Cont c1) k2@(Cont c2) = Cont (\k -> do 
  -- XXX: This seems a little naive. We store the next choice, and
  -- always take the first one first. There feels like a little room for
  -- abstraction here.
  -- Unexpected behavior if we try to backtrack after the second choice.
  st <- get
  pushCont (fakeCont st (do trace "Second choice taken" $ return (); c2 k)) $ Choose
  setSpecTag True
  trace "First choice taken" $ return ()
  c1 k)
--  st <- get
--  b1 <- liftIO $ runStateT (c1 k) st
--  liftIO $ return b1)

mapSendContinue :: [Chid] -> Comp r ()
mapSendContinue chs = do 
  msg <- buildContinueMsg
  mapM (\ch -> do
    sendMsg ch msg
    _ <- recvAck ch
    return ()) chs
  return ()

-- <s>TODO: Implement</s>
backToChoice :: [Chid] -> Comp r (FakeCont r)
backToChoice xs = do
  trace ("Entering backToChoice") $ return ()
  (k, ktype) <- popCont
  trace ("Got ktype: " ++ show ktype) $ return ()
  case ktype of
    -- <s>TODO: Ensure var order of contructors</s>
    -- <s>TODO: More helpers to build</s>
    Sent chid msgid -> do 
      trace "Sending unsend" $ return ()
      msg <- buildUnsendMsg msgid
      sendMsg chid msg
      ack <- recvAck chid
      trace ("Got acknowledge for unsend" ++  (show ack)) $ return ()
      backToChoice $ chid:xs
    Choose -> do
      trace ("Found choose k: " ++ (show xs)) $ return ()
      mapSendContinue xs
      return k

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
backtrack :: Proc r r
backtrack = Cont (\_ -> do
  trace "Backtracking started!" $ return ()
  c <- backToChoice []
  trace "Backtrack leaving" $ return ()
  liftIO $ c ())
  --pid <- getPid
  --liftIO $ putStrLn ("Pid: " ++ (show pid) ++ " backtracking..")
  --liftIO $ return [])
  
-- Pop continutations until we find the one with the message id we're
-- looking for.
backToMsgid :: Chid -> Msgid -> Comp r (FakeCont r)
backToMsgid chid msgid = do
  trace ("Entering backToMsgId " ++ show msgid) $ return ()
  (c, ktype) <- popCont
  trace ("Back to msgID: got " ++ show ktype) $ return ()
  case ktype of
    Sent chid' msgid' | (msgid == msgid') && (chid == chid') -> do
      trace ("Msg id's matched, returning") $ return ()
      return c
    _ -> trace "Nope" $ backToMsgid chid msgid

-- Wait on a particular channel for more Unsend or a Continue
waitForMsg :: Chid -> (FakeCont r) -> Comp r (FakeCont r)
waitForMsg chid c = do
  trace ("Waiting for more on chid: " ++ (show chid)) $ return ()
  msg <- recvMsg chid 
  msg' <- buildAckMsg chid 
  sendAck chid msg'
  case msg of
    Continue -> do
      trace ("Got continue message") $ return ()
      return c 
    Unsend msgid  -> do
      trace ("Got unsend message" ++ (show msgid) ++ " " ++ (show chid)) $ return ()
      c <- backToMsgid chid msgid
      trace ("Found k for unsend") $ return ()
      waitForMsg chid c
    _ -> trace ("Bad message: " ++ show msg) $ undefined

-- Listen on all our speculative channels for either Continue or Unsend
-- messages.
listenForMsg :: [Chid] -> Maybe (FakeCont r) -> Comp r (Maybe (FakeCont r))
listenForMsg [] c = return c
listenForMsg ls@(x:xs) c = do
  trace ("listen " ++ show ls) $ return ()
  msg <- nbRecvMsg x
  case msg of
    Just msg' -> do
      ack <- buildAckMsg x 
      sendAck x ack
      case msg' of
        Continue -> do
          xs <- getSpecChans
          trace ("MEOW " ++ (show xs)) $ return ()
          trace ("Got continue on " ++ (show x)) $ return ()
          remSpecCh x 
          --setChTag False x
          xs <- getSpecChans
          trace ("After remove: " ++ show xs) $ return ()
          listenForMsg xs c
        Unsend msgid -> do
          trace ("Got unsend on " ++ (show x)) $ return ()
          c <- backToMsgid x msgid
--          trace ("Got c.. waiting for continue" ++ (show msgid)) $ return ()
--          c <- waitForMsg x c
--          return $ Just c
          listenForMsg (x:xs) $ Just c
        _ -> trace (show msg') $ undefined
    Nothing -> listenForMsg (xs) c
 
-- Like the pokemon, not the greek letter.
mew f = f (mew f)

remChans :: [Chid] -> Comp a [()]
remChans [] = return []
remChans (x:xs) = do
  remSpecCh x 
  ls <- remChans xs
  return $ () : ls

endChoice :: r -> Proc r r
endChoice r = Cont (\k -> do
  chs <- getSpecChans
  mapM (\ch -> remSpecCh ch) chs
  mapSendContinue chs
  k r)

endProcess :: Show r => r -> Proc r r
endProcess r = Cont (\k ->
  mew (\f -> \r -> do
    xs <- getSpecChans
    trace ("endProcess: " ++ show xs) $ return ()
    if (null xs) 
      then do 
        q <- r
        trace ("Leaving end process with: " ++ show q) $ k q
      else do
        msg <- listenForMsg xs Nothing
        case msg of
          Just c -> f $ liftIO $ c ()
--            r <- liftIO $ (\r -> trace (show r) $ c r) ()
--            (\r -> trace (show r) $ k r) r
          Nothing -> f r)  $ return r)
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
  putStrLn $ show r

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
runProc p = evalStateT 
  (runCont p (\r -> return r)) 
  (0, 0, (False, empty, []), M.empty)

