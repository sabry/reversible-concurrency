-- At any communication event, and of course choice points, we
-- checkpoint. The checkpoint should save not only our state and
-- continuation, but a copy of all our channels at that time, and our
-- timestamp.
--
-- When we return, we set our time and the channels time to the maximum
-- time. We need to set the time correctly on all our channels,
-- differently for send and receive. 
--
-- We then wait on all channels, specifically on the time stamp. If
-- our partner's time increases to maximum, we stop waiting on that
-- channel. 
--
-- If a channels time decreases, our partner has backtracked, and we
-- should go back to that channels time. Note that since the partner has
-- restored the channels, all channels should be in a consistent state,
-- and this should propagate through the network.
--
-- If we need to backtrack, we pop continuations until we find the
-- time we're looking for. Then, we restore all our channels.
--
-- XXX: Race condition. What if a partner sends/recvs as we set the
-- channel time backwards? Since they are synchronous, the partner can't
-- detect that we've backtracked. Perhaps we can ignore this for now,
-- and add a form of concurrency to check for backtracking while we
-- compute. This will solve the other issue of only backtracking upon
-- return.
module Reversible.NewBase (
  par,
  send,
  recv,
  newChan,
  runProc,
  choose,
  backtrack,
  endProcess,
--  endChoice,
  yield,
  Proc
  ) where
  
  -- Helpers: 
  --
  -- The Channel we use for communication, which keeps logical time at
  -- each event, and can be hashed and unhashed, storing and restoring
  -- the channel to and from previous states
  import Reversible.Channel hiding (recv,send)
  -- We qualify recv and send as they are the names we want for the
  -- API
  import qualified Reversible.Channel as RC (recv,send)
  -- A model of logical time, used by the processes for backtracking.
  import Reversible.LogicalTime
  -- The helpers that actually spawn threads.
  import Reversible.Concurrent
  import Reversible.Debug

  -- We store continuations in a dequeue, allowing it to act like a
  -- stack, and to be garbage collected from the bottom when we
  -- implement that.
  import Data.Dequeue hiding (null)

  -- So I can use <$>
  import Control.Applicative hiding (empty)
  -- We use continuations to store checkpoints.
  import Control.Monad.Cont
  -- We use state to store thread local state
  import Control.Monad.State
  -- Thread primitives
  import Control.Concurrent hiding (yield,newChan)
  -- Thread primitives that we want to use in our API
  import qualified Control.Concurrent as CC (yield,newChan)

  -- The types for processes
  
  data CheckPoint r = CheckPoint {
    checkpointTime :: Time,
    checkpointSendChs :: [Channel Int],
    checkpointSendHashes :: [ChannelHash Int],
    checkpointRecvChs :: [Channel Int],
    checkpointRecvHashes :: [ChannelHash Int],
    checkpointK :: () -> IO r
  }

  type KDeq r =  BankersDequeue (CheckPoint r)

  data ThreadState r = ThreadState {
    threadTime :: Time,
    lastChoice :: Time,
    kDequeue :: KDeq r,
    sendCh :: [Channel Int],
    recvCh :: [Channel Int]
  } 

  type Comp r a = StateT (ThreadState r) IO a
  type Proc r a = Cont (Comp r r) a

  -- interface methods
  getTime :: Comp r Time 
  getTime = threadTime <$> get

  putTime :: Time -> Comp r ()
  putTime time = modify (\st -> 
    ThreadState {threadTime = time, 
                 lastChoice = (lastChoice st),
                 kDequeue = (kDequeue st), 
                 sendCh = (sendCh st), 
                 recvCh = (recvCh st)})
  
  getLastChoice :: Comp r Time
  getLastChoice = lastChoice <$> get

  putLastChoice :: Time -> Comp r ()
  putLastChoice time = modify (\st ->
    ThreadState {threadTime = (threadTime st),
                 lastChoice = time,
                 kDequeue = (kDequeue st),
                 sendCh = (sendCh st), 
                 recvCh = (recvCh st)})

  getDeque :: Comp r (KDeq r)
  getDeque = kDequeue <$> get

  putDeque :: KDeq r -> Comp r ()
  putDeque deq = modify (\st ->
    ThreadState {threadTime = (threadTime st), 
                 lastChoice = (lastChoice st),
                 kDequeue = deq, 
                 sendCh = (sendCh st), 
                 recvCh = (recvCh st)})

  getSendCh :: Comp r [Channel Int]
  getSendCh = sendCh <$> get

  putSendCh :: [Channel Int] -> Comp r ()
  putSendCh chs = modify (\st ->
    -- There's got to be an abstraction for this.
    ThreadState {threadTime = (threadTime st),
                 lastChoice = (lastChoice st),
                 kDequeue = (kDequeue st),
                 sendCh = chs,
                 recvCh = (recvCh st)})

  pushSendCh :: Channel Int -> Comp r ()
  pushSendCh ch = putSendCh <$> ((:) ch) =<< getSendCh
  
  getRecvCh :: Comp r [Channel Int]
  getRecvCh = recvCh <$> get

  putRecvCh :: [Channel Int] -> Comp r ()
  putRecvCh chs = modify (\st ->
    ThreadState {threadTime = (threadTime st),
                 lastChoice = (lastChoice st),
                 kDequeue = (kDequeue st),
                 sendCh = (sendCh st),
                 recvCh = chs})

  pushRecvCh :: Channel Int -> Comp r ()
  -- HA! that's unreadable. Maybe I should rewrite that in a more
  -- readable way...
  pushRecvCh ch = putRecvCh <$> ((:) ch) =<< getRecvCh

  pushCheckPoint :: (ThreadState r -> () -> IO r) -> Comp r ()
  pushCheckPoint k = do
    st <- get
    time <- getTime
    deq <- getDeque 
    sChs <- getSendCh
    rChs <- getRecvCh
    sHashes <- mapM (liftIO . saveChannel) sChs
    rHashes <- mapM (liftIO . saveChannel) rChs
    putDeque $ pushFront deq 
      CheckPoint {
        checkpointTime = time,
        checkpointSendChs = sChs,
        checkpointSendHashes = sHashes,
        checkpointRecvChs = rChs,
        checkpointRecvHashes = rHashes,
        checkpointK = k st
      }

  popCheckPoint :: Comp r (CheckPoint r)
  popCheckPoint = do
    deq <- getDeque
    case popFront deq of
      (Nothing, _) -> undefined
      (Just chkp, deq) -> do
        putDeque deq
        return chkp

  -- end of interface
  
  -- Basic parallelism with communication
  
  runChild :: ThreadState r -> (ThreadState r -> ThreadState r) -> Comp r r -> IO r
  runChild st f k = evalStateT k $ f st

  newThreadState :: () -> ThreadState r 
  newThreadState _ =  
    ThreadState {
      threadTime = baseTime, 
      lastChoice = baseTime,
      kDequeue = pushFront empty 
        CheckPoint {
          checkpointTime = baseTime,
          checkpointSendChs = [],
          checkpointSendHashes = [],
          checkpointRecvChs = [],
          checkpointRecvHashes = [],
          -- When we backtrack past a par call, we want to kill the
          -- thread. This should be more abstract though.
          checkpointK = (\_ -> do 
              _ <- killThread <$> myThreadId
              return undefined)
        },
      sendCh = [],
      recvCh = []
    }

  par :: Show r => Proc r a -> Proc r a -> Proc r a
  par (Cont c1) (Cont c2) = Cont (\k -> do
    traceM 1 $ "Base.par spawning 2 children"
    let b1 = runChild (newThreadState ()) id (c1 k)
    let b2 = runChild (newThreadState ()) id (c2 k)
    liftIO $ runPar b1 b2)

  yield :: Proc r ()
  yield = Cont (\k -> do 
    traceM 1 $ "Base.yield"
    liftIO CC.yield
    k ())

  newChan :: Proc r (Channel a)
  newChan = Cont (\k -> do 
    traceM 1 $ "Base.newChan empty channel created"
    ch <- liftIO $ newEmptyChannel
    k ch)

  -- <s>TODO: Take a checkpoint. This only needs to be done when in a
  -- stable region. According to my design above though, we do it
  -- everytime. So, for now, that's what we'll do.</s>
  -- <s>TODO: We also need to record this channel, if we're in a stable
  -- region. For now, we just assume we are</s>
  send :: Channel Int -> Int -> Proc r ()
  send ch v = Cont (\k -> do
    traceM 1 $ "Base.send sending value: " ++ (show v)
    
    -- Prepare for backtracking
    pushCheckPoint $ fakeCont (runCont (send ch v) k)
    pushSendCh ch
    traceM 2 $ "Base.send checkpoint made"
    time <- getTime
    nTime <- liftIO $ RC.send time ch v
    traceM 2 $ "Base.send send complete"

    putTime nTime
    k ())

  -- <s>TODO: Do we take a checkpoint here as well? According to the
  -- design, yes. But, maybe eventually not.</s>
  -- <s>TODO: Need to record this channel if we're in a stable region. For
  -- now, just do it regardless.</s>
  recv :: Channel Int -> Proc r Int
  recv ch = Cont (\k -> do
    traceM 2 $ "Base.recv receiving value"
    pushCheckPoint $ fakeCont (runCont (recv ch) k)
    pushRecvCh ch
    traceM 2 $ "Base.recv checkpoint made"
    time <- getTime
    (v, nTime) <- liftIO $ RC.recv time ch
    traceM 1 $ "Base.recv value received: " ++ (show v)
    putTime nTime
    k v)

  -- Backtracking
  
  fakeCont ::  Comp r r -> ThreadState r -> (() -> IO r)
  fakeCont exp st =  (\_ -> evalStateT exp st)

  choose :: Proc r a -> Proc r a -> Proc r a
  choose (Cont c1) k2@(Cont c2) = Cont (\k -> do 
    traceM 1 $ "Base.choose choice point entered"
    pushCheckPoint $ fakeCont (c2 k)
    traceM 2 $ "Base.choose checkpoint made"
    -- Note, since we evaled c2 already, it's last choice is the
    -- previous choice point, not this one. However, in c1, the choice
    -- point is changed to be this one.
    putLastChoice =<< getTime
    traceM 2 $ "Base.choose last choice updated"
    c1 k)

  timeTravel :: Time -> Comp r (() -> IO r)
  timeTravel time = do
    chkp <- popCheckPoint
    case compare (checkpointTime chkp) time of
      LT -> undefined
      EQ -> 
        let k = checkpointK chkp
            sendChs = checkpointSendChs chkp
            sendHashes = checkpointSendHashes chkp
            recvChs = checkpointRecvChs chkp
            recvHashes = checkpointRecvHashes chkp 
        in
          do liftIO $ mapM_ (uncurry restoreChannel) 
                            (zip sendHashes sendChs)
             liftIO $ mapM_ (uncurry restoreChannel) 
                            (zip recvHashes recvChs)
             putSendCh $ sendChs
             putRecvCh $ recvChs
             traceM 2 $ "Base.timeTravel Checkpoint found"
             return $ checkpointK chkp             
      GT -> timeTravel time
       
  backtrack :: Proc r r
  backtrack = Cont (\_ -> do
    traceM 1 $ "Base.backtrack entering backtrack"
    c <- timeTravel =<< getLastChoice
    traceM 2 $ "Base.backtrack timeTravel complete"
    liftIO $ c ())

  observeChannels :: r -> Comp r r
  observeChannels r = do
    -- If any channel time is set to maxTime, then the sender has
    -- entered endProcess.
    rChs <- liftIO . filterM (\ch -> do
      val <- readMVar $ channelTime $ channelTimeStamp ch
      return $ val /= maxTime) =<< getRecvCh
    putRecvCh rChs
    if null rChs
      -- Once our receive list is null, all our neighbors have finished,
      -- so we return the result.
      then do traceM 2 $ "Base.observeChannels neighbors have finished"; return r
      else do
        -- Otherwise, we look at each channel, and compare the channel
        -- time to our time. If the channel time is less than our time,
        -- the channel has backtracked.
        m <- foldM (\m ch ->
          case m of 
            Nothing -> do
              ourTime <- getTime
              cTime <- liftIO $ readMVar $ channelTime $
                channelTimeStamp ch 
              if cTime < ourTime 
                then return $ Just ch
                else return Nothing
            Just mch -> return m) Nothing rChs
        case m of 
          -- So we backtrack too.
          Just ch -> do
            time <- liftIO $ readMVar $ channelTime $ channelTimeStamp ch
            k <- timeTravel time
            liftIO $ k ()
          -- Otherwise, repeat until our neighbors finish
          Nothing -> do traceM 2 $ "Base.observeChannels recursive step"; observeChannels r

  endProcess :: Show r => r -> Proc r r
  endProcess r = Cont (\k -> do
    traceM 1 $ "Base.endProcess ending with value: " ++ (show r)
    -- Set the channel time to maximum for all sendChs. We leave the
    -- time alone in our thread state, as we'll need it for comparison.
    sChs <- getSendCh
    liftIO $ mapM_ (\ch -> do
      let timestamp = channelTimeStamp ch
      modifyMVar_ (channelTime timestamp) (\_ -> return maxTime)) sChs
    k =<< observeChannels r)

  -- Run 
  runProc :: Proc r r -> IO r
  runProc p = evalStateT 
    (runCont p (\r -> return r)) 
    ThreadState {threadTime = baseTime, 
                 lastChoice = baseTime, 
                 kDequeue = empty, 
                 sendCh = [], 
                 recvCh = []} 
