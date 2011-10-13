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
--  backtrack,
--  endProcess,
--  endChoice,
  yield,
  Proc
  ) where
  
  import Reversible.Channel hiding (recv,send)
  import qualified Reversible.Channel as RC (recv,send)
  import Reversible.LogicalTime
  import Reversible.Concurrent

  import Data.Dequeue

  import Control.Applicative hiding (empty)
  import Control.Monad.Cont
  import Control.Monad.State
  import Control.Concurrent hiding (yield,newChan)
  import qualified Control.Concurrent as CC (yield,newChan)

  -- The types for processes
  
  data Checkpoint r = CheckPoint {
    checkpointTime :: Time,
    checkpointSendChs :: [ChannelHash],
    checkpointRecvChs :: [ChannelHash],
    checkpointK :: () -> IO r
  }

  data ThreadState r = ThreadState {
    threadTime :: Time,
    lastChoice :: Time,
    kDequeue :: BankersDequeue (Checkpoint r),
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

  pushCheckpoint :: (() -> IO r) -> Comp r ()
  pushCheckpoint k = do
    time <- getTime
    deq <- getDeque 
    sChs <- mapM (liftIO saveChannel) getSendCh
    rChs <- mapM (liftIO saveChannel) getRecvCh
    putDeque $ pushFront deq 
      Checkpoint {
        checkpointTime = time,
        checkpointSendChs = sChs,
        checkpointRecvChs = rChs,
        checkpointK = k
      }

  -- end of interface
  
  -- Basic parallelism with communication
  
  runChild :: ThreadState r -> (ThreadState r -> ThreadState r) -> Comp r r -> IO r
  runChild st f k = evalStateT k $ f st

  par :: Show r => Proc r a -> Proc r a -> Proc r a
  par (Cont c1) (Cont c2) = Cont (\k -> do
    let baseState = \_ -> (ThreadState {threadTime = baseTime, 
                                        lastChoice = baseTime,
                                        kDequeue = pushFront empty (\_ -> do 
                                          _ <- killThread <$> myThreadId
                                          return undefined),
                                        sendCh = [],
                                        recvCh = []})
    let b1 = runChild (baseState ()) id (c1 k)
    let b2 = runChild (baseState ()) id (c2 k)
    liftIO $ runPar b1 b2)

  yield :: Proc r ()
  yield = Cont (\k -> do liftIO CC.yield; k ())

  newChan :: Proc r (Channel a)
  newChan = Cont (\k -> do 
    ch <- liftIO $ newEmptyChannel
    k ch)

  send :: Channel a -> a -> Proc r ()
  send ch v = Cont (\k -> do
    time <- getTime
    time <- liftIO $ RC.send time ch v
    putTime time
    k ())

  recv :: Channel a -> Proc r a
  recv ch = Cont (\k -> do
    time <- getTime
    (v, nTime) <- liftIO $ RC.recv time ch
    putTime time
    k v)

  -- Backtracking
  
  fakeCont :: ThreadState r -> Comp r r -> (() -> IO r)
  fakeCont st exp =  (\_ -> evalStateT exp st)

  choose :: Proc r a -> Proc r a -> Proc r a
  choose (Cont c1) k2@(Cont c2) = Cont (\k -> do 
    pushCheckpoint $ (fakeCont <$> get) (c2 k)
    -- Note, since we evaled c2 already, it's last choice is the
    -- previous choice point, not this one. However, in c1, the choice
    -- point is changed to be this one.
    putLastChoice <$> getTime
    c1 k)

  backtrack :: Proc r r
  backtrack = Cont (\_ -> do
    c <- backToChoice []
    liftIO $ c ())

  -- Run 
  runProc :: Proc r r -> IO r
  runProc p = evalStateT 
    (runCont p (\r -> return r)) 
    ThreadState {threadTime = baseTime, 
                 lastChoice = baseTime, 
                 kDequeue = empty, 
                 sendCh = [], 
                 recvCh = []} 
