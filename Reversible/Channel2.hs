module Reversible.Channel2 (
  send,
  recv, 
  ChannelState(..),
  getStates,
       -- States: 
       -- 0,0,0 < Consistent
       -- 1,0,0 < Send request
       -- 1,1,0 < Receiver backtrack emit
       -- 1,0,1 < Comm accept
       -- 1,1,1 < Consistent
       -- 0,1,1 < Send backtrack emit
       -- 0,1,0 < Backtrack accept
       -- 0,0,1 < Receive request
  Channel,
  newChannel,
  newEmptyChannel
  ) where

  import Data.IORef
  import Reversible.LogicalTime 

  data Channel_ a = Channel_ {
    chanValue :: a,
    sendTime :: Time,
    chanTime :: Time,
    recvTime :: Time
  }

  type Channal a = IORef (Channel_ a)

  data ChannelState =
      Consistent 
    | SendReq
    | SendInt1
    | SendBack
    | SendBInt1
    | RecvAck
    | CommAcc
    | RecvBack
    | BackAck
    deriving (Eq,Show,Read)

  getState :: Channel a -> IO ChannelState
  getState ch = do
    ch <- readIORef ch
    return $ case (compare(sendTime ch, recvTime ch),
                   compare(sendTime ch, chanTime ch),
                   compare(recvTime ch, chanTime ch)) of
      -- Consistent
      (EQ, EQ, EQ) ->
      -- SendReq
      (_, GT, EQ) ->
      -- SendBack
      (_, LT, EQ) ->
      -- CommInt 
      (LT, GT, _) ->
      -- RecvAck
      (EQ, GT, _) ->
      -- RecvBack
      (_, EQ, LT) ->
      -- Back
      (EQ, _, LT) ->
      -- Intermediate states
      -- SendBack1
      (GT, LT, LT) ->
      -- RecvInt2
      (_, GT, LT) ->
      -- RecvBack1
      (LT, LT, LT) ->
      -- Else should be an error
       -- 0,0,0 < Consistent
       -- 1,0,1 < Comm accept
       -- 0,1,0 < Backtrack accept
      EQ -> case compare(sendTime ch, chanTime ch) of
              -- 0,0,0 < Consistent
            EQ -> Consistent
              -- 1,0,1 < Comm accept
            GT -> CommAccept
              -- 0,1,0 < Backtrack accept
            LT -> BacktrackAccept
       -- 0,1,1 < Send backtrack emit
       -- 0,0,1 < Receive request
      LT -> if sendTime ch < chanTime ch
             then SendBacktrack
             else RecvRequest
       -- 1,0,0 < Send request
       -- 1,1,0 < Receiver backtrack emit
      GT -> if sendTime ch > chanTime ch
             then SendRequest
             else RecvBacktrack

  predList :: Channel_ a -> [Maybe ChannelState]
  predList ch = [consist, sendReq, recvReq, commAcc, recvBack,
                 sendBack, backAcc]

  -- I can't believe something like this isn't built in
  maybeIf :: Bool -> a -> Maybe a
  maybeIf b s = if b then Just s else Nothing

  -- I think I just abstracted too much
  type ChanRel = Time -> Time -> Bool
  pred :: ChanRel -- c ? r
          -> ChanRel -- c ? s
          -> ChanRel -- s ? r
          -> ChannelState
          -> Channel_ a
          -> Maybe ChannelState
  pred ch r1 r2 r3 st = let 
    c = chanTime ch
    r = recvTime ch
    s = sendTime ch in
      maybeIf ((r1 c r) && (r2 c s) && (r3 s r)) st

  top :: a -> a -> Bool
  top _ _ = True

  
  -- they will all by :: Channel_ a -> Maybe ChannelState
  -- c ? r, c ? s, s ? r
  consit = pred (==) (==) (==) Consistent
  sendReq = pred (==) (<) top SendReq
  -- s > c, r > s
  sendInt1 ch = pred top (<) (<) SendInt1
  -- c < s, s = r
  recvAck ch = pred top (<) (==)
  -- s < c, c = r
  sendBack ch = pred (==) (>) top
  -- r < c, s < c, r < s
  sendBInt1 ch = pred (>) (>) (>)
    
  getState :: Channel_ a -> ChannelState
  getState ch = let ls = map (\f -> case f ch of {
      Nothing -> (False, undefined);
      Just st -> (True, st)}) predList
    in
    case len (dropWhile (\(a,_) -> not a) ls) of
      1 -> head ls
      a | a > 1 -> error "This channel is in more than 1 state: " ++
        (show ls)
      a | a == 0 -> error "This channel is not in a state!"
      _ -> error "Appearently numbers don't work"
  
  _putSendTime :: Time -> Channel_ a -> Channel_ a
  _putSendTime time ch = Channel_ {
      chanValue = chanValue ch, 
      sendTime = time, 
      chanTime = chanTime ch, 
      recvTime = recvTime ch}

  _putRecvTime :: Time -> Channel_ a -> Channel_ a
  _putRecvTime time ch = Channel_ {
    chanValue = chanValue ch, 
    sendTime = sendTime ch, 
    chanTime = chanTime ch, 
    recvTime = time}

  _putChanTime :: Time -> Channel_ a -> Channel_ a
  _putChanTime time ch = Channel_ {
    chanValue = chanValue ch, 
    sendTime = sendTime ch, 
    chanTime = time, 
    recvTime = recvTime ch}

  _putChanValue :: a -> Channel_ a -> Channel_ a
  _putChanValue val ch = Channel_ {
    chanValue = val, 
    sendTime = sendTime ch, 
    chanTime = chanTime ch, 
    recvTime = recvTime ch}

  newChannel :: a -> IO (Channel a)
  newChannel a = newIORef Channel {
    chanValue = a,
    sendTime = baseTime,
    chanTime = baseTime,
    recvTime = baseTime
    }

  newEmptyChannel :: IO (Channel a)
  newEmptyChannel =  newChannel undefined
  
  atomicModifyChannel :: Channel a -> states -> 
                        (Channel_ a -> (Channel_ a, b)) ->  IO b
  atomicModifyChannel ch states f = atomicModifyIOREf (\_chan ->
    if elem (getStates _chan) states
      then f _chan
      else error "Invalid precondition")

  send :: Time -> a -> Channel a -> IO Time
  send time ch val = do 
    _ <- atomicModifyChannel ch [Consistent] (\_chan ->
      (_putChanValue val (_putSendTime (incTime time) ch), ()))
    -- Now we must wait until the state is changed. We need some sort of
    -- busy-loop over atomicModifyChannel.
    fix (\f -> do
      rep <- atomicModifyChannel ch [SendInt1, RecvAck, SendReq] (\_chan ->
        return $ case getState _chan of
          SendReq -> (_chan, (False, undefined)) -- Still waiting
          RecvAck -> (_putChanTime (sendTime _chan) _chan, 
            (True, (sendTime _chan))) -- Need to update channel time
          SendInt1 -> let chp = _putSendTime (recvTime _chan) _chan in
            let chpp = _putChanTime (recvTime _chan) chp in
              (chpp, (True, (recvTime _chan))))
            -- Need to update channel and send time
      if fst rep 
        then f 
        else snd rep)

  recv :: Time -> Channel a -> IO (a, Time)
  recv time ch = atomicModifyChannel ch [SendReq] (\_chan ->
     let val = chanValue ch 
         chp = _putChanValue undefined ch
         stime = sendTime chp
         rtime = if stime > (incTime time)
                  then stime
                  else (incTime time) 
         in
     (_putRecvTime rtime chp, (rtime, val)))

