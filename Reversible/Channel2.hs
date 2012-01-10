module Reversible.Channel2 (
  send,
  recv, 
  ChannelState(..),
  getState,
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
  newEmptyChannel,
  sendBacktrack,
  recvBacktrack
  ) where

  import Data.IORef
  import Reversible.LogicalTime 

  data _Channel a {
    chanValue :: a,
    sendTime :: Time,
    chanTime :: Time,
    recvTime :: Time
  }

  type Channal a = IORef (_Channel a)

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
  
  predList :: _Channel a -> [Maybe ChannelState]
  predList ch = [consist, sendReq, recvReq, commAcc, recvBack,
                 sendBack, backAcc]

  -- I can't believe something like this isn't built in
  maybeIf :: Bool -> a -> Maybe a
  maybeIf b s -> if b then Just s else Nothing

  -- I think I just abstracted too much
  type ChanRel = Time -> Time -> Bool
  pred :: ChanRel -- c ? r
          -> ChanRel -- c ? s
          -> ChanRel -- s ? r
          -> ChannelState
          -> _Channel a
          -> Maybe ChannelState
  pred ch r1 r2 r3 st = let 
    c = chanTime ch
    r = recvTime ch
    s = sendTime ch in
      maybeIf ((r1 c r) && (r2 c s) && (r3 s r)) st

  top :: a -> a -> Bool
  top _ _ = True

  
  -- they will all by :: _Channel a -> Maybe ChannelState
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
    
  getState :: _Channel a -> ChannelState
  getState ch = let ls = dropWhile (\(a,_) -> a) $ 
    map (\f -> case f ch of
                Nothing -> (False, undefined)
                Just st -> (True, st)) predList 
    case len ls of
      1 -> head ls
      a | a > 1 -> error "This channel is in more than 1 state: " ++
        (show ls)
      a | a == 0 -> error "This channel is not in a state!"
      _ -> error "Appearently numbers don't work"
  
  _putSendTime :: Time -> _Channel a -> _Channel a
  _putSendTime time ch = _Channel {chanValue ch, time, chanTime ch, recvTime ch}

  _putRecvTime :: Time -> _Channel a -> _Channel a
  _putRecvTime time ch = _Channel {chanValue ch, sendTime ch, chanTime ch, time}

  _putChanTime :: Time -> _Channel a -> _Channel a
  _putChanTime time ch = _Channel {chanValue ch, sendTime ch, time, recvTime ch}

  _putChanValue :: a -> _Channel a -> _Channel a
  _putChanValue val ch = _Channel {val, sendTime ch, chanTime ch, recvTime ch}

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
                        (_Channel a -> (_Channel a, b)) ->  IO b
  atomicModifyChannel ch states f = atomicModifyIOREf (\_chan ->
    if elem (getState _chan) states
      then f _chan
      else error "Invalid precondition")

  send :: Time -> a -> Channel a -> IO Time
  send time ch val = do 
    _ <- atomicModifyChannel ch sendStates (\_chan ->
      (_putChanValue val (_putSendTime time ch), ()))
    -- Now we must wait until the state is changed. We need some sort of
    -- busy-loop over atomicModifyChannel.
    fix (\f -> 
      atomicModifyChannel (\_chan ->
        
      )
    )
    

  send :: Time -> Channel a -> a -> IO Time
  send time ch val = do
    state <- getState ch
    case state of
      Consistent -> do -- Update send time, wait for CommAccept state
        putSendTime time ch
        state <- getState ch
        case state of
          CommAccept -> 
          RecvBacktrack -> 
        
        
      RecvRequest -> -- Update send and chan time, return
      RecvBacktrack -> -- We need to backtrack
      -- else, it's an error
      _ -> error "Error in send: inconsistent state " ++ state 



  recv :: Channel a -> IO (a, Time)

