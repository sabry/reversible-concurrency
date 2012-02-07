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
    | SendRequest 
    | RecvRequest
    | CommAccept
    | RecvBacktrack
    | SendBacktrack
    | BacktrackAccept 
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

  
  _putSendTime :: Time -> _Channel a -> _Channel a
  _putSendTime time ch = _Channel {chanValue ch, time, chanTime ch, recvTime ch}

  _putRecvTime :: Time -> _Channel a -> _Channel a
  _putRecvTime time ch = _Channel {chanValue ch, sendTime ch, chanTime ch, time}

  _putChanTime :: Time -> _Channel a -> _Channel a
  _putChanTime time ch = _Channel {chanValue ch, sendTime ch, time, recvTime ch}

  _putChanValue :: a -> _Channel a -> _Channel a
  _putChanValue val ch = _Channel {val, sendTime ch, chanTime ch, recvTime ch}

  putSendTime :: Time -> Channel a -> IO ()
  putSendTime time = (flip atomicModifyIORef) $ (\ch -> (_putSendTime time ch, ()))

  putRecvTime :: Time -> Channel a -> IO ()
  putRecvTime time = (flip atomicModifyIORef) $ (\ch -> (_putRecvTime time ch, ()))

  putChanTime :: Time -> Channel a -> IO ()
  putChanTime time = (flip atomicModifyIORef) $ (\ch -> (_putChanTime time ch, ()))

  putChanValue :: a -> Channel a -> IO ()
  putChanValue val = (flip atomicModifyIORef) $ (\ch -> (_putChanValue val ch, ()))

  getSendTime :: Channel a -> IO Time
  getSendTime = sendTime <$> readIORef 

  getRecvTime :: Channel a -> IO Time
  getRecvTime = recvTime <$> readIORef

  getChanTime :: Channel a -> IO Time
  getChanTime = chanTime <$> readIORef

  getChanValue :: Channel a -> IO a
  getChanValue = chanValue <$> readIORef

  newChannel :: a -> IO (Channel a)
  newChannel a = newIORef Channel {
    chanValue = a,
    sendTime = baseTime,
    chanTime = baseTime,
    recvTime = baseTime
    }

  newEmptyChannel :: IO (Channel a)
  newEmptyChannel =  newChannel undefined

  sendBacktrack :: Time -> Channel a -> IO () 
  sendBacktrack time ch = do
    cTime <- getChanTime
    if (not $ time < cTime)
      -- A sendBacktrack must put the send time before the current channel
      -- time
      then error "Error in sendBacktrack: given time is not" ++
        " less than the channel time"
      else putSendTime time ch

  recvBacktrack :: Time -> Channel a -> IO ()
  recvBacktrack time ch = do
    cTime <- getChanTime
    if (not $ time < cTime)
      -- A recvBacktrack must put the recv time before the current channel
      -- time
      then error "Error in recvBacktrack: given time is not" ++
        " less than the channel time"
      else putRecvTime time ch
  

  -- Given a current state and a state we want to be in, return a
  -- function that will move to that state.
  transition :: ChannelState -> ChannelState -> Channel a -> Channel a
  transition cur tar = 
    let bad = error "Invalid transition: " ++ (show cur) ++ " -> " ++ (show tar)
    in
    case cur of
      Consistent ->
        case tar of 
          SendRequest 
          RecvRequest
          RecvBacktrack
          SendBacktrack
          _ -> bad

      SendRequest -> 
        case tar of
          CommAccept
          RecvBacktrack
          _ -> bad

      RecvRequest ->
        case tar of 
          CommAccept
          SendBacktrack
          _ -> bad

      CommAccept ->
        case tar of 
          Consistent 
            -- since transitioning to consistent is the senders job, the
            -- receiver might immediately make a request
          RecvBacktrack
          RecvRequest
          _ -> bad

      RecvBacktrack ->
        case tar of
          BacktrackAccept 
          _ -> bad

      SendBacktrack ->
        case tar of
          BacktrackAccept 
          _ -> bad

      BacktrackAccept ->
       case tar of 
          Consistent 
            -- since transitioning to consistent is the
            -- senders job, the receiver might immediately make a request
          RecvRequest 
          RecvBacktrack
          _ -> bad

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

