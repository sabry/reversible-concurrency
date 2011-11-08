module Reversible.Channel2 (
  send,
  recv, 
  ChannelState,
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
    return $ case compare(sendTime ch, recvTime ch) of
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
  sendBacktrack time ch = atomicModifyIORef ch $  (\ch -> 
    -- A sendBacktrack must put the send time before the current channel
    -- time
      if (not $ time < chanTime ch)
        then error "Error in sendBacktrack: given time is not" ++
          " less than the channel time"
        else Channel {
          chanValue = chanValue ch,
          sendTime = time,
          chanTime = (chanTime ch),
          recvTime = (recvTime ch)
          }
      )

  recvBacktrack :: Time -> Channel a -> IO ()
  recvBacktrack time ch = atmoicModifyIORef ch $ (\ch ->
    if (not $ time < chanTime ch)
        then error "Error in recvBacktrack: given time is not" ++
          " less than the channel time"
        else Channel {
          chanValue = chanValue ch,
          sendTime = (sendTime ch),
          chanTime = (chanTime ch),
          recvTime = time
        }
    )

  send :: Time -> Channel a -> a -> IO Time
  send time ch val = do
    state <- getState ch
    case state of
      Consistent -> -- Update send time, wait for CommAccept state
      RecvRequest -> -- Update send and chan time, return
      RecvBacktrack -> -- We need to backtrack
      -- else, it's an error
      _ -> error "Error in send: inconsistent state " ++ state 



  recv :: Channel a -> IO (a, Time)

