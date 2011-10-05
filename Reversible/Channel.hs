-- Defines a zero buffer channel, which can store a value, and three
-- logical times.
module Reversible.Channel (
  send,
  recv,
  newChannel,
  newEmptyChannel,
  Time,
  Channel,
  TimeStamp
  ) where
  import Control.Monad.STM
  import Control.Concurrent.STM.TMVar
  import Reversible.LogicalTime 

  data TimeStamp = TimeStamp { 
    senderTime :: TMVar Time,
    receiverTime :: TMVar Time,
    channelTime :: TMVar Time
  }

  data Channel a = Channel {
    channelValue :: TMVar a,
    channelTimeStamp :: TimeStamp,
    channelRecvAck :: TMVar (),
    channelSyncAck :: TMVar ()
  }

  -- To read off the channel, first we read the value. Next, we get the
  -- max of the incoming timestamp, the receiver timestamp on the
  -- channel, and the channels timestamp, and update the times on the
  -- channel. Then, we set the acknowledge tag so the sender can updat
  -- the times. Once the sync tag is set, we return the value and the
  -- time.
  recv :: Time -> Channel a -> STM (a, Time)
  recv time ch = do
    val <- readTMVar $ channelValue ch

    let timeStamp = channelTimeStamp ch
    rTime <- takeTMVar $ receiverTime timeStamp 
    cTime <- takeTMVar $ channelTime timeStamp 
    let nTime = foldr1 max [time,rTime,cTime]
    putTMVar (receiverTime timeStamp)  nTime
    putTMVar (channelTime timeStamp) nTime

    putTMVar (channelRecvAck ch) ()
    takeTMVar $ channelSyncAck ch
    time <- readTMVar $ receiverTime timeStamp
    return (val, time)

  -- Write the value to the channel, then wait for an acknowledgment.
  -- After that, get the max of the incoming time and the channel time
  -- (which should be the same as the receiver's time as well), add 1 to
  -- it, and update all the times. Then, set the sync tag.
  send :: Time -> Channel a -> a -> STM Time
  send time ch val = do 
    putTMVar (channelValue ch) val

    takeTMVar $ channelRecvAck ch

    let timeStamp = channelTimeStamp ch
    cTime <- takeTMVar $ channelTime timeStamp
    let nTime = incTime $ max time cTime
    putTMVar (receiverTime timeStamp) nTime
    putTMVar (channelTime timeStamp) nTime
    putTMVar (senderTime timeStamp) nTime

    putTMVar (channelSyncAck ch) ()

    return nTime

  _newChannel :: STM (TMVar a) -> STM (Channel a)
  _newChannel f =  do 
    val <- f
    sTime <- newTMVar $ baseTime
    rTime <- newTMVar $ baseTime
    cTime <- newTMVar $ baseTime
    let timeStamp = TimeStamp {
      senderTime = sTime, 
      receiverTime = rTime, 
      channelTime = cTime}
    recv <- newEmptyTMVar
    sync <- newEmptyTMVar
    return $ Channel {
      channelValue = val, 
      channelTimeStamp = timeStamp,
      channelRecvAck = recv, 
      channelSyncAck = sync}

  newChannel :: a -> STM (Channel a)
  newChannel a = _newChannel $ newTMVar a

  newEmptyChannel :: STM (Channel a)
  newEmptyChannel = _newChannel newEmptyTMVar
