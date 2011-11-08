-- Defines a zero buffer channel, which can store a value, and three
-- logical times.
module Reversible.Channel (
  send,
  recv,
  newChannel,
  newEmptyChannel,
  Time,
  Channel,
  TimeStamp,
  traceTimeStamp,
  traceChannel,
  getSendTime,
  getRecvTime,
  getChanTime,
  -- I'm not sure these three should be exposed... well, at least not
  -- without more restriction. A thread should only be allowed to edit
  -- it's time, and not the others.
  modifySendTime,
  modifyRecvTime,
  modifyChanTime
  ) where

  import CHD.Control.Concurrent
  import Reversible.LogicalTime 
  import Reversible.Debug

  data TimeStamp = TimeStamp { 
    senderTime :: MVar Time,
    receiverTime :: MVar Time,
    channelTime :: MVar Time
  }
  
  traceTimeStamp :: Int -> TimeStamp -> IO ()
  traceTimeStamp i st = do
    sTime <- readMVar $ senderTime st
    rTime <- readMVar $ receiverTime st
    cTime <- readMVar $ channelTime st
    traceM i $ "Channel.traceTimeStamp senderTime: " ++ (show sTime)
    traceM i $ "Channel.traceTimeStamp receiverTime: " ++ (show rTime)
    traceM i $ "Channel.traceTimeStamp channelTime: " ++ (show cTime)

  data Channel a = Channel {
    channelValue :: MVar a,
    channelTimeStamp :: TimeStamp,
    channelRecvAck :: MVar (),
    channelSyncAck :: MVar ()
  }
  
  traceChannel :: Show a => Int -> Channel a -> IO ()
  traceChannel i ch = do
    val <- readMVar $channelValue ch
    traceM i $ "Channel.traceChannel val: " ++ (show val)
    traceM i $ "Channel,traceChannel timetstamp: "
    traceTimeStamp i $ channelTimeStamp ch

  -- To read off the channel, first we read the value. Next, we get the
  -- max of the incoming timestamp, the receiver timestamp on the
  -- channel, and the channels timestamp, and update the times on the
  -- channel. Then, we set the acknowledge tag so the sender can updat
  -- the times. Once the sync tag is set, we return the value and the
  -- time.
  recv :: Time -> Channel a -> IO (a, Time)
  recv time ch = do

    traceM 2 $ "Channel.recv time: " ++ (show time)

    val <- takeMVar $ channelValue ch

    traceM 2 $ "Channel.recv value received"

    --rTime <- takeMVar $ receiverTime timeStamp 
    --let nTime = max time cTime
    --putMVar (receiverTime timeStamp)  nTime
    modifyChanTime (\cTime -> return $ max time cTime) ch

    traceM 2 $ "Channel.recv timestamp updated"

    putMVar (channelRecvAck ch) ()

    traceM 2 $ "Channel.recv acknowledgement sent"

    takeMVar $ channelSyncAck ch

    traceM 2 $ "Channel.recv sync received"

    time <- getRecvTime ch
    return (val, time)

  -- Write the value to the channel, then wait for an acknowledgment.
  -- After that, get the max of the incoming time and the channel time
  -- (which should be the same as the receiver's time as well), add 1 to
  -- it, and update all the times. Then, set the sync tag.
  send :: Time -> Channel a -> a -> IO Time
  send time ch val = do 
    
    traceM 2 $ "Channel.send time: " ++ (show time)

    putMVar (channelValue ch) val

    traceM 2 $ "Channel.send put value"

    takeMVar $ channelRecvAck ch

    traceM 2 $ "Channel.send acknowledgement received"

    cTime <- getChanTime ch-- takeMVar $ channelTime timeStamp
    --_ <- takeMVar $ receiverTime timeStamp
    --_ <- takeMVar $ senderTime timeStamp

    let nTime = incTime $ max time cTime
    modifyChanTime (\_ -> return nTime) ch
    modifySendTime (\_ -> return nTime) ch
    modifyRecvTime (\_ -> return nTime) ch

    traceM 2 $ "Channel.send timestamp updated"

    putMVar (channelSyncAck ch) ()

    traceM 2 $ "Channel.send sync sent"

    return nTime

  _newChannel :: IO (MVar a) -> IO (Channel a)
  _newChannel f =  do 
    val <- f
    sTime <- newMVar baseTime
    rTime <- newMVar baseTime
    cTime <- newMVar baseTime
    let timeStamp = TimeStamp {
      senderTime = sTime, 
      receiverTime = rTime, 
      channelTime = cTime}
    recv <- newEmptyMVar
    sync <- newEmptyMVar
    return $ Channel {
      channelValue = val, 
      channelTimeStamp = timeStamp,
      channelRecvAck = recv, 
      channelSyncAck = sync}

  newChannel :: a -> IO (Channel a)
  newChannel a = _newChannel (newMVar a) 

  newEmptyChannel :: IO (Channel a)
  newEmptyChannel = _newChannel newEmptyMVar

  _getTime :: (TimeStamp -> MVar Time) -> Channel a -> IO Time
  _getTime f = readMVar . f . channelTimeStamp

  _modifyTime :: (TimeStamp -> MVar Time) -> (Time -> IO Time) -> Channel a -> IO ()
  _modifyTime f1 f2 ch = modifyMVar_ (f1 $ channelTimeStamp ch) f2

  getRecvTime :: Channel a -> IO Time
  getRecvTime = _getTime receiverTime

  modifyRecvTime :: (Time -> IO Time) -> Channel a -> IO ()
  modifyRecvTime f = _modifyTime receiverTime f

  getSendTime :: Channel a -> IO Time
  getSendTime = _getTime senderTime 

  modifySendTime :: (Time -> IO Time) -> Channel a -> IO ()
  modifySendTime f = _modifyTime senderTime f 

  getChanTime :: Channel a -> IO Time
  getChanTime = _getTime channelTime 

  modifyChanTime :: (Time -> IO Time) -> Channel a -> IO ()
  modifyChanTime f = _modifyTime channelTime f

