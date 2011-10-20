-- Defines a zero buffer channel, which can store a value, and three
-- logical times.
module Reversible.Channel (
  send,
  recv,
  newChannel,
  newEmptyChannel,
  Time,
  Channel,
  -- these 4 methods should not be exposed, but for now...
  channelTimeStamp,
  senderTime,
  receiverTime,
  channelTime,
  TimeStamp,
  ChannelHash,
  saveChannel,
  restoreChannel
  ) where

  import Control.Concurrent.MVar
  import Reversible.LogicalTime 

  data TimeStamp = TimeStamp { 
    senderTime :: MVar Time,
    receiverTime :: MVar Time,
    channelTime :: MVar Time
  }
  
  type TimeStampHash = (Time, Time, Time)
  
  -- Helpers for creating and restoring the channel.
  hashTimeStamp :: TimeStamp -> IO TimeStampHash
  hashTimeStamp t = do
    sTime <- readMVar $ senderTime t
    rTime <- readMVar $ receiverTime t
    cTime <- readMVar $ channelTime t
    return (sTime, rTime, cTime)

  unhashTimeStamp :: TimeStampHash -> IO TimeStamp
  unhashTimeStamp (sTime, rTime, cTime) = do
    sVar <- newMVar sTime
    rVar <- newMVar rTime
    cVar <- newMVar cTime
    return $
      TimeStamp {
        senderTime = sVar,
        receiverTime = rVar,
        channelTime = cVar
      }

  -- Copy t1 into t2
  copyTimeStamp :: TimeStamp -> TimeStamp -> IO ()
  copyTimeStamp t1 t2 = do
    modifyMVar_ (senderTime t2) $ \_ -> (readMVar $ senderTime t1) 
    modifyMVar_ (receiverTime t2) $ \_ -> (readMVar $ receiverTime t1) 
    modifyMVar_ (channelTime t2) $ \_ -> (readMVar $ channelTime t1) 

  data Channel a = Channel {
    channelValue :: MVar a,
    channelTimeStamp :: TimeStamp,
    channelRecvAck :: MVar (),
    channelSyncAck :: MVar ()
  }
  
--  type ChannelHash a = (Maybe a, TimeStampHash)
  type ChannelHash a = TimeStampHash

  hashChannel :: Channel a -> IO (ChannelHash a)
  hashChannel ch = hashTimeStamp $ channelTimeStamp ch
    -- Wait, these are zero buffer channels. The only thing we need to
    -- hash is the timestamp....
--     val <- tryTakeMVar $ channelValue ch  
--     case val of
--       Nothing -> return ()
--       Just v -> putMVar (channelValue ch) v
--     hash <- hashTimeStamp $ channelTimeStamp ch
--     return (val, hash)
  
  unhashChannel :: ChannelHash a -> IO (Channel a)
  unhashChannel hash = do
  -- Remember, zero buffer channel
--    vVar <- case val of 
--             Nothing -> newEmptyMVar
--             Just val -> newMVar val
    vVar <- newEmptyMVar
    rVar <- newEmptyMVar
    timestamp <- unhashTimeStamp hash
    sVar <- newEmptyMVar
    return $ 
      Channel {
        channelValue = vVar,
        channelTimeStamp = timestamp,
        channelRecvAck = rVar,
        channelSyncAck = sVar
      }

  -- Copies the contents of channel 1 into channel 2
  copyChannel :: Channel a -> Channel a -> IO ()
  copyChannel ch1 ch2 = do
  -- Remember, zero buffer channel
--    mval <- tryTakeMVar $ channelValue ch1
--    case mval of
--      Nothing -> do tryTakeMVar $ channelValue ch2; return ()
--      Just v -> do
--        tryTakeMVar $ channelValue ch2
--        putMVar (channelValue ch2) v
    copyTimeStamp (channelTimeStamp ch1) (channelTimeStamp ch2)

  -- To read off the channel, first we read the value. Next, we get the
  -- max of the incoming timestamp, the receiver timestamp on the
  -- channel, and the channels timestamp, and update the times on the
  -- channel. Then, we set the acknowledge tag so the sender can updat
  -- the times. Once the sync tag is set, we return the value and the
  -- time.
  recv :: Time -> Channel a -> IO (a, Time)
  recv time ch = do
    val <- takeMVar $ channelValue ch

    let timeStamp = channelTimeStamp ch
    --rTime <- takeMVar $ receiverTime timeStamp 
    cTime <- takeMVar $ channelTime timeStamp 
    --let nTime = max time cTime
    --putMVar (receiverTime timeStamp)  nTime
    putMVar (channelTime timeStamp) $ max time cTime

    putMVar (channelRecvAck ch) ()

    takeMVar $ channelSyncAck ch
    time <- readMVar $ receiverTime timeStamp
    return (val, time)

  -- Write the value to the channel, then wait for an acknowledgment.
  -- After that, get the max of the incoming time and the channel time
  -- (which should be the same as the receiver's time as well), add 1 to
  -- it, and update all the times. Then, set the sync tag.
  send :: Time -> Channel a -> a -> IO Time
  send time ch val = do 
    putMVar (channelValue ch) val

    takeMVar $ channelRecvAck ch

    let timeStamp = channelTimeStamp ch
    cTime <- takeMVar $ channelTime timeStamp
    --_ <- takeMVar $ receiverTime timeStamp
    --_ <- takeMVar $ senderTime timeStamp

    let nTime = incTime $ max time cTime
    putMVar (channelTime timeStamp) nTime
    modifyMVar_ (receiverTime timeStamp) (\_ -> return nTime)
    modifyMVar_ (senderTime timeStamp) (\_ -> return nTime)

    putMVar (channelSyncAck ch) ()

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
  newChannel a = _newChannel $ newMVar a

  newEmptyChannel :: IO (Channel a)
  newEmptyChannel = _newChannel newEmptyMVar

  _getTime :: (TimeStamp -> MVar Time) -> Channel a -> IO Time
  _getTime f = readMVar . f . channelTimeStamp

  getRecvTime :: Channel a -> IO Time
  getRecvTime = _getTime receiverTime

  getSendTime :: Channel a -> IO Time
  getSendTime = _getTime senderTime 

  getChanTime :: Channel a -> IO Time
  getChanTime = _getTime channelTime 

  saveChannel :: Channel a -> IO (ChannelHash a)
  saveChannel = hashChannel

  restoreChannel :: ChannelHash a -> Channel a -> IO ()
  restoreChannel hash ch = do
    newCh <- unhashChannel hash
    copyChannel newCh ch
