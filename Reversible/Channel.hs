-- Defines a zero buffer channel, which can store a value, and three
-- logical times.
module Reversible.Channel (
  ) where
  import Control.Monad.STM
  import Control.Concurrent.STM.TMVar

  type Time = Int

  data TimeStamp = Times { 
    senderTime :: TMVar Time,
    receiverTime :: TMVar Time,
    channelTime :: TMVar Time
  }

  data Channel a = Channel {
    channelValue :: TMVar a,
    channelTimeStamp :: TimeStamp,
    channelRecvAck :: TMVar ()
    channelSyncAck :: TMvar ()
  }

  -- To read off the channel, first we read the value. Next, we get the
  -- max of the incoming timestamp, the receiver timestamp on the
  -- channel, and the channels timestamp, and update the times on the
  -- channel. Then, we set the acknowledge tag so the sender can updat
  -- the times. Once the sync tag is set, we return the value and the
  -- time.
  readCh :: Time -> Channel a -> STM (a, Time)
  readCh time ch = do
    val <- readTMVar $ channelValue ch

    let timeStamp = channelTimeStamp ch
    recvT <- takeTMVar $ receiverTime timeStamp 
    chT <- takeTMVar $ channelTime timeStamp 
    let newT = (+1) $ foldr1 max [time,recvT,chT]
    putTMVar (receiverTime timeStamp)  newT
    putTMVar (channelTime timeStamp) newT

    putTMVar (channelAck ch) ()
    takeTMVar $ channelSyncAck ch

    return (a, readTMVar $ receiverTime timeStamp)

  writeCh :: Time -> TMVar (a, TimeStamp) -> a -> STM (a, Time)
  writeCh time ch val = do 
    putTMVar $ channelValue ch

    takeTMVar $ channelAck ch

    

  newChannel :: a -> Channel a
  newChannel a = newTMVar a

  newEmptyChannel :: STM (Channel a)
  newEmptyChannel = newEmptyTMVar 

  sendChannel :: Channel a -> STM ()
  sendChannel ch = do
    var <- ch
    readCh var

  recvChannel :: Channel a -> STM a





  getValue :: Channel a -> STM a
  getValue ch = fmap fst (fmap readCh ch)

  getInfo :: Channel a b -> STM b
  getInfo ch = fmap snd (fmap readCh ch)

  modifyChannel :: Channel a b -> ((a, b) -> (a, b)) -> STM ()
  modifyChannel ch f = do
    var <- ch
    pair <- readCh var
    writeCh var $ f pair

  setValue :: Channel a b -> a -> STM ()
  setValue ch val = modifyChannel ch (\ (_,b) -> (val, b))

  setInfo :: Channel a b -> b -> STM ()
  setInfo ch info = modifyChannel ch (\ (a,_) -> (a,info))

