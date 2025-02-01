{-# LANGUAGE MonoLocalBinds #-}

module Connection.Tx (Tx (..)) where

import Control.Concurrent (Chan, writeChan)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as B
import Data.Serialize (putWord32le, runPut)
import Msg (Msg (..))
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as Socket (sendAll)
import Network.TLS (Context, sendData)

class Tx conn msg where
  send :: MonadIO m => conn -> msg -> m ()

instance Tx Socket B.ByteString where
  send sock bytes =
    do
      liftIO . Socket.sendAll sock . runPut . putWord32le . fromIntegral $
        B.length bytes
      liftIO . Socket.sendAll sock $ bytes

instance Msg m => Tx Context m where
  send ctx = liftIO . sendData ctx . B.fromStrict . toBytes

instance Tx (Chan a) a where
  send chan = liftIO . writeChan chan

instance Tx (Chan a, Chan b) b where
  send (_, chan) = liftIO . writeChan chan
