{-# LANGUAGE MonoLocalBinds #-}

module Port where

import Data.ByteString qualified as B
import Data.Serialize qualified as Binary
import Msg (Msg (..))
import Network.Socket (Socket)
import Socket (recvNBytes, sendBytes, readHeader)

-- | A Port represents a type that can receive and send messages. We
-- should think of it as a network connection, or a send/recv channel.
class Port a where
  recvMsg :: forall m. (Msg m) => a -> IO (Either String m)

  sendMsg :: forall m. (Msg m) => a -> m -> IO ()


instance Port Socket where
  recvMsg sock = do
    n <- readHeader sock
    msgBytes <- recvNBytes sock . fromIntegral $ n
    pure $ fromBytes msgBytes

  sendMsg sock msg =
    let bytes = toBytes msg
        hdr = Binary.runPut $ Binary.putWord32le (fromIntegral $ B.length bytes)
     in sendBytes sock hdr >> sendBytes sock bytes

