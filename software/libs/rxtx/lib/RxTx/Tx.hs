{-# LANGUAGE AllowAmbiguousTypes #-}

module RxTx.Tx (
  Tx (..),
  Socket.SocketTxError,
  TLS.TLSTxError,
) where

import Control.Concurrent.Chan qualified as Chan
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Serialize (Serialize (put))
import Data.Serialize.Put (runPut)
import Network.Socket (Socket)
import Network.TLS qualified as TLS
import RxTx.Internal (sendMsgSock, sendMsgTLS)
import RxTx.Socket qualified as Socket
import RxTx.TLS qualified as TLS

class Tx msg chan err where
  sendMsg :: MonadIO m => chan -> msg -> m (Either err ())
  showTxErr :: err -> String

-- * Socket

instance {-# OVERLAPPABLE #-} Serialize msg => Tx msg Socket Socket.SocketTxError where
  sendMsg = sendMsgSock (runPut . put)
  showTxErr = show

instance {-# OVERLAPPING #-} Tx B.ByteString Socket Socket.SocketTxError where
  sendMsg = sendMsgSock id
  showTxErr = show

-- * TLS

instance {-# OVERLAPPABLE #-} Serialize msg => Tx msg TLS.Context TLS.TLSTxError where
  sendMsg = sendMsgTLS (runPut . put)
  showTxErr = show

instance {-# OVERLAPPING #-} Tx B.ByteString TLS.Context TLS.TLSTxError where
  sendMsg = sendMsgTLS id
  showTxErr = show

-- * Chan

data TxChanError

instance Tx msg (Chan.Chan msg) TxChanError where
  sendMsg chan msg = liftIO $ Right <$> Chan.writeChan chan msg
  showTxErr err = ""
