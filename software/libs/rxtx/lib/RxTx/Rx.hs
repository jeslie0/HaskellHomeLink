{-# LANGUAGE AllowAmbiguousTypes #-}
module RxTx.Rx (
  Rx (..),
  Socket.SocketRxError,
  TLS.TLSRxError,
) where

import Control.Concurrent.Chan qualified as Chan
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Serialize (Serialize, decode)
import Data.Text qualified as T
import Network.Socket (Socket)
import Network.TLS qualified as TLS
import RxTx.Internal (recvMsgSock, recvMsgTLS)
import RxTx.Socket qualified as Socket
import RxTx.TLS qualified as TLS

class Rx msg chan err where
  recvMsg :: MonadIO m => chan -> m (Either err msg)
  showRxErr :: err -> String

-- * Socket

instance {-# OVERLAPPABLE #-} Serialize msg => Rx msg Socket Socket.SocketRxError where
  showRxErr = show
  recvMsg sock = recvMsgSock withBytes sock
   where
    withBytes bytes =
      case decode bytes of
        Left err ->
          Left . Socket.SocketRxError sock $ Socket.FailedToParseBody (T.pack err)
        Right (msg :: msg) ->
          Right msg

instance {-# OVERLAPPING #-} Rx B.ByteString Socket Socket.SocketRxError where
  recvMsg = recvMsgSock Right
  showRxErr = show

-- * TLS

instance {-# OVERLAPPABLE #-} Serialize msg => Rx msg TLS.Context TLS.TLSRxError where
  showRxErr = show
  recvMsg = recvMsgTLS withBytes
   where
    withBytes bytes =
      case decode @msg bytes of
        Left err -> Left . TLS.TLSRxError $ TLS.FailedToParseBody (T.pack err)
        Right (msg :: msg) -> Right msg

instance {-# OVERLAPPING #-} Rx B.ByteString TLS.Context TLS.TLSRxError where
  recvMsg = recvMsgTLS Right
  showRxErr = show

-- * Channels

data RxChanError

instance Rx msg (Chan.Chan msg) RxChanError where
  showRxErr _ = ""
  recvMsg chan = liftIO $ Right <$> Chan.readChan chan
