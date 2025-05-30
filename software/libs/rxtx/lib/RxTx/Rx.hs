module RxTx.Rx (
  Rx (..),
  Socket.SocketRxError,
  TLS.TLSRxError,
) where

import Control.Concurrent.Chan qualified as Chan
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (get))
import Data.Serialize.Get (runGet)
import Data.Text qualified as T
import GHC.IO.Exception (IOException (IOError, ioe_errno))
import Network.Socket (Socket)
import Network.TLS (TLSException)
import Network.TLS qualified as TLS
import RxTx.Socket qualified as Socket
import RxTx.TLS qualified as TLS

class Rx msg chan err where
  recvMsg :: MonadIO m => chan -> m (Either err msg)

instance Serialize msg => Rx msg Socket Socket.SocketRxError where
  recvMsg sock = do
    liftIO $
      runRecv
        `catch` ( \(IOError {ioe_errno} :: IOException) -> pure . Left . Socket.RxIOError $ fromMaybe (-1) ioe_errno
                )
   where
    runRecv = do
      eBytes <- Socket.runRecvUnsafe sock
      case eBytes of
        Left err -> pure $ Left err
        Right bytes ->
          case runGet get bytes of
            Left err -> pure . Left $ Socket.FailedToParseBody (T.pack err)
            Right (msg :: msg) -> pure $ Right msg

instance Serialize msg => Rx msg TLS.Context TLS.TLSRxError where
  recvMsg ctx = do
    liftIO $
      runRecv
        `catch` ( \(IOError {ioe_errno} :: IOException) -> pure . Left . TLS.IOException $ fromMaybe (-1) ioe_errno
                )
        `catch` ( \(tlsExc :: TLSException) -> pure . Left . TLS.RxTLSError $ tlsExc
                )
   where
    runRecv = do
      bytes <- TLS.recvData ctx
      if B.null bytes
        then pure $ Left TLS.ConnectionClosed
        else case runGet get bytes of
          Left err -> pure . Left $ TLS.FailedToParseBody (T.pack err)
          Right (msg :: msg) -> pure $ Right msg

data RxChanError

instance Rx msg (Chan.Chan msg) RxChanError where
  recvMsg chan = liftIO $ Right <$> Chan.readChan chan
