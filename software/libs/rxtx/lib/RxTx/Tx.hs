module RxTx.Tx (
  Tx (..),
  Socket.SocketTxError,
  TLS.TLSTxError,
) where

import Control.Concurrent.Chan qualified as Chan
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (put))
import Data.Serialize.Put (putWord32le, runPut)
import Data.Word (Word32)
import GHC.IO.Exception (IOException (IOError, ioe_errno))
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as Socket
import Network.TLS (TLSException)
import Network.TLS qualified as TLS
import RxTx.Socket qualified as Socket
import RxTx.TLS qualified as TLS

class Tx msg chan err where
  sendMsg :: MonadIO m => chan -> msg -> m (Either err ())

instance Serialize msg => Tx msg Socket Socket.SocketTxError where
  sendMsg sock msg = do
    liftIO $
      catch
        runSend
        ( \(IOError {ioe_errno} :: IOException) -> pure . Left . Socket.TxIOError $ fromMaybe (-1) ioe_errno
        )
   where
    runSend =
      let
        body = runPut (put msg)
        len :: Word32 = fromIntegral $ B.length body
      in
        do
          Socket.sendAll sock (runPut . putWord32le $ len)
          Socket.sendAll sock body
          pure $ Right ()

instance Serialize msg => Tx msg TLS.Context TLS.TLSTxError where
  sendMsg ctx msg = do
    liftIO $
      runSend
        `catch` ( \(IOError {ioe_errno} :: IOException) -> pure . Left . TLS.TLSTxError ctx . TLS.TxIOError $ fromMaybe (-1) ioe_errno
                )
        `catch` ( \(tlsExc :: TLSException) -> pure . Left . TLS.TLSTxError ctx . TLS.TxTLSError $ tlsExc
                )
   where
    runSend = do
      TLS.sendData ctx (B.fromStrict . runPut . put $ msg)
      pure $ Right ()

data TxChanError

instance Tx msg (Chan.Chan msg) TxChanError where
  sendMsg chan msg = liftIO $ Right <$> Chan.writeChan chan msg
