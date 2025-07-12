module RxTx.Internal where

import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Serialize qualified as Binary
import Data.Serialize.Put (putWord32le, runPut)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.IO.Exception (IOException (..))
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as Socket
import Network.TLS (TLSException)
import Network.TLS qualified as TLS
import RxTx.Socket qualified as RxTxSock
import RxTx.TLS qualified as RxTxTLS

socketHdrSize :: Int
socketHdrSize = 4

recvNBytes :: Socket -> Int -> IO (Either RxTxSock.RxError B.ByteString)
recvNBytes sock n = do
  go n B.empty
 where
  go 0 acc = do
    pure $ Right acc
  go m acc = do
    bytes <- Socket.recv sock m
    if B.length bytes == 0
      then pure $ Left RxTxSock.ConnectionClosed
      else
        let bytesToGo = m - B.length bytes
        in go bytesToGo (acc <> bytes)

-- | We need to read 4 bytes from a socket to get the size of the
--  incoming header.
readHeader :: Socket -> IO (Either RxTxSock.RxError Word32)
readHeader sock = do
  mHdr <- recvNBytes sock socketHdrSize
  case mHdr of
    Left err -> pure $ Left err
    Right hdr -> do
      case Binary.runGet Binary.getWord32le hdr of
        Left str -> pure . Left $ RxTxSock.FailedToParseBody (T.pack str)
        Right n -> do
          pure $ Right n

runRecvUnsafe :: Socket -> IO (Either RxTxSock.RxError B.ByteString)
runRecvUnsafe sock = do
  mHdr <- readHeader sock
  case mHdr of
    Left err -> pure $ Left err
    Right len -> do
      recvNBytes sock $ fromIntegral len

recvMsgSock ::
  MonadIO m =>
  (B.ByteString -> Either RxTxSock.SocketRxError b)
  -> Socket
  -> m (Either RxTxSock.SocketRxError b)
recvMsgSock withBytes sock =
  liftIO $
    runRecv
      `catch` ( \(IOError {ioe_errno} :: IOException) ->
                  pure . Left . RxTxSock.SocketRxError sock $
                    RxTxSock.RxIOError $
                      fromMaybe (-1) ioe_errno
              )
 where
  runRecv = do
    eBytes <- runRecvUnsafe sock
    case eBytes of
      Left err -> pure $ Left . RxTxSock.SocketRxError sock $ err
      Right bytes -> do
        pure $ withBytes bytes

sendMsgSock ::
  MonadIO m =>
  (msg -> B.ByteString)
  -> Socket
  -> msg
  -> m (Either RxTxSock.SocketTxError ())
sendMsgSock toBytes sock msg =
  liftIO $
    catch
      runSend
      ( \(IOError {ioe_errno} :: IOException) -> pure . Left . RxTxSock.TxIOError $ fromMaybe (-1) ioe_errno
      )
 where
  runSend =
    let
      body = toBytes msg
      len :: Word32 = fromIntegral $ B.length body
    in
      do
        Socket.sendAll sock (runPut . putWord32le $ len)
        Socket.sendAll sock body
        pure $ Right ()

-- * TLS

recvNBytesTLS :: TLS.Context -> Int -> IO (Either RxTxTLS.RxError B.ByteString)
recvNBytesTLS ctx n = do
  go n B.empty
 where
  go 0 acc = do
    pure $ Right acc
  go m acc = do
    bytes <- TLS.recvData ctx
    if B.length bytes == 0
      then pure $ Left RxTxTLS.ConnectionClosed
      else
        let bytesToGo = m - B.length bytes
        in go bytesToGo (acc <> bytes)

-- | We need to read 4 bytes from a context to get the size of the
--  incoming header.

-- | We need to read 4 bytes from a socket to get the size of the
--  incoming header.
readHeaderTLS :: TLS.Context -> IO (Either RxTxTLS.RxError Word32)
readHeaderTLS ctx = do
  hdrBytes <- TLS.recvData ctx
  case Binary.runGet Binary.getWord32le hdrBytes of
    Left str -> pure . Left $ RxTxTLS.FailedToParseBody (T.pack str)
    Right n -> do
      pure $ Right n

recvMsgTLS ::
  MonadIO m =>
  (B.ByteString -> Either RxTxTLS.TLSRxError b)
  -> TLS.Context
  -> m (Either RxTxTLS.TLSRxError b)
recvMsgTLS withBytes ctx = do
  liftIO $
    runRecv
      `catch` ( \(IOError {ioe_errno} :: IOException) ->
                  pure . Left . RxTxTLS.TLSRxError . RxTxTLS.IOException $
                    fromMaybe (-1) ioe_errno
              )
      `catch` ( \(tlsExc :: TLSException) -> pure . Left . RxTxTLS.TLSRxError . RxTxTLS.RxTLSError $ tlsExc
              )
 where
  runRecv = do
    eHdr <- readHeaderTLS ctx
    case eHdr of
      Left err -> pure . Left . RxTxTLS.TLSRxError $ err
      Right n -> do
        eBytes <- recvNBytesTLS ctx (fromIntegral n)
        case eBytes of
          Left err -> pure . Left . RxTxTLS.TLSRxError $ err
          Right bytes ->
            if B.null bytes
              then pure . Left . RxTxTLS.TLSRxError $ RxTxTLS.ConnectionClosed
              else pure $ withBytes bytes

sendMsgTLS ::
  MonadIO m =>
  (msg -> B.ByteString)
  -> TLS.Context
  -> msg
  -> m (Either RxTxTLS.TLSTxError ())
sendMsgTLS toBytes ctx msg =
  liftIO $
    runSend
      `catch` ( \(IOError {ioe_errno} :: IOException) ->
                  pure . Left . RxTxTLS.TLSTxError . RxTxTLS.TxIOError $
                    fromMaybe (-1) ioe_errno
              )
      `catch` ( \(tlsExc :: TLSException) -> pure . Left . RxTxTLS.TLSTxError . RxTxTLS.TxTLSError $ tlsExc
              )
 where
  runSend =
    let
      body = toBytes msg
      len :: Word32 = fromIntegral $ B.length body
    in
      do
        TLS.sendData ctx (B.fromStrict . runPut . putWord32le $ len)
        TLS.sendData ctx (B.fromStrict . toBytes $ msg)
        pure $ Right ()
