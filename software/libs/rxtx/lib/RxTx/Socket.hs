module RxTx.Socket (
  RxError (..),
  SocketRxError (..),
  SocketTxError (..),
  recvNBytes,
  readHeader,
  socketHdrSize,
  runRecvUnsafe,
) where

import Data.ByteString qualified as B
import Data.Serialize qualified as Binary
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as Socket

data RxError
  = InsufficientHeader
  | FailedToGetBody
  | FailedToParseBody T.Text
  | ConnectionClosed
  | RxIOError CInt
  deriving Show

data SocketRxError = SocketRxError Socket RxError

newtype SocketTxError = TxIOError CInt

socketHdrSize :: Int
socketHdrSize = 4

recvNBytes :: Socket -> Int -> IO (Either RxError B.ByteString)
recvNBytes sock n = do
  go n B.empty
 where
  go 0 acc = pure $ Right acc
  go m acc = do
    bytes <- Socket.recv sock m
    print "got bytes!"
    if B.length bytes == 0
      then pure $ Left ConnectionClosed
      else
        let bytesToGo = n - B.length bytes
        in go bytesToGo (acc <> bytes)

-- | We need to read 4 bytes from a socket to get the size of the
--  incoming header.
readHeader :: Socket -> IO (Either RxError Word32)
readHeader sock = do
  mHdr <- recvNBytes sock socketHdrSize
  case mHdr of
    Left err -> pure $ Left err
    Right hdr ->
      case Binary.runGet Binary.getWord32le hdr of
        Left str -> pure . Left $ FailedToParseBody (T.pack str)
        Right n -> pure $ Right n

runRecvUnsafe :: Socket -> IO (Either RxError B.ByteString)
runRecvUnsafe sock = do
  mHdr <- readHeader sock
  case mHdr of
    Left err -> pure $ Left err
    Right len -> do
      recvNBytes sock $ fromIntegral len
