{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TCP where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Serialize (Serialize, getWord32be)
import Data.Serialize.Get (runGet)
import HAsio.Async.IO (RecvResult (..), asyncRecv)
import HAsio.Async.Reactor (Reactor)
import HAsio.Control (bracketOnError)
import HAsio.Error.Error (ECat, Error (..))
import HAsio.Error.ErrorCategory (ErrorCategory (..))
import HAsio.Error.ErrorStack (ErrorStack, makeErrorStack)
import HAsio.Fd.Socket (Socket, ownNetworkSocket)
import Network.Socket (
  AddrInfo (..),
  AddrInfoFlag (AI_PASSIVE),
  HostName,
  ServiceName,
  SocketOption (..),
  SocketType (..),
  bind,
  defaultHints,
  getAddrInfo,
  listen,
  openSocket,
  setSocketOption,
 )
import Network.Socket qualified as Network

data TCPErrorCategory

instance ErrorCategory TCPErrorCategory where
  getErrorCategoryName = "tcp-error"

data TCPError
  = FailedToExtractAddrInfo
  | FailedToParseMessageSize

instance Error TCPError where
  type ECat TCPError = TCPErrorCategory
  getErrorMessage err =
    case err of
      FailedToExtractAddrInfo -> "failed to extract address info"
      FailedToParseMessageSize -> "failed to parse message size"

-- TODO getAddrInfo can throw an IOError.
mkAddrInfo :: HostName -> ServiceName -> ExceptT ErrorStack IO AddrInfo
mkAddrInfo host port = do
  let hints = defaultHints {addrSocketType = Stream}
  results <- liftIO $ getAddrInfo (Just hints) (Just host) (Just port)
  case results of
    (addr : _) -> pure addr
    [] -> makeErrorStack FailedToExtractAddrInfo

mkServerAddrInfo :: ServiceName -> ExceptT ErrorStack IO AddrInfo
mkServerAddrInfo port = do
  let hints = defaultHints {addrSocketType = Stream, addrFlags = [AI_PASSIVE]}
  results <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
  case results of
    (addr : _) -> pure addr
    [] -> makeErrorStack FailedToExtractAddrInfo

aquireClientSocket ::
  HostName -> ServiceName -> ExceptT ErrorStack IO (Socket, AddrInfo)
aquireClientSocket host port = do
  addrInfo <- mkAddrInfo host port
  sock <- liftIO $ openSocket addrInfo
  liftIO $ Network.connect sock $ addrAddress addrInfo
  fdSock <- liftIO $ ownNetworkSocket sock
  pure (fdSock, addrInfo)

aquireBoundListeningServerSocket ::
  ServiceName
  -> ExceptT ErrorStack IO Socket
aquireBoundListeningServerSocket port = do
  addrInfo <- mkServerAddrInfo port
  sock <-
    liftIO $
      Network.socket
        (addrFamily addrInfo)
        (addrSocketType addrInfo)
        (addrProtocol addrInfo)
  liftIO $ setSocketOption sock ReuseAddr 1
  liftIO $ bind sock (addrAddress addrInfo)
  liftIO $ listen sock 1
  liftIO $ putStrLn $ "Listening on port " <> port <> " for TCP connections..."
  liftIO $ ownNetworkSocket sock

headerSize :: Int
headerSize = 4

-- | Subscribes to a socket and builds up a full message of
-- bytes. Applies handler to the bytes when a full message has been
-- received.
asyncRecvHandler ::
  Reactor
  -> Socket
  -> (B.ByteString -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO ()
asyncRecvHandler reactor socket handleBytes = do
  bytesRef <- liftIO $ newIORef (Nothing, mempty @B.ByteString)

  let
    handleRecvEvent dereg RecvClosed = dereg
    handleRecvEvent _ (RecvData bytes) = do
      (maybeIncomingMsgLen, acc) <- liftIO $ readIORef bytesRef
      let newAcc = acc <> bytes
      case maybeIncomingMsgLen of
        Nothing -> setIncomingMessageLen newAcc
        Just msgLen -> handleData msgLen newAcc

    setIncomingMessageLen bytes = do
      if B.length bytes < headerSize
        then liftIO $ writeIORef bytesRef (Nothing, bytes)
        else do
          let (hdrBytes, remBytes) = B.splitAt headerSize bytes
          case runGet getWord32be hdrBytes of
            Left _ -> makeErrorStack FailedToParseMessageSize
            Right msgSize -> do
              liftIO $ writeIORef bytesRef (Just msgSize, remBytes)
              handleData msgSize remBytes

    handleData msgLen bytes
      | msgLen <= fromIntegral (B.length bytes) = do
          let (msgBytes, remBytes) = B.splitAt (fromIntegral msgLen) bytes
          liftIO $ writeIORef bytesRef (Nothing, remBytes)
          handleBytes msgBytes
      | otherwise = do
          liftIO $ writeIORef bytesRef (Just msgLen, bytes)

  void $ asyncRecv reactor socket handleRecvEvent
