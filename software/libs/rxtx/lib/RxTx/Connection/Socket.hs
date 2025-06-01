module RxTx.Connection.Socket where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Serialize (Serialize)
import Data.ByteString qualified as B
import Network.Socket (
  AddrInfo (..),
  HostName,
  ServiceName,
  Socket,
  SocketType (Stream),
  connect,
  defaultHints,
  getAddrInfo,
  openSocket, close,
 )
import RxTx.Connection (Connection(..), mkConnection)
import RxTx.Socket (SocketRxError(..), SocketTxError)

mkAddrInfo :: HostName -> ServiceName -> IO (Maybe AddrInfo)
mkAddrInfo host port = do
  let hints = defaultHints {addrSocketType = Stream}
  results <- getAddrInfo (Just hints) (Just host) (Just port)
  case results of
    (addr : _) -> pure $ Just addr
    [] -> pure Nothing

-- | Create a socket for connection to a server. The Socket has not
-- tried to connect yet.
aquireClientSocket :: HostName -> ServiceName -> IO (Maybe (Socket, AddrInfo))
aquireClientSocket host port = do
  mAddrInfo <- mkAddrInfo host port
  case mAddrInfo of
    Nothing -> pure Nothing
    Just addrInfo -> do
      sock <- openSocket addrInfo
      pure $ Just (sock, addrInfo)

-- | Will try and connect to the host, blocking unti the connection is established.
connectToHost :: Socket -> AddrInfo -> IO ()
connectToHost sock addrInfo = do
  safeConnect
 where
  safeConnect =
    tryConnect `catch` handleConnectionFail

  tryConnect = do
    connect sock (addrAddress addrInfo)

  handleConnectionFail (_ :: IOException) = do
    putStrLn $
      "Could not connect to "
        <> show (addrAddress addrInfo)
        <> ". Trying again in 2s..."
    threadDelay 2000000
    safeConnect

createConnection ::
  forall m msg.
  (MonadIO m, Serialize msg) =>
  HostName
  -> ServiceName
  -> m (Maybe (Connection m msg SocketRxError SocketTxError))
createConnection host port = do
  mSockPair <- liftIO $ aquireClientSocket host port
  case mSockPair of
    Nothing -> pure Nothing
    Just (sock, addrInfo) -> do
      liftIO $ connectToHost sock addrInfo
      Just <$> mkConnection sock (liftIO . close)

foo :: IO ()
foo = do
  Just conn <- createConnection @IO @B.ByteString "127.0.0.1" "8080"
  addSub conn $  \bytes -> print bytes
  run conn
  where
    run conn = do
      merror <- recvAndDispatch conn
      case merror of
        Nothing -> run conn
        Just (SocketRxError _ err) -> do
          print $ "ERROR: " <> show err
          cleanup conn
