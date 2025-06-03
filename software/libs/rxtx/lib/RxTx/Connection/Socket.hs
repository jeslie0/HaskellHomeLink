module RxTx.Connection.Socket where

import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Network.Socket (
  AddrInfo (..),
  AddrInfoFlag (AI_PASSIVE),
  HostName,
  ServiceName,
  Socket,
  SocketOption (..),
  SocketType (Stream),
  accept,
  bind,
  close,
  connect,
  defaultHints,
  getAddrInfo,
  listen,
  openSocket,
  setSocketOption,
  socket,
 )
import RxTx (RxTx)
import RxTx.Connection (Connection (..), mkConnection)
import RxTx.Socket (SocketRxError (..), SocketTxError)

mkAddrInfo :: HostName -> ServiceName -> IO (Maybe AddrInfo)
mkAddrInfo host port = do
  let hints = defaultHints {addrSocketType = Stream}
  results <- getAddrInfo (Just hints) (Just host) (Just port)
  case results of
    (addr : _) -> pure $ Just addr
    [] -> pure Nothing

-- | Create a socket for connecting to a server. The Socket has not
-- tried to connect yet.
aquireClientSocket :: HostName -> ServiceName -> IO (Maybe (Socket, AddrInfo))
aquireClientSocket host port = do
  mAddrInfo <- mkAddrInfo host port
  case mAddrInfo of
    Nothing -> pure Nothing
    Just addrInfo -> do
      sock <- openSocket addrInfo
      pure $ Just (sock, addrInfo)

mkServerAddrInfo :: ServiceName -> IO AddrInfo
mkServerAddrInfo port = do
  let hints = defaultHints {addrSocketType = Stream, addrFlags = [AI_PASSIVE]}
  results <- getAddrInfo (Just hints) Nothing (Just port)
  case results of
    (addr : _) -> return addr
    [] -> ioError $ userError "No address information found"

aquireBoundListeningServerSocket ::
  ServiceName
  -> IO Socket
aquireBoundListeningServerSocket port = do
  addrInfo <- mkServerAddrInfo port
  sock <-
    socket (addrFamily addrInfo) (addrSocketType addrInfo) (addrProtocol addrInfo)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addrInfo)
  listen sock 1
  putStrLn $ "Listening on port " <> port <> " for TLS connections..."
  pure sock

-- | Given a server socket, accept an incoming connection and return
-- it. The server socket is still active and can be used later.
getNewClientSocket ::
  Socket
  -- ^ Server socket.
  -> IO Socket
getNewClientSocket sock = do
  (client, peer) <- accept sock
  putStrLn $ "Accepted connection from " <> show peer
  pure client

getNewClientConnection ::
  RxTx msg Socket SocketRxError SocketTxError =>
  Socket
  -- ^ Server socket.
  -> IO (Connection msg SocketRxError SocketTxError)
getNewClientConnection sock = do
  client <- getNewClientSocket sock
  mkConnection client (liftIO . close)

-- | Will try and connect to the host, blocking unti the connection is established.
connectToHost :: Socket -> AddrInfo -> IO Bool
connectToHost sock addrInfo = do
  safeConnect
 where
  safeConnect =
    tryConnect `catch` handleConnectionFail

  tryConnect = do
    connect sock (addrAddress addrInfo)
    pure True

  handleConnectionFail (_ :: IOException) =
    pure False

createClientSocket ::
  HostName
  -> ServiceName
  -> IO (Maybe Socket)
createClientSocket host port = do
  mSockPair <- liftIO $ aquireClientSocket host port
  case mSockPair of
    Nothing -> pure Nothing
    Just (sock, addrInfo) -> do
      bool <- connectToHost sock addrInfo
      pure $ if bool then Just sock else Nothing

createClientConnection ::
  RxTx msg Socket SocketRxError SocketTxError =>
  HostName
  -> ServiceName
  -> IO (Maybe (Connection msg SocketRxError SocketTxError))
createClientConnection host port = do
  mSocket <- createClientSocket host port
  case mSocket of
    Nothing -> pure Nothing
    Just sock ->
      Just <$> mkConnection sock close

-- foo :: IO ()
-- foo = do
--   Just conn <- createConnection @B.ByteString "127.0.0.1" "8080"
--   addSub conn $  \bytes -> print bytes
--   run conn
--   where
--     run conn = do
--       merror <- recvAndDispatch conn
--       case merror of
--         Nothing -> run conn
--         Just (SocketRxError _ err) -> do
--           print $ "ERROR: " <> show err
--           cleanup conn
