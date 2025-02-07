{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module ConnectionManager (
  ConnectionManager,
  mkConnectionManager,
  removeConnection,
  killConnections,
  initTCPClientConnection,
  initTCPServerConnection,
  trySendMsg,
  addChannelsConnection,
) where

import Connection.Rx (SocketRxError, recv, ChannelRxError)
import Connection.RxTx (RxTx)
import Connection.TCP (
  aquireBoundListeningServerSocket,
  aquireConnectedClientSocket,
 )
import Connection.Tx (Tx (..))
import Control.Concurrent (
  MVar,
  ThreadId,
  forkIO,
  killThread,
  modifyMVar_,
  newMVar,
  threadDelay,
  withMVar, Chan,
 )
import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Islands (Island)
import Msg (Msg (..))
import Network.Socket

data Connection
  = Connection
  { _recvThread :: ThreadId
  , _senderFunc :: Maybe (B.ByteString -> IO ())
  }

newtype ConnectionManager = ConnectionManager
  { connectionsMVar ::
      MVar (Map.Map Island Connection)
  }

addRxTxConnection ::
  forall conn err.
  RxTx conn B.ByteString B.ByteString err =>
  ConnectionManager
  -- ^ Place to store connections
  -> (B.ByteString -> IO ())
  -- ^ Callback to act on received msgs
  -> (conn -> err -> IO ())
  -- ^ Cleanup action called when an error occurs
  -> IO conn
  -- ^ Create RxTx channel
  -> IO ()
  -> IO ()
  -> Island
  -- ^ The corresponding Island
  -> IO ()
addRxTxConnection (ConnectionManager mvar) onRecvMsg onError makeConn onConnect onDisconect island =
  let
    mainThread = do
      conn <- makeConn
      modifyMVar_ mvar $
        Map.alterF
          ( \case
              Nothing -> pure Nothing
              Just (Connection threadId _) ->

                pure . Just $ Connection threadId (Just $ send conn)
          )
          island
      onConnect
      go conn

    go conn = do
      mMsg <- recv conn
      case mMsg of
        Left err -> do
          disableConnection
          onError conn err
        Right msg -> do
          onRecvMsg msg
          go conn

    disableConnection = do
      modifyMVar_ mvar $
        Map.alterF
          ( \case
              Nothing -> pure Nothing
              Just (Connection threadId _) ->
                pure . Just $ Connection threadId Nothing
          )
          island
      onDisconect
  in
    modifyMVar_ mvar $ \connMap -> do
      threadId <- forkIO mainThread
      pure $
        Map.insert
          island
          (Connection threadId Nothing)
          connMap

mkConnectionManager :: IO ConnectionManager
mkConnectionManager = do
  mapMVar <- newMVar Map.empty
  pure $ ConnectionManager {connectionsMVar = mapMVar}

removeConnection :: Island -> ConnectionManager -> IO ()
removeConnection island (ConnectionManager connectionsMVar) = do
  modifyMVar_ connectionsMVar $
    Map.alterF
      ( \case
          Just (Connection threadId _) -> killThread threadId >> pure Nothing
          Nothing -> pure Nothing
      )
      island

killConnections :: ConnectionManager -> IO ()
killConnections (ConnectionManager connectionsMVar) = do
  modifyMVar_ connectionsMVar $
    \connections -> do
      forM_ connections $ \(Connection threadId _) -> killThread threadId
      pure Map.empty

initTCPClientConnection ::
  Island
  -> ConnectionManager
  -> HostName
  -> ServiceName
  -> (B.ByteString -> IO ())
  -> IO ()
  -> IO ()
  -> IO ()
initTCPClientConnection island connMngr host port withBytes onConnect onDisconnect =
  addRxTxConnection @Socket @SocketRxError
    connMngr
    withBytes
    onErr
    (aquireConnectedClientSocket host port onConnect onDisconnect)
    onConnect
    onDisconnect
    island
 where
  onErr sock _err = do
    close sock
    putStrLn "TCP Client connection ended. Trying again in 2 seconds..."
    threadDelay 2000000
    initTCPClientConnection island connMngr host port withBytes onConnect onDisconnect

initTCPServerConnection ::
  Island
  -> ConnectionManager
  -> ServiceName
  -> (B.ByteString -> IO ())
  -> IO ()
  -> IO ()
  -> IO Socket
initTCPServerConnection island connMngr port withMsg onConnect onDisconnect = do
  serverSock <- aquireBoundListeningServerSocket port
  go serverSock
  pure serverSock
 where
  go serverSock =
    addRxTxConnection @Socket @SocketRxError
      connMngr
      withMsg
      (onErr serverSock)
      (open serverSock)
      onConnect
      onDisconnect
      island

  open sock = do
    (conn, peer) <- accept sock
    onConnect
    putStrLn $ "Accepted connection from " <> show peer
    pure conn

  onErr serverSock sock _err = do
    close sock
    putStrLn "TCP Server connection ended. Trying again in 2 seconds..."
    threadDelay 2000000
    go serverSock

addChannelsConnection ::
  Island -- ^ Island that this connection is to
  -> ConnectionManager 
  -> (Chan B.ByteString, Chan B.ByteString)
  -> (B.ByteString -> IO ())
  -> IO ()
  -> IO ()
addChannelsConnection island connMngr chans withBytes onConnect = do
  addRxTxConnection @(Chan B.ByteString, Chan B.ByteString)  @ChannelRxError
    connMngr
    withBytes
    onErr
    (pure chans)
    onConnect
    (pure ())
    island

 where
  onErr _ _ = pure ()

trySendMsg ::
  forall msg.
  Msg msg =>
  ConnectionManager
  -> Island
  -- ^ Source of msg
  -> Island
  -- ^ Target of msg
  -> Island
  -- ^ Next hop for message
  -> msg
  -> IO Bool
trySendMsg (ConnectionManager mvar) src finalDest nextDest msg = do
  withMVar mvar $ \connMap -> case Map.lookup nextDest connMap of
    Just (Connection _ (Just sender)) -> do
      sender $ toBytes (src, finalDest, msg)
      pure True
    _ -> pure False
