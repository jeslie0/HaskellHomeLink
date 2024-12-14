{-# LANGUAGE LambdaCase #-}

module ConnectionManager (
    ConnectionManager,
    Island (..),
    mkConnectionManager,
    removeConnection,
    getSrcDest,
    killConnections,
    initTCPClientConnection,
    initTCPServerConnection,
    trySendBytes
) where

import Connection.TCP (aquireActiveClientSocket, aquireActiveServerSocket)
import Control.Concurrent (
    MVar,
    ThreadId,
    forkIO,
    killThread,
    modifyMVar_,
    myThreadId,
    newMVar,
    withMVar,
 )
import Control.Monad (forM_, void)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize (..), ensure, getWord8, putWord8, runGet, putWord32le)
import GHC.Generics (Generic)
import Network.Socket
import Socket (sendAll)
import Data.Serialize.Put (runPut)

data Island
    = Home
    | RemoteProxy
    | LocalHTTP
    deriving (Generic, Eq, Ord, Enum, Show)

instance Serialize Island where
    get = do
        void $ ensure 1
        toEnum . fromIntegral <$> getWord8

    put = putWord8 . fromIntegral . fromEnum

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

getSrcDest :: B.ByteString -> Maybe (Island, Island)
getSrcDest bytes
    | B.length bytes /= 2 = Nothing
    | otherwise =
        let (srcByte, destByte) = B.splitAt 1 bytes
         in rightToMaybe $ do
                src <- runGet get srcByte
                dest <- runGet get destByte
                pure (src, dest)

newtype ConnectionManager = ConnectionManager
    { connectionsMVar ::
        MVar (Map.Map Island (ThreadId, Maybe (B.ByteString -> IO ())))
    }

mkConnectionManager :: IO ConnectionManager
mkConnectionManager = do
    mapMVar <- newMVar Map.empty
    pure $ ConnectionManager {connectionsMVar = mapMVar}

removeConnection :: Island -> ConnectionManager -> IO ()
removeConnection island (ConnectionManager connectionsMVar) = do
    modifyMVar_ connectionsMVar $
        Map.alterF
            ( \case
                Just (threadId, _) -> killThread threadId >> pure Nothing
                Nothing -> pure Nothing
            )
            island

trySendBytes :: ConnectionManager -> Island -> B.ByteString -> IO Bool
trySendBytes (ConnectionManager mapMVar) island bytes = do
    withMVar mapMVar $ \m -> case Map.lookup island m of
        Just (_, Just f) -> do
            f . runPut . putWord32le . fromIntegral $ B.length bytes
            f bytes
            pure True
        _ -> pure False

killConnections :: ConnectionManager -> IO ()
killConnections (ConnectionManager connectionsMVar) = do
    modifyMVar_ connectionsMVar $
        \connections -> do
            forM_ connections $ \(threadId, _) -> killThread threadId
            pure Map.empty

initTCPClientConnection ::
    Island
    -> ConnectionManager
    -> HostName
    -> ServiceName
    -> (B.ByteString -> IO ())
    -> IO ()
initTCPClientConnection island (ConnectionManager connectionsMVar) host port withBytes =
    let handle = do
            threadId <- myThreadId
            aquireActiveClientSocket
                host
                port
                withBytes
                ( \sock -> modifyMVar_ connectionsMVar $ \conns ->
                    pure $ Map.insert island (threadId, Just $ sendAll sock) conns
                )
                ( \_ -> do
                    modifyMVar_ connectionsMVar $ pure . Map.insert island (threadId, Nothing)
                    handle
                )
     in modifyMVar_ connectionsMVar $ \conns -> do
            threadId <- forkIO handle
            pure $ Map.insert island (threadId, Nothing) conns

initTCPServerConnection ::
    Island
    -> ConnectionManager
    -> ServiceName
    -> (B.ByteString -> IO ())
    -> IO ()
initTCPServerConnection island (ConnectionManager connectionsMVar) port withBytes =
    let handle = do
            threadId <- myThreadId
            aquireActiveServerSocket
                port
                withBytes
                ( \sock -> modifyMVar_ connectionsMVar $ \conns ->
                    pure $ Map.insert island (threadId, Just $ sendAll sock) conns
                )
                ( \_ -> do
                    modifyMVar_ connectionsMVar $ pure . Map.insert island (threadId, Nothing)
                    handle
                )
     in modifyMVar_ connectionsMVar $ \conns -> do
            threadId <- forkIO handle
            pure $ Map.insert island (threadId, Nothing) conns
