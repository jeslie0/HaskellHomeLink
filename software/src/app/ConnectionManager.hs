module ConnectionManager (
    ConnectionManager,
    Island (..),
    mkConnectionManager,
    addConnection,
    removeConnection,
    getConnection,
    getSrcDest,
    killConnections,
) where

import Connection (Connection, killConnection)
import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import Control.Monad (forM_, void)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize (..), ensure, getWord8, putWord8, runGet)
import GHC.Generics (Generic)

data Island
    = Home
    | RemoteProxy
    | LocalProxy
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

newtype ConnectionManager = ConnectionManager {connectionsMVar :: MVar (Map.Map Island Connection)}

mkConnectionManager :: IO ConnectionManager
mkConnectionManager = do
    mapMVar <- newMVar Map.empty
    pure $ ConnectionManager {connectionsMVar = mapMVar}

addConnection :: Island -> Connection -> ConnectionManager -> IO ()
addConnection island conn (ConnectionManager connectionsMVar) = do
    modifyMVar_ connectionsMVar $ pure . Map.insert island conn

removeConnection :: Island -> ConnectionManager -> IO ()
removeConnection island (ConnectionManager connectionsMVar) = do
    modifyMVar_ connectionsMVar $ pure . Map.delete island

getConnection :: Island -> ConnectionManager -> IO (Maybe Connection)
getConnection island (ConnectionManager connectionsMVar) = do
    withMVar connectionsMVar $ pure . Map.lookup island

killConnections :: ConnectionManager -> IO ()
killConnections (ConnectionManager connectionsMVar) = do
    modifyMVar_ connectionsMVar $
        \connections -> do
            forM_ connections killConnection
            pure Map.empty
