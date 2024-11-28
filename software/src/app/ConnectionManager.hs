module ConnectionManager (
    ConnectionManager,
    Island (..),
    mkConnectionManager,
    addConnection,
    removeConnection,
    getConnection,
) where

import Connection (Connection)
import Control.Concurrent (MVar, modifyMVar_, newMVar, withMVar)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize (..), ensure, getWord8, putWord8)
import GHC.Generics (Generic)

data Island
    = Home
    | Proxy
    deriving (Generic, Eq, Ord)

instance Serialize Island where
    put island = putWord8 $ case island of
        Home -> 0
        Proxy -> 1

    get = do
        bytes <- ensure 1
        word <- getWord8
        case word of
            0 -> pure Home
            1 -> pure Proxy
            _ -> fail "Could not deserialise value into an island"

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
