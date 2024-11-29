{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Router (Router, thisIsland, connectionsManager, mkRouter, trySendMessage, forwardMsg) where

import Connection (sendMsg)
import ConnectionManager (
    ConnectionManager,
    Island (..),
    getConnection,
    mkConnectionManager,
 )
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..), runPut)
import Lens.Micro.TH (makeLenses)
import Msg (Msg (..))

data Router = Router
    { _thisIsland :: Island
    , _connectionsManager :: ConnectionManager
    }
$(makeLenses ''Router)

nextHop ::
    Island
    -- ^ Source
    -> Island
    -- ^ Destination
    -> Maybe Island
nextHop Home _ = Nothing
nextHop _ Home = Nothing
nextHop src dest
    | src == dest = Just dest
    | otherwise = Just Home

mkRouter :: Island -> IO Router
mkRouter island = do
    Router island <$> mkConnectionManager

trySendMessage :: (Msg msg) => Router -> Island -> msg -> IO Bool
trySendMessage (Router island connMgr) dest msg = do
    -- Get next hop
    let hop = fromMaybe dest $ nextHop island dest
        bytes = toBytes msg
        addressedBytes = runPut (put island) <> runPut (put dest) <> bytes
    mConn <- getConnection hop connMgr
    case mConn of
        Nothing -> pure False
        Just conn -> sendMsg conn addressedBytes >> pure True

forwardMsg :: Router -> Island -> B.ByteString -> IO Bool
forwardMsg (Router island connMgr) dest bytes = do
    -- Get next hop
    mConn <- getConnection dest connMgr
    case mConn of
        Nothing -> pure False
        Just conn -> sendMsg conn bytes >> pure True
