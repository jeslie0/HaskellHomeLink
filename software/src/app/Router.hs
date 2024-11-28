{-# LANGUAGE MonoLocalBinds #-}

module Router (Router, mkRouter, trySendMessage) where

import Connection (sendMsg)
import ConnectionManager (
    ConnectionManager,
    Island (..),
    getConnection,
    mkConnectionManager,
 )
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..), runPut)
import Msg (Msg (..))

data Router = Router
    { thisIsland :: Island
    , connectionsManager :: ConnectionManager
    }

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
trySendMessage (Router thisIsland connMgr) dest msg = do
    -- Get next hop
    let hop = fromMaybe dest $ nextHop thisIsland dest
        bytes = toBytes msg
        addressedBytes = runPut (put thisIsland) <> runPut (put dest) <> bytes
    mConn <- getConnection hop connMgr
    case mConn of
        Nothing -> pure False
        Just conn -> sendMsg conn addressedBytes >> pure True
