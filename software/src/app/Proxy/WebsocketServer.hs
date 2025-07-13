module Proxy.WebsocketServer (websocketServer) where

import Control.Concurrent (MVar, modifyMVar_, withMVar)
import Control.Exception (IOException, catch, SomeException)
import Control.Monad (forM_, when)
import Data.ByteString qualified as B
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Proxy.Handler (RemoveWSConn (..))
import Servant (Application)
import System.Random (randomIO)

handleProxyWS ::
  MVar (Maybe B.ByteString) -> MVar (Map.Map Int32 WS.Connection) -> WS.ServerApp
handleProxyWS initialChunk conns pconn = do
  iden :: Int32 <- randomIO
  putStrLn $ "HANDLE PROXY WS " <> show iden
  conn <- WS.acceptRequest pconn
  withMVar initialChunk $ \mChunk -> forM_ mChunk $ \chunk -> do
    sendInitialChunk conn chunk iden
      `catch` \(e :: SomeException) -> do
        putStrLn $ "Caught exception sending initial chunk. Will not add to map."
  loop iden conn
 where
  sendInitialChunk conn chunk iden = do
    WS.sendBinaryData conn chunk
    print $ "Sent initial chunk to new WS: " <> show iden
    modifyMVar_ conns $ pure . Map.insert iden conn

  loop iden conn = do
    res <-
      (WS.receive conn >> pure True) `catch` \(e :: SomeException) -> do
        putStrLn "[wsServer]: Connection terminated"
        modifyMVar_ conns $ pure . Map.delete iden
        pure False
    when res $ loop iden conn

websocketServer ::
  MVar (Maybe B.ByteString)
  -> MVar (Map.Map Int32 WS.Connection)
  -> Application
  -> Application
websocketServer initialChunk conns =
  websocketsOr WS.defaultConnectionOptions (handleProxyWS initialChunk conns)
