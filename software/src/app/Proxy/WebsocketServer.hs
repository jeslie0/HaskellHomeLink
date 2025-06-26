module Proxy.WebsocketServer (websocketServer) where

import Control.Concurrent (MVar, modifyMVar_, withMVar)
import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Servant (Application)
import System.Random (randomIO)

handleProxyWS ::
  MVar (Maybe B.ByteString) -> MVar (Map.Map Int32 WS.Connection) -> WS.ServerApp
handleProxyWS initialChunk conns pconn = do
  iden :: Int32 <- randomIO
  conn <- WS.acceptRequest pconn
  modifyMVar_ conns $ pure . Map.insert iden conn
  withMVar initialChunk $ \mChunk -> forM_ mChunk $ WS.sendBinaryData conn

websocketServer ::
  MVar (Maybe B.ByteString)
  -> MVar (Map.Map Int32 WS.Connection)
  -> Application
  -> Application
websocketServer initialChunk conns =
  websocketsOr WS.defaultConnectionOptions (handleProxyWS initialChunk conns)
