module Proxy.WebsocketServer (websocketServer) where

import Network.WebSockets qualified as WS
import Control.Concurrent (MVar, modifyMVar_)
import qualified Data.Map.Strict as Map
import Data.Int (Int32)
import System.Random (randomIO)
import Servant (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)

handleProxyWS :: MVar (Map.Map Int32 WS.Connection) -> WS.ServerApp
handleProxyWS conns pconn = do
  iden :: Int32 <- randomIO
  conn <- WS.acceptRequest pconn
  modifyMVar_ conns $ pure . Map.insert iden conn

websocketServer :: MVar (Map.Map Int32 WS.Connection) -> Application -> Application
websocketServer conns =
  websocketsOr WS.defaultConnectionOptions (handleProxyWS conns)
