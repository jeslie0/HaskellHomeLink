module Proxy.Main (proxyMain) where

import ConnectionManager (
  killConnections,
 )
import Control.Concurrent (MVar)
import Control.Exception.Lifted (bracket)
import Control.Monad (void, when)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Vector qualified as V
import EventLoop (EventLoop, addMsg, mkEventLoop, run, setInterval)
import Home.AudioStream (StationId, StreamStatus)
import Islands (Island (..))
import Lens.Micro
import Logger (Logs)
import Network.Socket (Socket, close)
import Proto.Messages qualified as Proto
import Proxy.Env (
  Env,
  addLocalHTTPServerConnection,
  logs,
  memoryMap,
  mkEnv,
  router,
  streamStatusState,
  systemMap,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import REST.HomeServer qualified as HTTP
import Router (Router, connectionsManager)
import State (State)
import System (SystemData)
import System.Memory (MemoryInformation)
import Threads (killAsyncComputation, spawnAsyncComputation)

httpServer ::
  State (StreamStatus, StationId)
  -> MVar (Map.Map Island SystemData)
  -> MVar (Map.Map Island (V.Vector MemoryInformation))
  -> Logs
  -> Router
  -> IO ()
httpServer state systemData memoryData logs' rtr = do
  env <- HTTP.mkEnv state systemData memoryData logs' rtr
  HTTP.runApp env

mkServerSocket ::
  MonadIO m =>
  Router
  -> EventLoop m (Island, ExProxyHandler)
  -> IO Socket
mkServerSocket rtr loop =
  addLocalHTTPServerConnection @Proto.ProxyEnvelope
    (addMsg loop . second ExProxyHandler)
    rtr

proxyMain ::
  (Router -> EventLoop (ReaderT Env IO) (Island, ExProxyHandler) -> IO a)
  -> (a -> IO ())
  -> Island
  -> IO ()
proxyMain mkConnection cleanupConn island = do
  bracket (mkEnv island) cleanupEnv $ \env ->
    bracket
      ( spawnAsyncComputation $
          httpServer
            (env ^. streamStatusState)
            (env ^. systemMap)
            (env ^. memoryMap)
            (env ^. logs)
            (env ^. router)
      )
      killAsyncComputation
      $ \_async ->
        runReaderT (action env) env
 where
  action :: Env -> ReaderT Env IO ()
  action env = do
    loop <- mkEventLoop @(Island, ExProxyHandler)
    bracket (liftIO $ mkConnection (env ^. router) loop) (liftIO . cleanupConn) $ \_ -> do
      when (island == RemoteProxy)
        . void
        . liftIO
        $ setInterval loop (Home, ExProxyHandler (defMessage @Proto.CheckMemoryUsage))
        $ 30 * 1000

      run loop $ \evloop b -> uncurry (proxyHandler evloop) b

  cleanupEnv env = do
    killConnections (env ^. (router . connectionsManager))

-- main :: IO ()
-- main = proxyMain RemoteProxy
