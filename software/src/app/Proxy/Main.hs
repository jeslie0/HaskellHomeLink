module Proxy.Main (proxyMain, main) where

import Control.Concurrent (MVar, forkIO, killThread)
import Control.Exception.Lifted (bracket)
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Vector qualified as V
import EventLoop (
  EventLoop,
  EventLoopT,
  addMsg,
  addMsgIO,
  getLoop,
  runEventLoopT,
  setInterval, MonadEventLoop (..),
 )
import Home.AudioStreamTypes (StationId, StreamStatus)
import Islands (Island (..))
import Lens.Micro
import Logger (Logs)
import Network.Socket (Socket, close)
import Proto.Messages qualified as Proto
import Proxy.Env (
  Env,
  addLocalHTTPServerConnection,
  cleanupEnv,
  logs,
  memoryMap,
  mkEnv,
  router,
  streamStatusState,
  systemMap,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import REST.HomeServer qualified as HTTP
import Router (Router)
import State (State)
import System (SystemData)
import System.Memory (MemoryInformation)

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
  Router
  -> EventLoop (Island, ExProxyHandler)
  -> IO Socket
mkServerSocket rtr loop =
  addLocalHTTPServerConnection @Proto.ProxyEnvelope
    (\(island, msg') -> addMsgIO (island, ExProxyHandler msg') loop)
    rtr

createHttpServerThread :: Env -> IO ()
createHttpServerThread env =
  httpServer
    (env ^. streamStatusState)
    (env ^. systemMap)
    (env ^. memoryMap)
    (env ^. logs)
    (env ^. router)

startCheckMemoryPoll :: EventLoopT Env (Island, ExProxyHandler) IO ()
startCheckMemoryPoll = do
  addMsg (RemoteProxy, ExProxyHandler (defMessage @Proto.CheckMemoryUsage))
  void . setInterval (RemoteProxy, ExProxyHandler (defMessage @Proto.CheckMemoryUsage)) $ 30 * 1000

proxyMain ::
  Env
  -> (Router -> EventLoop (Island, ExProxyHandler) -> IO a)
  -> (a -> IO ())
  -> Island
  -> IO ()
proxyMain env mkConnection cleanupConn island = do
  bracket (forkIO $ createHttpServerThread env) killThread $
    \_threadId ->
      runEventLoopT action env
 where
  action :: EventLoopT Env (Island, ExProxyHandler) IO ()
  action = do
    loop <- getLoop
    bracket (liftIO $ mkConnection (env ^. router) loop) (liftIO . cleanupConn)$ \_ -> do
      when (island == RemoteProxy) startCheckMemoryPoll
      start $ uncurry proxyHandler

main :: IO ()
main =
  bracket (mkEnv RemoteProxy) cleanupEnv $ \env ->
    proxyMain
      env
      mkServerSocket
      close
      RemoteProxy
