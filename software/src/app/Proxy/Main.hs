module Proxy.Main (proxyMain, main) where

import Control.Concurrent (MVar, forkIO, killThread)
import Control.Exception.Lifted (bracket)
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Vector qualified as V
import Envelope (toProxyEnvelope)
import EventLoop (
  EventLoop,
  EventLoopT,
  MonadEventLoop (..),
  addMsg,
  addMsgIO,
  getLoop,
  runEventLoopT,
  setInterval,
 )
import Home.AudioStreamTypes (StationId, StreamStatus)
import Islands (Island (..))
import Lens.Micro
import Logger (Logs)
import Network.Socket (Socket, close)
import Proto.Messages qualified as Proto
import ProtoHelper (toMessage)
import Proxy.Env (
  Env,
  addServerConnection,
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
import Router (Router, trySendMessage)
import State (State)
import System (SystemData, mkSystemData)
import System.Memory (MemoryInformation)
import Proxy.Options (ProxyOptions, httpsCertificatePath, httpsKeyPath)
import Options (runCommand)

httpServer ::
  FilePath
  -> FilePath
  -> State (StreamStatus, StationId)
  -> MVar (Map.Map Island SystemData)
  -> MVar (Map.Map Island (V.Vector MemoryInformation))
  -> Logs
  -> Router
  -> IO ()
httpServer certPath keyPath state systemData memoryData logs' rtr = do
  env <- HTTP.mkEnv state systemData memoryData logs' rtr
  HTTP.runApp certPath keyPath env

mkServerSocket ::
  Router
  -> EventLoop (Island, ExProxyHandler)
  -> IO Socket
mkServerSocket rtr loop =
  addServerConnection @Proto.ProxyEnvelope
    (\(island, msg') -> addMsgIO (island, ExProxyHandler msg') loop)
    rtr
    sendSystemData
    (pure ())
 where
  sendSystemData = do
    systemMsg <-
      toMessage @Proto.SystemData @SystemData <$> mkSystemData RemoteProxy
    val <- trySendMessage rtr LocalHTTP $ toProxyEnvelope systemMsg
    print val

createHttpServerThread :: FilePath -> FilePath -> Env -> IO ()
createHttpServerThread certPath keyPath env =
  httpServer
    certPath
    keyPath
    (env ^. streamStatusState)
    (env ^. systemMap)
    (env ^. memoryMap)
    (env ^. logs)
    (env ^. router)

startCheckMemoryPoll :: EventLoopT Env (Island, ExProxyHandler) IO ()
startCheckMemoryPoll = do
  addMsg (RemoteProxy, ExProxyHandler (defMessage @Proto.CheckMemoryUsage))
  void
    . setInterval (RemoteProxy, ExProxyHandler (defMessage @Proto.CheckMemoryUsage))
    $ 30 * 1000

proxyMain ::
  FilePath -> FilePath ->
  Env
  -> (Router -> EventLoop (Island, ExProxyHandler) -> IO a)
  -> (a -> IO ())
  -> Island
  -> IO ()
proxyMain certPath keyPath env mkConnection cleanupConn island = do
  bracket (forkIO $ createHttpServerThread certPath keyPath env) killThread $
    \_threadId ->
      runEventLoopT action env
 where
  action :: EventLoopT Env (Island, ExProxyHandler) IO ()
  action = do
    loop <- getLoop
    bracket (liftIO $ mkConnection (env ^. router) loop) (liftIO . cleanupConn) $ \_ -> do
      when (island == RemoteProxy) startCheckMemoryPoll
      start $ uncurry proxyHandler

main :: IO ()
main = runCommand $ \(opts :: ProxyOptions) _args -> do
  bracket (mkEnv RemoteProxy) cleanupEnv $ \env ->
    proxyMain
      (opts ^. httpsCertificatePath) (opts ^. httpsKeyPath)
      env
      mkServerSocket
      close
      RemoteProxy
