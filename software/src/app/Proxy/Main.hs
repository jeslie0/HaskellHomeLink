module Proxy.Main (proxyMain, main) where

import Connection.TLS (setupTLSServerParams)
import Control.Concurrent (MVar, forkIO, killThread)
import Control.Exception.Lifted (bracket)
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
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
import Network.Socket (HostName, PortNumber, Socket, close)
import Network.TLS (ServerParams)
import Options (runCommand)
import Proto.Messages qualified as Proto
import ProtoHelper (toMessage)
import Proxy.Env (
  Env,
  addTLSServerConnection,
  cleanupEnv,
  logs,
  memoryMap,
  mkEnv,
  router,
  streamStatusState,
  systemMap,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import Proxy.Options (
  ProxyOptions,
  configPath,
  httpHostname,
  httpPort,
  httpsCACertificatePath,
  httpsCertificatePath,
  httpsKeyPath,
  tlsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
  tlsPort, tlsHostname,
 )
import REST.HomeServer qualified as HTTP
import Router (Router, trySendMessage)
import State (State)
import System (SystemData, mkSystemData)
import System.Memory (MemoryInformation)

httpServer ::
  FilePath
  -> FilePath
  -> FilePath
  -> HostName
  -> PortNumber
  -> State (StreamStatus, StationId)
  -> MVar (Map.Map Island SystemData)
  -> MVar (Map.Map Island (V.Vector MemoryInformation))
  -> Logs
  -> Router
  -> IO ()
httpServer certPath keyPath caCertPath host port state systemData memoryData logs' rtr = do
  env <- HTTP.mkEnv state systemData memoryData logs' rtr
  HTTP.runApp certPath keyPath caCertPath host port env

-- mkServerSocket ::
--   Router
--   -> EventLoop (Island, ExProxyHandler)
--   -> IO Socket
-- mkServerSocket rtr loop =
--   addServerConnection @Proto.ProxyEnvelope
--     (\(island, msg') -> addMsgIO (island, ExProxyHandler msg') loop)
--     rtr
--     sendSystemData
--     (pure ())
--  where
--   sendSystemData = do
--     systemMsg <-
--       toMessage @Proto.SystemData @SystemData <$> mkSystemData RemoteProxy
--     void . trySendMessage rtr LocalHTTP $ toProxyEnvelope systemMsg

mkTLSServerSocket ::
  ServerParams
  -> PortNumber
  -> Router
  -> EventLoop (Island, ExProxyHandler)
  -> IO Socket
mkTLSServerSocket params port rtr loop = do
  addTLSServerConnection @Proto.ProxyEnvelope
    params
    (\(island, msg') -> addMsgIO (island, ExProxyHandler msg') loop)
    rtr
    port
    sendSystemData
    (pure ())
 where
  sendSystemData = do
    systemMsg <-
      toMessage @Proto.SystemData @SystemData <$> mkSystemData RemoteProxy
    void . trySendMessage rtr LocalHTTP $ toProxyEnvelope systemMsg

createHttpServerThread ::
  FilePath -> FilePath -> FilePath -> HostName -> PortNumber -> Env -> IO ()
createHttpServerThread certPath keyPath caCertPath host port env =
  httpServer
    certPath
    keyPath
    caCertPath
    host
    port
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
  FilePath
  -> FilePath
  -> FilePath
  -> HostName
  -> PortNumber
  -> Env
  -> (Router -> EventLoop (Island, ExProxyHandler) -> IO a)
  -> (a -> IO ())
  -> Island
  -> IO ()
proxyMain certPath keyPath caCertPath host port env mkConnection cleanupConn island = do
  bracket
    (forkIO $ createHttpServerThread certPath keyPath caCertPath host port env)
    killThread
    $ \_threadId ->
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
  mConfig <- eitherDecodeFileStrict (opts ^. configPath)
  case mConfig of
    Left errs -> putStrLn $ "Could not parse configuration file: " <> errs
    Right config -> do
      mParams <-
        setupTLSServerParams
          (config ^. tlsHostname)
          (config ^. tlsCertificatePath)
          (config ^. tlsKeyPath)
          (config ^. tlsCACertificatePath)
      case mParams of
        Nothing -> putStrLn "Couldn't make TLS parameters..."
        Just params -> do
          bracket (mkEnv RemoteProxy) cleanupEnv $ \env ->
            proxyMain
              (config ^. httpsCertificatePath)
              (config ^. httpsKeyPath)
              (config ^. httpsCACertificatePath)
              (config ^. httpHostname)
              (config ^. httpPort)
              env
              (mkTLSServerSocket params (config ^. tlsPort))
              close
              RemoteProxy
