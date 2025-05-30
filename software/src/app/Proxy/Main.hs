module Proxy.Main (proxyMain, main) where

import Connection.TLS (setupTLSServerParams)
import Control.Concurrent (MVar, forkIO, killThread, modifyMVar_)
import Control.Exception.Lifted (bracket)
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Int (Int32)
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
import Home.AudioStreamTypes (StationId, StreamStatus (..))
import Islands (Island (..))
import Lens.Micro
import Logger (Logs)
import Network.Socket (HostName, PortNumber, Socket, close)
import Network.TLS (ServerParams)
import Network.WebSockets qualified as WS
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
  systemMap, websocketsMap,
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
  tlsHostname,
  tlsKeyPath,
  tlsPort,
 )
import REST.HomeServer qualified as HTTP
import Router (Router, trySendMessage)
import State (State, setState)
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
  -> MVar (Map.Map Int32 WS.Connection)
  -> Logs
  -> Router
  -> IO ()
httpServer certPath keyPath caCertPath host port state systemData memoryData wsConns logs' rtr = do
  env <- HTTP.mkEnv state systemData memoryData wsConns logs' rtr
  HTTP.runApp certPath keyPath caCertPath host port env

mkTLSServerSocket ::
  ServerParams
  -> PortNumber
  -> Env
  -> EventLoop (Island, ExProxyHandler)
  -> IO Socket
mkTLSServerSocket params port env loop = do
  addTLSServerConnection @Proto.ProxyEnvelope
    params
    (\(island, msg') -> addMsgIO (island, ExProxyHandler msg') loop)
    (env ^. router)
    port
    sendSystemData
    onDisconnect
 where
  sendSystemData = do
    systemMsg <-
      toMessage @Proto.SystemData @SystemData <$> mkSystemData RemoteProxy
    void . trySendMessage (env ^. router) LocalHTTP $ toProxyEnvelope systemMsg

  -- When we disconnect, we need to remove the HOME connection from
  -- the http resources
  onDisconnect = do
    modifyMVar_ (env ^. systemMap) $ pure . Map.delete Home
    modifyMVar_ (env ^. memoryMap) $ pure . Map.delete Home
    setState (Off, 0) (env ^. streamStatusState)

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
    (env ^. websocketsMap)
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
  -> (Env -> EventLoop (Island, ExProxyHandler) -> IO a)
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
    bracket (liftIO $ mkConnection env loop) (liftIO . cleanupConn) $ \_ -> do
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
          writeFile "/mnt/normalexit" ""
