module Proxy.Main (main) where

import Control.Concurrent (MVar, forkIO, killThread, modifyMVar_, myThreadId)
import Control.Exception (bracket, bracket_, finally)
import Control.Monad (forever, void, when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString qualified as B
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Serialize (decode)
import Data.Vector qualified as V
import Devices (Device (..))
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
import Lens.Micro
import Logger (Logs)
import Network.Socket (HostName, PortNumber, Socket, accept, close)
import Network.TLS (ServerParams)
import Network.WebSockets qualified as WS
import Options (runCommand)
import Proto.DeviceData qualified as Proto
import Proto.DeviceData_Fields qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import ProtoHelper (toMessage)
import Proxy.Env (
  Env,
  cleanupEnv,
  deviceMap,
  logs,
  memoryMap,
  mkEnv,
  router,
  streamStatusState,
  websocketsMap,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import Proxy.Options (
  ProxyConfiguration,
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
import Proxy.REST.HomeServer qualified as HTTP
import Router (
  MessagePackage,
  Router,
  connectionsRegistry,
  handleBytes,
  trySendMessage,
 )
import RxTx.Connection (cleanup, recvAndDispatch)
import RxTx.Connection.Socket (aquireBoundListeningServerSocket)
import RxTx.Connection.TLS (upgradeSocket)
import RxTx.ConnectionRegistry (
  AsyncConnectionRegistry,
  addConnection,
  removeConnection,
 )
import RxTx.TLS (TLSRxError (..))
import State (State, setState)
import System (DeviceData, mkDeviceData)
import System.Memory (MemoryInformation)
import TLSHelper (setupTLSServerParams)

httpServer ::
  FilePath
  -> FilePath
  -> FilePath
  -> HostName
  -> PortNumber
  -> State (StreamStatus, StationId)
  -> MVar (Map.Map Device DeviceData)
  -> MVar (Map.Map Device (V.Vector MemoryInformation))
  -> MVar (Map.Map Int32 WS.Connection)
  -> Logs
  -> Router
  -> IO ()
httpServer certPath keyPath caCertPath host port state systemData memoryData wsConns logs' rtr = do
  env <- HTTP.mkEnv state systemData memoryData wsConns logs' rtr
  HTTP.runApp certPath keyPath caCertPath host port env

-- mkTLSServerSocket ::
--   ServerParams
--   -> PortNumber
--   -> Env
--   ->
--   -> IO Socket
-- mkTLSServerSocket params port env loop = do
--   addTLSServerConnection @Proto.ProxyEnvelope
--     params
--     (\(island, msg') -> addMsgIO (island, ExProxyHandler msg') loop)
--     (env ^. router)
--     port
--     sendDeviceData
--     onDisconnect
--  where
--   sendDeviceData = do
--     systemMsg <-
--       toMessage @Proto.DeviceData @DeviceData <$> mkDeviceData RemoteProxy
--     void . trySendMessage (env ^. router) LocalHTTP $ toProxyEnvelope systemMsg

--   -- When we disconnect, we need to remove the HOME connection from
--   -- the http resources
--   onDisconnect = do
--     modifyMVar_ (env ^. systemMap) $ pure . Map.delete Home
--     modifyMVar_ (env ^. memoryMap) $ pure . Map.delete Home
--     setState (Off, 0) (env ^. streamStatusState)

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
    (env ^. deviceMap)
    (env ^. memoryMap)
    (env ^. websocketsMap)
    (env ^. logs)
    (env ^. router)

-- listenSock <- aquireBoundListeningServerSocket port

listeningSocketThread ::
  EventLoop (Device, ExProxyHandler)
  -> Router 
  -> ServerParams
  -> Socket
  -> IO ()
listeningSocketThread loop router params listenSock =
  forever $ do
    bracket (accept listenSock) (\(sock, peer) -> close sock) $ \(sock, peer) -> do
      putStrLn $ "Accepted TCP connection from " <> show peer
      mConn <- upgradeSocket params sock
      case mConn of
        Nothing -> do
          putStrLn "Failed to upgrade connection to TLS"
        Just conn -> do
          putStrLn "Established TLS connection"
          threadId <- myThreadId
          bracket_
            (addConnection Home threadId conn (router ^. connectionsRegistry))
            (removeConnection Home (router ^. connectionsRegistry))
            (runConnection conn)
 where
  runConnection conn = do
    merror <- recvAndDispatch conn
    case merror of
      Left (TLSRxError _ err) -> do
        print $ "ERROR: " <> show err
        cleanup conn
      Right bytes -> do
        handleBytes @Proto.WrappedEnvelope bytes router $ \src wrappedEnv ->
          case wrappedEnv ^. Proto.maybe'wrappedPayload of
            Just (Proto.WrappedEnvelope'ProxyMsg proxyMsg) -> addMsgIO (src, ExProxyHandler proxyMsg) loop
            _ -> putStrLn "Dropping message for wrong device..."
        runConnection conn

startCheckMemoryPoll :: EventLoopT Env (Device, ExProxyHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Proxy, ExProxyHandler (defMessage @Proto.CheckMemoryUsage))
  void
    . setInterval (Proxy, ExProxyHandler (defMessage @Proto.CheckMemoryUsage))
    $ 30 * 1000

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
          bracket (mkEnv Proxy) cleanupEnv $ \env ->
            bracket
              ( forkIO $
                  createHttpServerThread
                    (config ^. httpsCertificatePath)
                    (config ^. httpsKeyPath)
                    (config ^. httpsCACertificatePath)
                    (config ^. httpHostname)
                    (config ^. httpPort)
                    env
              )
              killThread
              $ \_ -> bracket (aquireBoundListeningServerSocket (show $ config ^. tlsPort)) close $ \serverSock -> do
                runEventLoopT (action config params serverSock) env
                writeFile "/mnt/normalexit" ""
         where
          action ::
            ProxyConfiguration
            -> ServerParams
            -> Socket
            -> EventLoopT Env (Device, ExProxyHandler) IO ()
          action config params socket = do
            loop <- getLoop
            env <- getEnv
            socketThread <-
              liftIO . forkIO $
                listeningSocketThread loop (env ^. router) params socket
            startCheckMemoryPoll
            start $ uncurry proxyHandler
