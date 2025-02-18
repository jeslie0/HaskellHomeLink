module Home.Main (main) where

import Connection.TLS (setupTLSClientParams)
import ConnectionManager (
  addChannelsConnection,
  initTLSClientConnection,
 )
import Control.Concurrent (Chan, ThreadId, forkIO, killThread, newChan)
import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
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
import Home.Env (Env, cleanupEnv, mkEnv, router)
import Home.Handler (ExHomeHandler (..), homeHandler)
import Home.Options (
  HomeConfiguration,
  HomeOptions,
  configPath,
  httpPort,
  httpsCACertificatePath,
  httpsCertificatePath,
  httpsKeyPath,
  proxyPort,
  proxyURL,
  tlsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath, httpHostname,
 )
import Islands (Island (..))
import Lens.Micro
import Network.Socket (HostName, PortNumber)
import Options (runCommand)
import Proto.Messages qualified as Proto
import ProtoHelper (toMessage)
import Proxy.Env qualified as Proxy
import Proxy.Handler (ExProxyHandler (..))
import Proxy.Main (proxyMain)
import Router (Router, connectionsManager, handleBytes, trySendMessage)
import System (SystemData, mkSystemData)

addLocalHostConnection ::
  Env
  -> EventLoop (Island, ExHomeHandler)
  -> (Chan B.ByteString, Chan B.ByteString)
  -> IO ()
addLocalHostConnection env loop clientConn =
  addChannelsConnection
    LocalHTTP
    (env ^. router . connectionsManager)
    clientConn
    ( handleBytes @Proto.HomeEnvelope
        (\(island, msg) -> addMsgIO (island, ExHomeHandler msg) loop)
        (env ^. router)
    )
    (pure ())

-- mkRemoteProxyConn ::
--   Env
--   -> EventLoop (Island, ExHomeHandler)
--   -> IO ()
-- mkRemoteProxyConn env loop = do
--   let rtr = env ^. router
--   initTCPClientConnection
--     RemoteProxy
--     (rtr ^. connectionsManager)
--     "127.0.0.1"
--     "3001"
--     ( handleBytes @Proto.HomeEnvelope
--         (\(island, msg) -> addMsgIO (island, ExHomeHandler msg) loop)
--         rtr
--     )
--     sendSystemData
--     (pure ())
--  where
--   sendSystemData = do
--     systemMsg <- toMessage @Proto.SystemData @SystemData <$> mkSystemData Home
--     void . trySendMessage (env ^. router) RemoteProxy $ toProxyEnvelope systemMsg

mkRemoteProxyConnTLS ::
  FilePath
  -> FilePath
  -> FilePath
  -> HostName
  -> PortNumber
  -> HostName
  -> Env
  -> EventLoop (Island, ExHomeHandler)
  -> IO ()
mkRemoteProxyConnTLS certPath keyPath caCertPath host port certHostname env loop = do
  let rtr = env ^. router
  mParams <- setupTLSClientParams certPath keyPath caCertPath certHostname
  case mParams of
    Nothing -> putStrLn "Could not create client TLS parameters"
    Just params ->
      initTLSClientConnection
        params
        RemoteProxy
        (rtr ^. connectionsManager)
        host
        port
        ( handleBytes @Proto.HomeEnvelope
            (\(island, msg) -> addMsgIO (island, ExHomeHandler msg) loop)
            rtr
        )
        sendSystemData
        (pure ())
 where
  sendSystemData = do
    systemMsg <- toMessage @Proto.SystemData @SystemData <$> mkSystemData Home
    void . trySendMessage (env ^. router) RemoteProxy $ toProxyEnvelope systemMsg

startCheckMemoryPoll ::
  EventLoopT Env (Island, ExHomeHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

-- Use channels to communicate with local proxy
mkLocalProxyThread ::
  FilePath
  -> FilePath
  -> FilePath
  -> HostName
  -> PortNumber
  -> (Chan B.ByteString, Chan B.ByteString)
  -> IO ThreadId
mkLocalProxyThread certPath keyPath caCertPath host port serverConn =
  forkIO $ bracket (Proxy.mkEnv LocalHTTP) Proxy.cleanupEnv $ \env -> do
    proxyMain
      certPath
      keyPath
      caCertPath
      host
      port
      env
      (mkChannelsConnection serverConn)
      (const $ pure ())
      LocalHTTP

mkChannelsConnection ::
  (Chan B.ByteString, Chan B.ByteString)
  -> Router
  -> EventLoop (Island, ExProxyHandler)
  -> IO ()
mkChannelsConnection serverConn rtr loop =
  addChannelsConnection
    Home
    (rtr ^. connectionsManager)
    serverConn
    ( handleBytes @Proto.ProxyEnvelope
        (\(island, msg) -> addMsgIO (island, ExProxyHandler msg) loop)
        rtr
    )
    (pure ())

main :: IO ()
main = runCommand $ \(opts :: HomeOptions) _args -> do
  mConfig <- eitherDecodeFileStrict (opts ^. configPath)
  case mConfig of
    Left errs -> putStrLn $ "Could not parse configuration file: " <> errs
    Right config -> do
      bracket mkEnv cleanupEnv $ \env ->
        runEventLoopT (action config) env
 where
  action ::
    HomeConfiguration -> EventLoopT Env (Island, ExHomeHandler) IO ()
  action config = do
    ch1 <- liftIO newChan
    ch2 <- liftIO newChan
    let
      clientConn = (ch1, ch2)
      serverConn = (ch2, ch1)

    loop <- getLoop
    env <- getEnv
    bracket
      ( liftIO $
          mkLocalProxyThread
            (config ^. httpsCertificatePath)
            (config ^. httpsKeyPath)
            (config ^. httpsCACertificatePath)
            (config ^. httpHostname)
            (config ^. httpPort)
            serverConn
      )
      (liftIO . killThread)
      $ \_threadId -> do
        liftIO $ addLocalHostConnection env loop clientConn
        liftIO $
          mkRemoteProxyConnTLS
            (config ^. tlsCertificatePath)
            (config ^. tlsKeyPath)
            (config ^. tlsCACertificatePath)
            (config ^. proxyURL)
            (config ^. proxyPort)
            (config ^. httpHostname)
            env
            loop
        startCheckMemoryPoll
        start $ uncurry homeHandler
