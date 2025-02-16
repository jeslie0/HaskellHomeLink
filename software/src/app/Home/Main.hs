module Home.Main (main) where

import Connection.TLS (setupTLSClientParams)
import ConnectionManager (
  addChannelsConnection,
  initTCPClientConnection,
  initTLSClientConnection,
 )
import Control.Concurrent (Chan, ThreadId, forkIO, killThread, newChan)
import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
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
  HomeOptions,
  httpsCACertificatePath,
  httpsCertificatePath,
  httpsKeyPath,
  proxyPort,
  proxyURL,
  tlsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath, httpPort,
 )
import Islands (Island (..))
import Lens.Micro
import Network.Socket (HostName, ServiceName)
import Options (runCommand)
import Proto.Messages qualified as Proto
import ProtoHelper (toMessage)
import Proxy.Env qualified as Proxy
import Proxy.Handler (ExProxyHandler (..))
import Proxy.Main (proxyMain)
import Router (Router, connectionsManager, handleBytes, trySendMessage)
import System (SystemData, mkSystemData)
import Network.Wai.Handler.Warp (Port)

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

mkRemoteProxyConn ::
  Env
  -> EventLoop (Island, ExHomeHandler)
  -> IO ()
mkRemoteProxyConn env loop = do
  let rtr = env ^. router
  initTCPClientConnection
    RemoteProxy
    (rtr ^. connectionsManager)
    "127.0.0.1"
    "3001"
    ( handleBytes @Proto.HomeEnvelope
        (\(island, msg) -> addMsgIO (island, ExHomeHandler msg) loop)
        rtr
    )
    sendSystemData
    (pure ())
 where
  sendSystemData = do
    systemMsg <- toMessage @Proto.SystemData @SystemData <$> mkSystemData Home
    val <- trySendMessage (env ^. router) RemoteProxy $ toProxyEnvelope systemMsg
    print val

mkRemoteProxyConnTLS ::
  FilePath
  -> FilePath
  -> FilePath
  -> HostName
  -> ServiceName
  -> Env
  -> EventLoop (Island, ExHomeHandler)
  -> IO ()
mkRemoteProxyConnTLS certPath keyPath caCertPath host port env loop = do
  let rtr = env ^. router
  mParams <- setupTLSClientParams certPath keyPath caCertPath
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
    val <- trySendMessage (env ^. router) RemoteProxy $ toProxyEnvelope systemMsg
    print val

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
  -> Port
  -> (Chan B.ByteString, Chan B.ByteString)
  -> IO ThreadId
mkLocalProxyThread certPath keyPath caCertPath port serverConn =
  forkIO $ bracket (Proxy.mkEnv LocalHTTP) Proxy.cleanupEnv $ \env -> do
    proxyMain
      certPath
      keyPath
      caCertPath
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
main = runCommand $ \(opts :: Home.Options.HomeOptions) _args -> do
  bracket mkEnv cleanupEnv $ \env ->
    runEventLoopT (action opts) env
 where
  action ::
    Home.Options.HomeOptions -> EventLoopT Env (Island, ExHomeHandler) IO ()
  action opts = do
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
            (opts ^. httpsCertificatePath)
            (opts ^. httpsKeyPath)
            (opts ^. httpsCACertificatePath)
            (opts ^. httpPort)
            serverConn
      )
      (liftIO . killThread)
      $ \_threadId -> do
        liftIO $ addLocalHostConnection env loop clientConn
        liftIO $
          mkRemoteProxyConnTLS
            (opts ^. tlsCertificatePath)
            (opts ^. tlsKeyPath)
            (opts ^. tlsCACertificatePath)
            (opts ^. proxyURL)
            (opts ^. proxyPort)
            env
            loop
        startCheckMemoryPoll
        start $ uncurry homeHandler
