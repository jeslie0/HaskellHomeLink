module Home.Main (main) where

import Co
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
  httpHostname,
  httpPort,
  httpsCACertificatePath,
  httpsCertificatePath,
  httpsKeyPath,
  proxyPort,
  proxyURL,
  tlsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
 )
import Islands (Island (..))
import Lens.Micro
import Network.Socket (HostName, PortNumber)
import Options (runCommand)
import Proto.Messages qualified as Proto
import ProtoHelper (toMessage)
import Proxy.Env qualified as Proxy
import Proxy.Handler (ExProxyHandler (..))
import Router (Router, connectionsManager, handleBytes, trySendMessage)
import System (SystemData, mkSystemData)

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

main :: IO ()
main = runCommand $ \(opts :: HomeOptions) _args -> do
  mConfig <- eitherDecodeFileStrict (opts ^. configPath)
  case mConfig of
    Left errs -> putStrLn $ "Could not parse configuration file: " <> errs
    Right config -> do
      bracket mkEnv cleanupEnv $ \env -> do
        runEventLoopT (action config) env
        writeFile "/mnt/normalexit" ""
 where
  action ::
    HomeConfiguration -> EventLoopT Env (Island, ExHomeHandler) IO ()
  action config = do
    loop <- getLoop
    env <- getEnv
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
