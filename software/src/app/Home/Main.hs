module Home.Main (main) where

import ConnectionManager (addChannelsConnection, initTCPClientConnection)
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
import Home.Options (HomeOptions (..), httpsCertificatePath, httpsKeyPath)
import Islands (Island (..))
import Lens.Micro
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

mkRemoteProxyConn ::
  Env
  -> EventLoop (Island, ExHomeHandler)
  -> IO ()
mkRemoteProxyConn env loop =
  initTCPClientConnection
    RemoteProxy
    (env ^. router . connectionsManager)
    "127.0.0.1"
    "3001"
    ( handleBytes @Proto.HomeEnvelope
        (\(island, msg) -> addMsgIO (island, ExHomeHandler msg) loop)
        (env ^. router)
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
  -> (Chan B.ByteString, Chan B.ByteString)
  -> IO ThreadId
mkLocalProxyThread certPath keyPath serverConn =
  forkIO $ bracket (Proxy.mkEnv LocalHTTP) Proxy.cleanupEnv $ \env -> do
    proxyMain
      certPath
      keyPath
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
  bracket mkEnv cleanupEnv $ \env ->
    runEventLoopT (action opts) env
 where
  action :: HomeOptions -> EventLoopT Env (Island, ExHomeHandler) IO ()
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
            serverConn
      )
      (liftIO . killThread) $ \_threadId -> do
      liftIO $ addLocalHostConnection env loop clientConn
      liftIO $ mkRemoteProxyConn env loop
      startCheckMemoryPoll
      start $ uncurry homeHandler
