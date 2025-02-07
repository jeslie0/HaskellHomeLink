module Home.Main (main) where

import ConnectionManager (addChannelsConnection, initTCPClientConnection)
import Control.Concurrent (Chan, ThreadId, forkIO, killThread, newChan)
import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
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
import Islands (Island (..))
import Lens.Micro
import Proto.Messages qualified as Proto
import Proxy.Env qualified as Proxy
import Proxy.Handler (ExProxyHandler (..))
import Proxy.Main (proxyMain)
import Router (Router, connectionsManager, handleBytes)

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
    Nothing

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
    (pure ())
    (pure ())

startCheckMemoryPoll ::
  EventLoopT Env (Island, ExHomeHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

-- Use channels to communicate with local proxy
mkLocalProxyThread ::
  (Chan B.ByteString, Chan B.ByteString)
  -> IO ThreadId
mkLocalProxyThread serverConn =
  forkIO $ bracket (Proxy.mkEnv LocalHTTP) Proxy.cleanupEnv $ \env -> do
    proxyMain
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
    Nothing

main :: IO ()
main = do
  bracket mkEnv cleanupEnv $ \env ->
    runEventLoopT action env
 where
  action :: EventLoopT Env (Island, ExHomeHandler) IO ()
  action = do
    ch1 <- liftIO newChan
    ch2 <- liftIO newChan
    let
      clientConn = (ch1, ch2)
      serverConn = (ch2, ch1)

    loop <- getLoop
    env <- getEnv
    bracket (liftIO $ mkLocalProxyThread serverConn) (liftIO . killThread) $ \_threadId -> do
      liftIO $ addLocalHostConnection env loop clientConn
      liftIO $ mkRemoteProxyConn env loop
      startCheckMemoryPoll
      start $ uncurry homeHandler
