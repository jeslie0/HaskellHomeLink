module Home.Main (main) where

import ConnectionManager (addChannelsConnection, initTCPClientConnection)
import Control.Concurrent (Chan, newChan)
import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
import EventLoop (EventLoop, addMsg, mkEventLoop, run, setInterval)
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
  -> EventLoop (ReaderT Env IO) (Island, ExHomeHandler)
  -> (Chan B.ByteString, Chan B.ByteString)
  -> IO ()
addLocalHostConnection env loop clientConn =
  addChannelsConnection
    LocalHTTP
    (env ^. router . connectionsManager)
    clientConn
    ( handleBytes @Proto.HomeEnvelope
        (addMsg loop . second ExHomeHandler)
        (env ^. router)
    )
    Nothing

mkRemoteProxyConn ::
  Env
  -> EventLoop (ReaderT Env IO) (Island, ExHomeHandler)
  -> IO ()
mkRemoteProxyConn env loop =
  initTCPClientConnection
    RemoteProxy
    (env ^. router . connectionsManager)
    "127.0.0.1"
    "3001"
    ( handleBytes @Proto.HomeEnvelope
        (addMsg loop . second ExHomeHandler)
        (env ^. router)
    )
    (pure ())
    (pure ())

startCheckMemoryPoll ::
  EventLoop (ReaderT Env IO) (Island, ExHomeHandler)
  -> IO ()
startCheckMemoryPoll loop = do
  addMsg
    loop
    (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval loop (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

-- Use channels to communicate with local proxy
mkLocalProxyConn ::
  (Chan B.ByteString, Chan B.ByteString)
  -> IO ()
mkLocalProxyConn serverConn =
  bracket (Proxy.mkEnv LocalHTTP) Proxy.cleanupEnv $ \env -> do
    proxyMain
      env
      (mkChannelsConnection serverConn)
      (const $ pure ())
      LocalHTTP

mkChannelsConnection ::
  (Chan B.ByteString, Chan B.ByteString)
  -> Router
  -> EventLoop (ReaderT Proxy.Env IO) (Island, ExProxyHandler)
  -> IO ()
mkChannelsConnection serverConn rtr loop =
  addChannelsConnection
    Home
    (rtr ^. connectionsManager)
    serverConn
    (handleBytes @Proto.ProxyEnvelope (addMsg loop . second ExProxyHandler) rtr)
    Nothing

main :: IO ()
main = do
  bracket mkEnv cleanupEnv $ \env ->
    runReaderT (action env) env
 where
  action :: Env -> ReaderT Env IO ()
  action env = do
    ch1 <- liftIO newChan
    ch2 <- liftIO newChan
    let
      clientConn = (ch1, ch2)
      serverConn = (ch2, ch1)

    loop <- mkEventLoop @(Island, ExHomeHandler)
    liftIO $ mkLocalProxyConn serverConn
    liftIO $ addLocalHostConnection env loop clientConn
    liftIO $ mkRemoteProxyConn env loop
    liftIO $ startCheckMemoryPoll loop

    run loop $ \evloop b -> uncurry (homeHandler evloop) b
