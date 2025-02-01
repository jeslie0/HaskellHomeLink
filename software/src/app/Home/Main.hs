module Home.Main (main) where

import ConnectionManager (
  addChannelsConnection,
  killConnections,
 )
import Control.Concurrent (forkIO, killThread, newChan)
import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (for_)
import Data.IORef (readIORef, writeIORef)
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop, run, setInterval)
import Home.AudioStream (StreamStatus (Off))
import Home.Env (
  Env,
  audioStreamRef,
  mkEnv,
  router,
 )
import Home.Handler (ExHomeHandler (..), homeHandler)
import Islands (Island (..))
import Lens.Micro
import Proto.Messages qualified as Proto
import Proxy.Handler (ExProxyHandler (..))
import Proxy.Main (proxyMain)
import Router (connectionsManager, handleBytes)

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

    bracket (liftIO $ mkLocalProxyConn serverConn) (liftIO . killThread) $ \_ -> do
      liftIO $
        addChannelsConnection
          LocalHTTP
          (env ^. router . connectionsManager)
          clientConn
          ( handleBytes @Proto.HomeEnvelope
              (addMsg loop . second ExHomeHandler)
              (env ^. router)
          )

      void . liftIO . addMsg loop $
        (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
      void . liftIO $
        setInterval loop (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
          30 * 1000

      run loop $ \evloop b -> uncurry (homeHandler evloop) b

  mkLocalProxyConn serverConn =
    forkIO $
      proxyMain
        ( \rtr loop ->
            addChannelsConnection
              Home
              (rtr ^. connectionsManager)
              serverConn
              (handleBytes @Proto.ProxyEnvelope (addMsg loop . second ExProxyHandler) rtr)
        )
        (const $ pure ())
        LocalHTTP

  cleanupEnv env = do
    (mThread, _, _) <- readIORef (env ^. audioStreamRef)
    for_ mThread killThread
    writeIORef (env ^. audioStreamRef) (Nothing, Off, 0)
    killConnections (env ^. (router . connectionsManager))
