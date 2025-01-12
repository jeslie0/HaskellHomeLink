module Home.Main (main) where

import ConnectionManager (
  killConnections,
 )
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (for_)
import Data.IORef (readIORef, writeIORef)
import EventLoop (addMsg, mkEventLoop, run, setInterval)
import Home.AudioStream (StreamStatus (Off))
import Home.Env (
  Env,
  addLocalHTTPServerConnection,
  audioStreamRef,
  mkEnv,
  router,
 )
import Home.Handler (ExHomeHandler (..), homeHandler)
import Islands (Island (..))
import Lens.Micro
import Proto.Messages qualified as Proto
import Proxy.Main (proxyMain)
import Router (connectionsManager)
import Control.Monad (void)
import Data.ProtoLens (defMessage)

main :: IO ()
main = do
  bracket (forkIO (proxyMain LocalHTTP)) killThread $ \_ ->
    bracket mkEnv cleanupEnv $ \env ->
      runReaderT (action env) env
 where
  action :: Env -> ReaderT Env IO ()
  action env = do
    loop <- mkEventLoop @(Island, ExHomeHandler)
    liftIO
      $ addLocalHTTPServerConnection @Proto.HomeEnvelope
        (addMsg loop . second ExHomeHandler)
      $ env ^. router

    void . liftIO . addMsg loop $ (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
    void . liftIO $ setInterval loop (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $ 30 * 1000
    run loop $ \evloop b -> uncurry (homeHandler evloop) b

  cleanupEnv env = do
    (mThread, _, _) <- readIORef (env ^. audioStreamRef)
    for_ mThread killThread
    writeIORef (env ^. audioStreamRef) (Nothing, Off, 0)
    killConnections (env ^. (router . connectionsManager))
