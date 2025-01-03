module Home.Main (main) where

import ConnectionManager (
    Island (..),
    killConnections,
 )
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (for_)
import Data.IORef (readIORef, writeIORef)
import EventLoop (addMsg, mkEventLoop, run)
import Home.AudioStream (StreamStatus (Off))
import Home.Env (
    Env,
    addLocalHTTPServerConnection,
    audioStreamRef,
    mkEnv,
    router,
 )
import Home.Handler (ExHomeHandler (..), homeHandler)
import Lens.Micro
import Proto.Messages qualified as Proto
import Proxy.Main (proxyMain)
import Router (connectionsManager)

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
        run loop $ \evloop b -> uncurry (homeHandler evloop) b

    cleanupEnv env = do
        (mThread, _, _) <- readIORef (env ^. audioStreamRef)
        for_ mThread killThread
        writeIORef (env ^. audioStreamRef) (Nothing, Off, 0)
        killConnections (env ^. (router . connectionsManager))
