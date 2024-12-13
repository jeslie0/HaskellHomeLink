module Home.Main (main) where

import ConnectionManager (
    Island (..),
    killConnections,
 )
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (for_)
import Data.IORef (readIORef, writeIORef)
import EventLoop (addMsg, mkEventLoop, run)
import Home.Env (
    Env,
    addLocalHTTPServerConnection,
    audioStreamRef,
    mkEnv,
    router,
 )
import Home.Handler (ExHomeHandler (..), homeHandler)
import Lens.Micro
import Proto.Home qualified as Home
import Proxy.Main (proxyMain)
import Router (connectionsManager)
import Threads (killAsyncComputation, spawnAsyncComputation)

main :: IO ()
main = do
    void $ spawnAsyncComputation (proxyMain LocalHTTP)
    bracket mkEnv cleanupEnv $ \env ->
        runReaderT (action env) env
  where
    action :: Env -> ReaderT Env IO ()
    action env = do
        loop <- mkEventLoop @(Island, ExHomeHandler)
        liftIO
            $ addLocalHTTPServerConnection @Home.Envelope
                (addMsg loop . second ExHomeHandler)
            $ env ^. router
        run loop $ \evloop b -> uncurry (homeHandler evloop) b

    cleanupEnv env = do
        mAudioStream <- readIORef (env ^. audioStreamRef)
        for_ mAudioStream killAsyncComputation
        writeIORef (env ^. audioStreamRef) Nothing
        killConnections (env ^. (router . connectionsManager))
