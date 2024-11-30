module Home.Main (main) where

import ConnectionManager (
    Island (..),
    killConnections,
 )
import Control.Concurrent (modifyMVar_)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (for_)
import EventLoop (addMsg, mkEventLoop, run)
import Home.Env (
    Env,
    addLocalHTTPServerConnection,
    audioStreamMVar,
    mkEnv,
    router,
 )
import Home.Handler (ExHomeHandler (..), homeHandler)
import Lens.Micro
import Proto.Home qualified as Home
import Proxy.Main (proxyMain)
import Router (connectionsManager)
import Threads (killAsyncComputation, spawnAsyncComputation)

-- httpServer :: IO ()"
-- httpServer = do
--     env <- HomeServer.mkEnv
--     HomeServer.runApp env $ \(ExMsg msg) -> do
--         void $ trySendMessage (env ^. HomeServer.router) Home msg

main :: IO ()
main = do
    spawnAsyncComputation (proxyMain LocalHTTP)
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
        modifyMVar_ (env ^. audioStreamMVar) $ \m -> for_ m killAsyncComputation >> pure Nothing
        killConnections (env ^. (router . connectionsManager))
