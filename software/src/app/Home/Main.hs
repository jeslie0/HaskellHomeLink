module Home.Main (main) where

import ConnectionManager (
    Island (..),
    killConnections,
 )
import Control.Concurrent (modifyMVar_, tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (for_)
import EventLoop (addMsg, mkEventLoop, run)
import Home.Env (
    Env,
    addLocalHTTPServerConnection,
    audioStreamMVar,
    httpServerMVar,
    mkEnv,
    router,
 )
import Home.Handler (ExHomeHandler (..), homeHandler)
import Lens.Micro
import Msg (ExMsg (..))
import Proto.Home qualified as Home
import REST.HomeServer qualified as HomeServer
import Router (connectionsManager, trySendMessage)
import Threads (killAsyncComputation, spawnAsyncComputation)

-- startConnection :: Home.Envelope
-- startConnection =
--     toEnvelope
--         ( defMessage @Home.ConnectTCP
--             & Home.host
--             .~ "127.0.0.1"
--             & Home.port
--             .~ "3000"
--         )

-- startRadio :: Home.Envelope
-- startRadio =
--     toEnvelope $ defMessage @Home.StartRadio

-- stopRadio :: Home.Envelope
-- stopRadio =
--     toEnvelope $ defMessage @Home.StopRadio

httpServer :: IO ()
httpServer = do
    env <- HomeServer.mkEnv
    HomeServer.runApp env $ \(ExMsg msg) -> do
        void $ trySendMessage (env ^. HomeServer.router) Home msg

main :: IO ()
main = do
    bracket (spawnAsyncComputation httpServer) killAsyncComputation $ \_ ->
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
        tryTakeMVar (env ^. httpServerMVar) >>= \m -> for_ m killAsyncComputation
