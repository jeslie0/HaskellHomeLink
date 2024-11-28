{-# LANGUAGE OverloadedStrings #-}

module Home.Main (main) where

import Connection (
    killConnection,
    mkTCPClientConnection,
    sendMsg,
 )
import Control.Concurrent (modifyMVar_, putMVar, tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Foldable (for_)
import EventLoop (addMsg, mkEventLoop, run)
import Home.Env (audioStreamMVar, connectionMVar, httpServerMVar, mkEnv)
import Home.Handler (ExHomeHandler (..), homeHandler)
import Lens.Micro
import Msg (ExMsg (..), Msg (..))
import Proto.Home qualified as Home
import REST.HomeServer (tcpServer)
import REST.HomeServer qualified as HomeServer (mkEnv, runApp)
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
    HomeServer.runApp env $ \(ExMsg msg) -> sendMsg (env ^. tcpServer) (toBytes msg)

main :: IO ()
main = do
    bracket (spawnAsyncComputation httpServer) killAsyncComputation $ \_ ->
        bracket mkEnv cleanupEnv $ \env ->
            runReaderT (action env) env
  where
    action env = do
        loop <- mkEventLoop @ExHomeHandler
        connection <- liftIO $ mkTCPClientConnection "127.0.0.1" "3000" $ \bytes ->
            case fromBytes @Home.StartRadio bytes of
                Left _ -> putStrLn "FAIL"
                Right msg -> addMsg loop $ ExHomeHandler msg
        liftIO . putMVar (env ^. connectionMVar) $ connection
        run loop homeHandler

    cleanupEnv env = do
        modifyMVar_ (env ^. audioStreamMVar) $ \m -> for_ m killAsyncComputation >> pure Nothing
        tryTakeMVar (env ^. connectionMVar) >>= \m -> for_ m killConnection
        tryTakeMVar (env ^. httpServerMVar) >>= \m -> for_ m killAsyncComputation
