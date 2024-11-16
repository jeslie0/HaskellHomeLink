{-# LANGUAGE OverloadedStrings #-}

module Home.Main where

import Control.Concurrent (putMVar, tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop, run)
import Home.Env (audioStreamMVar, connectionMVar, httpServerMVar, mkEnv)
import Home.Handler (homeHandler, toEnvelope)
import Lens.Micro
import Proto.Home qualified as Home
import Proto.Home_Fields qualified as Home
import REST.HomeServer (runApp)
import Threads (killAsyncComputation, spawnAsyncComputation)

startConnection :: Home.Envelope
startConnection =
    toEnvelope
        ( defMessage @Home.ConnectTCP
            & Home.host
            .~ "127.0.0.1"
            & Home.port
            .~ "3000"
        )

startRadio :: Home.Envelope
startRadio =
    toEnvelope $ defMessage @Home.StartRadio

stopRadio :: Home.Envelope
stopRadio =
    toEnvelope $ defMessage @Home.StopRadio

main :: IO ()
main = do
    bracket mkEnv cleanupEnv $ \env ->
        runReaderT (action env) env
  where
    action env = do
        loop <- mkEventLoop @Home.Envelope
        httpServerAsyncComp <- liftIO . spawnAsyncComputation $ runApp (addMsg loop)
        liftIO . putMVar (env ^. httpServerMVar) $ httpServerAsyncComp
        run loop homeHandler

    cleanupEnv env = do
        tryTakeMVar (env ^. audioStreamMVar) >>= \m -> for_ m killAsyncComputation
        tryTakeMVar (env ^. connectionMVar) >>= \m -> for_ m killAsyncComputation
        tryTakeMVar (env ^. httpServerMVar) >>= \m -> for_ m killAsyncComputation
