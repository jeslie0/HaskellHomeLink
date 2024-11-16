{-# LANGUAGE OverloadedStrings #-}

module Home.Main where

import Control.Concurrent (tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop, run)
import Home.Env (audioStreamMVar, connectionMVar, mkEnv)
import Home.Handler (homeHandler)
import Lens.Micro
import Proto.Home qualified as Home
import Proto.Home_Fields qualified as Home
import Threads (killAsyncComputation)

startConnection :: Home.Envelope
startConnection =
    defMessage
        & Home.maybe'payload
        ?~ Home.Envelope'M3
            ( defMessage
                & Home.host
                .~ "127.0.0.1"
                & Home.port
                .~ "3000"
            )

startRadio :: Home.Envelope
startRadio =
    defMessage
        & Home.maybe'payload
        ?~ Home.Envelope'M1 defMessage

stopRadio :: Home.Envelope
stopRadio =
    defMessage
        & Home.maybe'payload
        ?~ Home.Envelope'M2 defMessage

main :: IO ()
main = do
    bracket mkEnv cleanupEnv $ \env ->
        runReaderT action env
  where
    action = do
        loop <- mkEventLoop @Home.Envelope
        addMsg loop startRadio
        addMsg loop startConnection
        run loop homeHandler

    cleanupEnv env = do
        tryTakeMVar (env ^. audioStreamMVar) >>= \m -> for_ m killAsyncComputation
        tryTakeMVar (env ^. connectionMVar) >>= \m -> for_ m killAsyncComputation
