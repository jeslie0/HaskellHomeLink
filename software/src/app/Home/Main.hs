{-# LANGUAGE OverloadedStrings #-}
module Home.Main where

import Control.Concurrent (isEmptyMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop, run)
import Home.AudioStream (stop)
import Home.Env (mkEnv, audioStream, connectionMVar)
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
                & Home.host .~ "127.0.0.1"
                & Home.port .~ "3000"
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
        addMsg loop startConnection
        -- addMsg loop startRadio
        run loop homeHandler

    cleanupEnv env = do
        stop $ env ^. audioStream
        isConnectionDead <- isEmptyMVar $ env ^. connectionMVar
        unless isConnectionDead $ do
            takeMVar (env ^. connectionMVar) >>= killAsyncComputation
