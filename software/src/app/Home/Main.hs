module Home.Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (mkEventLoop, run, addMsg)
import Home.AudioStream (stop)
import Home.Env (Env (..), mkEnv)
import Home.Handler (homeHandler)
import Lens.Micro
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio
import ThreadPool (killThreadPool, killAsyncComputation)
import Control.Concurrent (isEmptyMVar, takeMVar)
import Control.Monad (unless)

startConnection :: Radio.Envelope
startConnection =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M3 defMessage

startRadio :: Radio.Envelope
startRadio =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M1 defMessage

stopRadio :: Radio.Envelope
stopRadio =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M2 defMessage

main :: IO ()
main = do
    bracket mkEnv cleanupEnv $ \env ->
        runReaderT action env
  where
    action = do
        loop <- mkEventLoop @Radio.Envelope
        addMsg loop startConnection
        -- addMsg loop startRadio
        run loop homeHandler

    cleanupEnv env = do
        putStrLn "Running cleanup"
        stop . _audioStream $ env
        putStrLn "1"
        isConnectionDead <- isEmptyMVar $  _connectionMVar env
        putStrLn "3"
        unless isConnectionDead $ do
          takeMVar (_connectionMVar env) >>= killAsyncComputation
        putStrLn "Cleanup successful"
