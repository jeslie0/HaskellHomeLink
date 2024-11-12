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
import ThreadPool (killThreadPool)

envelopeStart :: Radio.Envelope
envelopeStart =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M1 defMessage

envelopeStop :: Radio.Envelope
envelopeStop =
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
        addMsg loop envelopeStart
        run loop homeHandler

    cleanupEnv env = do
        stop . _audioStream $ env
        killThreadPool . _threadPool $ env
