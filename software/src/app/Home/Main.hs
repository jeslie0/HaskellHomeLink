module Home.Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (EventLoop (..), mkEventLoop, _run)
import Home.AudioStream (stop)
import Home.Env (Env (..), mkEnv)
import Home.Handler (homeHandler)
import Lens.Micro
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio

envelopeStart :: Radio.Envelope
envelopeStart =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M3 defMessage

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
        loop <- mkEventLoop @Radio.Envelope homeHandler
        liftIO $ _addMsg loop envelopeStart
        _run loop

    cleanupEnv env = do
        stop . _audioStream $ env
