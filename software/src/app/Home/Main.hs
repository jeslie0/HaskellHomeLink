module Home.Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop)
import Home.Env (Env, mkEnv)
import Home.Handler (ExHomeHandler (..), homeHandler)
import Lens.Micro
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio

start :: Radio.StartRadio
start = defMessage

stop :: Radio.StopRadio
stop = defMessage

envelopeStart :: Radio.Envelope
envelopeStart =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M1 start

envelopeStop :: Radio.Envelope
envelopeStop =
    defMessage
        & Radio.maybe'payload
        ?~ Radio.Envelope'M2 stop

main :: IO ()
main = do
    env <- mkEnv
    runReaderT action env
  where
    action = do
        evLoop <- mkEventLoop @(ExHomeHandler Env) homeHandler
        addMsg evLoop $ ExHomeHandler envelopeStart
        liftIO $ threadDelay 10000000
        addMsg evLoop $ ExHomeHandler envelopeStop
        liftIO $ threadDelay 10000000
