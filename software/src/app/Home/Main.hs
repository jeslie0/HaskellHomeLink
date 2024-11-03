module Home.Main where

import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop)
import Home.Env (mkEnv)
import Home.Handler (homeHandler)
import Lens.Micro
import Msg (fromBytes)
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio
import Socket (addSubscriber, makeClientSocketHandler, killSocketHandler)

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
    sockHandler <- makeClientSocketHandler "127.0.0.1" "3000"
    env <- mkEnv sockHandler
    runReaderT (action sockHandler) env
  where
    action sockHandler = do
        loop <- mkEventLoop @Radio.Envelope homeHandler
        let sub bytes = do
                case fromBytes @Radio.Envelope bytes of
                    Left err -> putStrLn err
                    Right msg -> addMsg loop msg
        liftIO $ addSubscriber sockHandler sub
        liftIO getLine
        liftIO $ killSocketHandler sockHandler
