module Home.Main where

import Control.Monad.Reader
import Data.Serialize qualified as Binary
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
import EventLoop (addMsg, mkEventLoop)
import Home.Env (mkEnv)
import Home.Handler (homeHandler)
import Lens.Micro
import Msg (fromBytes, Msg (..))
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio
import Socket (addSubscriber, makeClientSocketHandler, killSocketHandler, makeServerSocketHandler, sendMsg)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted (threadDelay)
import Data.Word (Word32)
import Data.ProtoLens.Encoding

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
    forkIO $ void $ do
      serverSockHand <- makeServerSocketHandler "3005"
      threadDelay 2000000
      let bytes = encodeMessage envelopeStart
          bytesSize = fromIntegral @_ @Word32 $ B.length bytes
      putStrLn $ "msg length: " <> show (B.length bytes)
      sendMsg serverSockHand $ Binary.runPut $ Binary.putWord32le bytesSize
      sendMsg serverSockHand $ bytes
    threadDelay 1000000
    sockHandler <- makeClientSocketHandler "127.0.0.1" "3005"
    env <- mkEnv sockHandler
    runReaderT (action sockHandler) env
  where
    action sockHandler = do
        loop <- mkEventLoop @Radio.Envelope homeHandler
        let sub bytes = do
                case fromBytes @Radio.Envelope bytes of
                    Left err -> putStrLn err
                    Right msg -> do
                      print "got msg!"
                      addMsg loop msg
        liftIO $ addSubscriber sockHandler sub
        _ <- liftIO getLine
        liftIO $ killSocketHandler sockHandler

