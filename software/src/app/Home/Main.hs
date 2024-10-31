module Home.Main where

import Control.Monad.Reader
import EventLoop (mkEventLoop, addMsg)
import Home.Env (Env, mkEnv)
import Home.Handler (ExHomeHandler(..), homeHandler)
import Proto.Radio qualified as Radio
import Data.ProtoLens (defMessage)
import Control.Concurrent (threadDelay)

start :: Radio.StartRadio
start = defMessage

stop :: Radio.StopRadio
stop = defMessage

main :: IO ()
main = do
    env <- mkEnv
    runReaderT action env
  where
    action = do
        evLoop <- mkEventLoop @(ExHomeHandler Env) homeHandler
        addMsg evLoop $ ExHomeHandler start
        liftIO $ threadDelay 10000000
        addMsg evLoop $ ExHomeHandler stop
        liftIO $ threadDelay 10000000
