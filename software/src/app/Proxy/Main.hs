module Proxy.Main (proxyMain) where

import ConnectionManager (
    Island (..),
    killConnections,
 )
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Bifunctor (second)
import EventLoop (addMsg, mkEventLoop, run)
import Home.AudioStream (StreamStatus)
import Lens.Micro
import Proto.Proxy qualified as Proxy
import Proxy.Env (
    Env,
    addLocalHTTPServerConnection,
    mkEnv,
    router,
    streamStatusState,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import REST.HomeServer qualified as HTTP
import Router (Router, connectionsManager)
import State (State)
import Threads (killAsyncComputation, spawnAsyncComputation)

httpServer :: State StreamStatus -> Router -> IO ()
httpServer state rtr = do
    env <- HTTP.mkEnv state rtr
    HTTP.runApp env

proxyMain :: Island -> IO ()
proxyMain island = do
    bracket (mkEnv island) cleanupEnv $ \env ->
        bracket
            (spawnAsyncComputation $ httpServer (env ^. streamStatusState) (env ^. router))
            killAsyncComputation
            $ \_async ->
                runReaderT (action env) env
  where
    action :: Env -> ReaderT Env IO ()
    action env = do
        loop <- mkEventLoop @(Island, ExProxyHandler)
        liftIO
            $ addLocalHTTPServerConnection @Proxy.ProxyRecieveEnvelope
                (addMsg loop . second ExProxyHandler)
            $ env ^. router
        run loop $ \evloop b -> uncurry (proxyHandler evloop) b

    cleanupEnv env = do
        killConnections (env ^. (router . connectionsManager))

-- main :: IO ()
-- main = proxyMain RemoteProxy
