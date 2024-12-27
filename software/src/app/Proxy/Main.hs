module Proxy.Main (proxyMain) where

import ConnectionManager (
    Island (..),
    killConnections,
 )
import Control.Concurrent (MVar)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Bifunctor (second)
import EventLoop (addMsg, mkEventLoop, run)
import Home.AudioStream (StreamStatus)
import Lens.Micro
import Proto.Messages qualified as Proto
import Proxy.Env (
    Env,
    addLocalHTTPServerConnection,
    mkEnv,
    router,
    streamStatusState,
    systemDataState,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import REST.HomeServer qualified as HTTP
import Router (Router, connectionsManager)
import State (State)
import Threads (killAsyncComputation, spawnAsyncComputation)

httpServer ::
    State StreamStatus -> MVar Proto.SystemDataMessage -> Router -> IO ()
httpServer state systemData rtr = do
    env <- HTTP.mkEnv state systemData rtr
    HTTP.runApp env

proxyMain :: Island -> IO ()
proxyMain island = do
    bracket (mkEnv island) cleanupEnv $ \env ->
        bracket
            ( spawnAsyncComputation $
                httpServer (env ^. streamStatusState) (env ^. systemDataState) (env ^. router)
            )
            killAsyncComputation
            $ \_async ->
                runReaderT (action env) env
  where
    action :: Env -> ReaderT Env IO ()
    action env = do
        loop <- mkEventLoop @(Island, ExProxyHandler)
        liftIO
            $ addLocalHTTPServerConnection @Proto.ProxyEnvelope
                (addMsg loop . second ExProxyHandler)
            $ env ^. router
        run loop $ \evloop b -> uncurry (proxyHandler evloop) b

    cleanupEnv env = do
        killConnections (env ^. (router . connectionsManager))

-- main :: IO ()
-- main = proxyMain RemoteProxy
