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
import Home.AudioStream (StreamStatus, StationId)
import Lens.Micro
import Proto.Messages qualified as Proto
import Proxy.Env (
    Env,
    addLocalHTTPServerConnection,
    mkEnv,
    router,
    streamStatusState,
    systemMap,
 )
import Proxy.Handler (ExProxyHandler (..), proxyHandler)
import REST.HomeServer qualified as HTTP
import Router (Router, connectionsManager)
import State (State)
import Threads (killAsyncComputation, spawnAsyncComputation)
import System (SystemData)
import qualified Data.Map.Strict as Map

httpServer ::
    State (StreamStatus, StationId) -> MVar (Map.Map Island SystemData) -> Router -> IO ()
httpServer state systemData rtr = do
    env <- HTTP.mkEnv state systemData rtr
    HTTP.runApp env

proxyMain :: Island -> IO ()
proxyMain island = do
    bracket (mkEnv island) cleanupEnv $ \env ->
        bracket
            ( spawnAsyncComputation $
                httpServer (env ^. streamStatusState) (env ^. systemMap) (env ^. router)
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
