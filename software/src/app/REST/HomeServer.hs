{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module REST.HomeServer (runApp, mkEnv, router) where

import Control.Exception (SomeAsyncException, catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ProtoLens (defMessage)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import REST.Api (Api)
import Servant (
    Handler,
    Proxy (Proxy),
    Raw,
    Server,
    serve,
    serveDirectoryWith,
    (:<|>) (..),
 )

import ConnectionManager (Island (..))
import Control.Monad (void)
import Home.AudioStream (StreamStatus)
import Envelope (toEnvelope)
import Lens.Micro ((&), (^.))
import Lens.Micro.TH (makeLenses)
import Network.Wai.Application.Static (
    defaultWebAppSettings,
    ssIndices,
    ssRedirectToIndex,
 )
import Proto.Home qualified as Home
import Proto.Proxy qualified as Proxy
import Proto.Proxy_Fields qualified as Proxy
import ProtoHelper (streamStatusToprotoRadioStatusResponse)
import Router (Router, trySendMessage)
import Servant.Server (Application)
import State (State, waitForStateUpdate, withState, StateId)
import WaiAppStatic.Types (unsafeToPiece)

data Env = Env
    { _streamStatusState :: State StreamStatus
    , _router :: Router
    }

$(makeLenses ''Env)

mkEnv :: State StreamStatus -> Router -> IO Env
mkEnv streamStatus rtr = do
    pure $ Env {_streamStatusState = streamStatus, _router = rtr}

-- * Server handlers

handleGetRadioStatus :: Env -> Handler Proxy.GetRadioStatusResponse
handleGetRadioStatus env = do
    liftIO $ withState (env ^. streamStatusState) $ \stateId state -> pure . streamStatusToprotoRadioStatusResponse stateId $ state

handleModifyRadioRequest ::
    Env -> Proxy.ModifyRadioRequest -> Maybe StateId -> Handler Bool
handleModifyRadioRequest _ _ Nothing = pure False
handleModifyRadioRequest env req (Just stateId) = do
    void . liftIO $ waitForStateUpdate (env ^. streamStatusState) stateId $ \_ _ ->
        void $
            if req ^. Proxy.start
                then do
                    trySendMessage (env ^. router) Home (toEnvelope $ defMessage @Home.StartRadio)
                else
                    trySendMessage (env ^. router) Home (toEnvelope $ defMessage @Home.StopRadio)
    pure True

-- * Server
server :: Env -> Server Api
server env =
    ( handleGetRadioStatus env
        :<|> handleModifyRadioRequest env
    )
        :<|> serveDir "/usr/local/haskell-home-link"

-- | Serves the directory and uses index.html as the complete root.
serveDir :: FilePath -> Server Raw
serveDir path = do
    let initSettings = defaultWebAppSettings path
        staticSettings =
            initSettings
                { ssRedirectToIndex = False
                , ssIndices = [unsafeToPiece "index.html"]
                }
    serveDirectoryWith staticSettings

app :: Env -> Application
app env = serve (Proxy @Api) $ server env

runApp :: Env -> IO ()
runApp env = do
    putStrLn $ "Starting HTTP server on port " <> show port
    runAppImpl `catch` handleAsyncException
  where
    runAppImpl =
        runSettings settings $ app env

    settings =
        defaultSettings
            & setPort port

    port = 8080

    handleAsyncException (ex :: SomeAsyncException) = do
        putStrLn "Async exception caught. Killing HTTP server"
        throwIO ex
