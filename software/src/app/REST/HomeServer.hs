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
import Control.Concurrent (MVar, readMVar)
import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Envelope (toEnvelope)
import Home.AudioStream (StreamId, StreamStatus)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Network.Wai.Application.Static (
    defaultWebAppSettings,
    ssIndices,
    ssRedirectToIndex,
 )
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (toMessage)
import Router (Router, trySendMessage)
import Servant.Server (Application)
import State (State, StateId, waitForStateUpdate, withState)
import System (SystemData)
import WaiAppStatic.Types (unsafeToPiece)

data Env = Env
    { _streamStatusState :: State (StreamStatus, StreamId)
    , _systemDataState :: MVar (Map.Map Island SystemData)
    , _router :: Router
    }

$(makeLenses ''Env)

mkEnv ::
    State (StreamStatus, StreamId)
    -> MVar (Map.Map Island SystemData)
    -> Router
    -> IO Env
mkEnv streamStatus systemState rtr = do
    pure $
        Env
            { _streamStatusState = streamStatus
            , _systemDataState = systemState
            , _router = rtr
            }

-- * Server handlers

handleGetRadioStatus :: Env -> Handler Proto.GetRadioStatusResponse
handleGetRadioStatus env = do
    liftIO $ withState (env ^. streamStatusState) $ \stateId (status, stationId) ->
        pure $
            defMessage
                & Proto.stateId
                .~ stateId
                & Proto.status
                .~ toMessage status
                & Proto.currentStationId
                .~ stationId

handleModifyRadioRequest ::
    Env -> Proto.ModifyRadioRequest -> Maybe StateId -> Handler Bool
handleModifyRadioRequest _ _ Nothing = pure False
handleModifyRadioRequest env req (Just stateId) = do
    void . liftIO $ waitForStateUpdate (env ^. streamStatusState) stateId $ \_ _ -> do
        void $
            trySendMessage
                (env ^. router)
                Home
                (toEnvelope req)
    pure True

handleGetSystemDataRequest :: Env -> Handler Proto.IslandsSystemData
handleGetSystemDataRequest env = do
    sysMap <- liftIO $ readMVar (env ^. systemDataState)
    pure . toMessage $ sysMap

-- * Server
server :: Env -> Server Api
server env =
    ( ( handleGetRadioStatus env
            :<|> handleModifyRadioRequest env
      )
        :<|> handleGetSystemDataRequest env
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
