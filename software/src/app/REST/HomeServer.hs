{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module REST.HomeServer (runApp, mkEnv, router) where

import Control.Concurrent (MVar, readMVar)
import Control.Exception (SomeAsyncException, catch, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Vector qualified as V
import Envelope (toEnvelope)
import Home.AudioStreamTypes (StationId, StreamStatus)
import Islands (Island (..))
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Logger (Logs, getLogs)
import Network.TLS (Version (..))
import Network.TLS.Extra (ciphersuite_strong)
import Network.Wai.Application.Static (
  defaultWebAppSettings,
  ssIndices,
  ssRedirectToIndex,
 )
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (
  TLSSettings (..),
  runTLS,
  tlsAllowedVersions,
  tlsSettings,
 )
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (toMessage)
import REST.Api (Api)
import Router (Router, trySendMessage)
import Servant (
  Handler,
  Proxy (Proxy),
  Raw,
  Server,
  serve,
  serveDirectoryWith,
  (:<|>) (..),
 )
import Servant.Server (Application)
import State (State, StateId, waitForStateUpdate, withState)
import System (SystemData)
import System.Memory (MemoryInformation)
import WaiAppStatic.Types (unsafeToPiece)
import Connection.TLS (mTLSHooks, loadCAStore)
import Network.Socket (PortNumber)
import Network.Socket (HostName)

data Env = Env
  { _streamStatusState :: State (StreamStatus, StationId)
  , _systemDataState :: MVar (Map.Map Island SystemData)
  , _memoryMap :: MVar (Map.Map Island (V.Vector MemoryInformation))
  , _router :: Router
  , _logs :: Logs
  }

$(makeLenses ''Env)

mkEnv ::
  State (StreamStatus, StationId)
  -> MVar (Map.Map Island SystemData)
  -> MVar (Map.Map Island (V.Vector MemoryInformation))
  -> Logs
  -> Router
  -> IO Env
mkEnv streamStatus systemState memMap logs' rtr = do
  pure $
    Env
      { _streamStatusState = streamStatus
      , _systemDataState = systemState
      , _memoryMap = memMap
      , _router = rtr
      , _logs = logs'
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
  pure $ toMessage sysMap

handleGetAllIslandsMemoryDataRequest :: Env -> Handler Proto.AllIslandMemoryData
handleGetAllIslandsMemoryDataRequest env = do
  memMap <- liftIO $ readMVar (env ^. memoryMap)
  pure . toMessage $ memMap

handleGetLogs :: Env -> Handler Proto.Logs
handleGetLogs env = do
  logsVec <- liftIO $ getLogs $ env ^. logs
  pure $
    defMessage
      & Proto.logs
      .~ V.toList logsVec

-- * Server

server :: Env -> Server Api
server env =
  ( ( handleGetRadioStatus env
        :<|> handleModifyRadioRequest env
    )
      :<|> handleGetSystemDataRequest env
      :<|> handleGetAllIslandsMemoryDataRequest env
      :<|> handleGetLogs env
  )
    :<|> serveDir "/usr/local/haskell-home-link"

-- | Serves the directory and uses index.html as the complete root.
serveDir :: FilePath -> Server Raw
serveDir path = do
  let
    initSettings = defaultWebAppSettings path
    staticSettings =
      initSettings
        { ssRedirectToIndex = False
        , ssIndices = [unsafeToPiece "index.html"]
        }
  serveDirectoryWith staticSettings

app :: Env -> Application
app env = serve (Proxy @Api) $ server env

runApp :: FilePath -> FilePath -> FilePath -> HostName -> PortNumber -> Env -> IO ()
runApp certPath keyPath caCertPAth hostname port env = do
  putStrLn $ "Starting HTTP server on port " <> show port
  caStore <- loadCAStore caCertPAth
  runAppImpl caStore `catch` handleAsyncException
 where
  runAppImpl caStore =
    runTLS (appTLSSettings caStore) settings $ app env

  appTLSSettings caStore =
    (tlsSettings certPath keyPath)
      { tlsAllowedVersions = [TLS13, TLS12]
      , tlsCiphers = ciphersuite_strong
      -- TODO Change back to true
      , tlsWantClientCert = False
      -- , tlsWantClientCert = True
      , tlsServerHooks = mTLSHooks hostname caStore
      }

  settings =
    defaultSettings
      & (setPort . fromEnum $ port)

  handleAsyncException (ex :: SomeAsyncException) = do
    putStrLn "Async exception caught. Killing HTTP server"
    throwIO ex
