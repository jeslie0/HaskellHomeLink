{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.REST.HomeServer (runApp, mkEnv, router) where

import Proto.Logging qualified as Proto
import Proto.Logging_Fields qualified as Proto
import Proto.Radio qualified as Proto
import Proto.Radio_Fields qualified as Proto
import Proto.DeviceData qualified as Proto
import Proto.DeviceData_Fields qualified as Proto

import TLSHelper (loadCAStore, mTLSHooks)
import Control.Concurrent (MVar, readMVar)
import Control.Exception (SomeAsyncException, catch, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Vector qualified as V
import Envelope (toEnvelope)
import Home.AudioStreamTypes (StationId, StreamStatus)
import Devices qualified as Dev
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Logger (Logs, getLogs)
import Network.Socket (HostName, PortNumber)
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
import Network.WebSockets qualified as WS
import ProtoHelper (toMessage)
import Proxy.WebsocketServer (websocketServer)
import Proxy.REST.Api (Api)
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
import System (DeviceData)
import System.Memory (MemoryInformation)
import WaiAppStatic.Types (unsafeToPiece)

data Env = Env
  { _streamStatusState :: State (StreamStatus, StationId)
  , _deviceDataState :: MVar (Map.Map Dev.Device DeviceData)
  , _memoryMap :: MVar (Map.Map Dev.Device (V.Vector MemoryInformation))
  , _wsConns :: MVar (Map.Map Int32 WS.Connection)
  , _router :: Router
  , _logs :: Logs
  }

$(makeLenses ''Env)

mkEnv ::
  State (StreamStatus, StationId)
  -> MVar (Map.Map Dev.Device DeviceData)
  -> MVar (Map.Map Dev.Device (V.Vector MemoryInformation))
  -> MVar (Map.Map Int32 WS.Connection)
  -> Logs
  -> Router
  -> IO Env
mkEnv streamStatus systemState memMap wsConns' logs' rtr = do
  pure $
    Env
      { _streamStatusState = streamStatus
      , _deviceDataState = systemState
      , _memoryMap = memMap
      , _wsConns = wsConns'
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
        Dev.Home
        (toEnvelope req)
  pure True

handleGetDeviceDataRequest :: Env -> Handler Proto.AllDeviceData
handleGetDeviceDataRequest env = do
  devMap <- liftIO $ readMVar (env ^. deviceDataState)
  pure $ toMessage devMap

handleGetAllDevicesMemoryDataRequest :: Env -> Handler Proto.AllDeviceMemoryData
handleGetAllDevicesMemoryDataRequest env = do
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
      :<|> handleGetDeviceDataRequest env
      :<|> handleGetAllDevicesMemoryDataRequest env
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
app env =
  websocketServer (env ^. wsConns) $
    serve (Proxy @Api) $
      server env

runApp ::
  FilePath -> FilePath -> FilePath -> HostName -> PortNumber -> Env -> IO ()
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
      , tlsWantClientCert = True
      , tlsServerHooks = mTLSHooks hostname caStore
      }

  settings =
    defaultSettings
      & (setPort . fromEnum $ port)

  handleAsyncException (ex :: SomeAsyncException) = do
    putStrLn "Async exception caught. Killing HTTP server"
    throwIO ex
