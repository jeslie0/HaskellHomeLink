{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module REST.HomeServer (runApp, mkEnv, radioStreamActive, router) where

import Control.Exception (SomeAsyncException, catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as B
import Data.ProtoLens (defMessage)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort)
import Proto.Home qualified as Home
import REST.Api (Api, RadioCommand (..))
import Servant (
    Handler,
    Proxy (Proxy),
    Raw,
    Server,
    serve,
    serveDirectoryWith,
    (:<|>) (..),
 )

import Connection (mkTCPServerConnection)
import Control.Concurrent (MVar, newMVar, withMVar)
import Lens.Micro ((^.), (&))
import Lens.Micro.TH (makeLenses)
import Network.Wai.Application.Static (
    defaultWebAppSettings,
    ssIndices,
    ssRedirectToIndex,
 )
import Servant.Server (Application)
import WaiAppStatic.Types (unsafeToPiece)
import Msg (ExMsg (..))
import Router (Router, mkRouter, connectionsManager)
import ConnectionManager (Island(..), addConnection)
import Home.Handler (toEnvelope)

type AddMsg = ExMsg -> IO ()

data Env = Env
    { _radioStreamActive :: MVar Bool
    , _router :: Router
    }

$(makeLenses ''Env)

mkEnv :: IO Env
mkEnv = do
    _radioStreamActive <- newMVar False
    _router <- mkRouter LocalHTTP
    tcpServerConnection <- mkTCPServerConnection "3000" B.putStrLn
    addConnection Home tcpServerConnection (_router ^. connectionsManager)
    pure $ Env {_radioStreamActive, _router}

handleRadioCommand :: AddMsg -> RadioCommand -> Handler Bool
handleRadioCommand addMsg Start = liftIO $ addMsg (ExMsg . toEnvelope $ defMessage @Home.StartRadio) >> pure True
handleRadioCommand addMsg Stop = liftIO $ addMsg (ExMsg . toEnvelope $ defMessage @Home.StopRadio) >> pure True

handleGetRadioStatus :: Env -> Handler Bool
handleGetRadioStatus env = do
    liftIO $ withMVar (env ^. radioStreamActive) pure

handleConnectionCommand :: AddMsg -> Handler Bool
handleConnectionCommand addMsg = liftIO $ addMsg (ExMsg . toEnvelope$ defMessage @Home.ConnectTCP) >> pure True

server :: Env -> AddMsg -> Server Api
server env addMsg =
    ( ( handleGetRadioStatus env
            :<|> handleRadioCommand addMsg Start
            :<|> handleRadioCommand addMsg Stop
      )
        :<|> handleConnectionCommand addMsg
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

app :: Env -> AddMsg -> Application
app env = serve (Proxy @Api) . server env

runApp :: Env -> AddMsg -> IO ()
runApp env addMsg = do
    putStrLn $ "Starting HTTP server on port " <> show port
    runAppImpl `catch` handleAsyncException
  where
    runAppImpl =
        runSettings settings $ app env addMsg

    settings = defaultSettings
      & setPort port

    port = 8080

    handleAsyncException (ex :: SomeAsyncException) = do
        putStrLn "Async exception caught. Killing HTTP server"
        throwIO ex
