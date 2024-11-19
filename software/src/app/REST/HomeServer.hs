{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module REST.HomeServer (runApp, mkEnv, radioStreamActive, tcpServer) where

import Control.Exception (SomeAsyncException, bracket, catch)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as B
import Data.ProtoLens (defMessage)
import Home.Handler (ExHomeHandler (..))
import Network.Wai.Handler.Warp (run)
import Proto.Home qualified as Home
import REST.Api (Api, RadioCommand (..))
import Servant (
    Handler,
    Proxy (Proxy),
    Raw,
    Server,
    serve,
    serveDirectoryWebApp,
    serveDirectoryWith,
    (:<|>) (..),
 )

import Connection (Connection, mkTCPServerConnection)
import Control.Concurrent (MVar, newMVar, tryPutMVar, tryTakeMVar, withMVar)
import Control.Monad (void)
import Home.Env qualified as Home
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Network.Wai.Application.Static (
    defaultWebAppSettings,
    ssIndices,
    ssRedirectToIndex,
 )
import Servant.Server (Application)
import Threads (AsyncComputation, isAsyncComputationRunning)
import WaiAppStatic.Types (unsafeToPiece)
import Msg (ExMsg (..))

type AddMsg = ExMsg -> IO ()

data Env = Env
    { _radioStreamActive :: MVar Bool
    , _tcpServer :: Connection
    }

$(makeLenses ''Env)

mkEnv :: IO Env
mkEnv = do
    _radioStreamActive <- newMVar False
    _tcpServer <- mkTCPServerConnection "3000" B.putStrLn
    pure $ Env {_radioStreamActive, _tcpServer}

handleRadioCommand :: AddMsg -> RadioCommand -> Handler Bool
handleRadioCommand addMsg Start = liftIO $ addMsg (ExMsg $ defMessage @Home.StartRadio) >> pure True
handleRadioCommand addMsg Stop = liftIO $ addMsg (ExMsg $ defMessage @Home.StopRadio) >> pure True

handleGetRadioStatus :: Env -> Handler Bool
handleGetRadioStatus env = do
    liftIO $ withMVar (env ^. radioStreamActive) pure

handleConnectionCommand :: AddMsg -> Handler Bool
handleConnectionCommand addMsg = liftIO $ addMsg (ExMsg $ defMessage @Home.ConnectTCP) >> pure True

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
        run port $ app env addMsg

    port = 8080

    handleAsyncException (_ :: SomeAsyncException) = do
        putStrLn "Async exception caught. Killing HTTP server"
