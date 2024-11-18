{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module REST.HomeServer (runApp, mkEnv) where

import Control.Exception (SomeAsyncException, bracket, catch)
import Control.Monad.IO.Class (liftIO)
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

import Control.Concurrent (MVar, tryPutMVar, tryTakeMVar, withMVar)
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

type AddMsg = ExHomeHandler -> IO ()

data Env = Env {_asyncRadioStream :: MVar (Maybe AsyncComputation)}

$(makeLenses ''Env)

mkEnv :: Home.Env -> Env
mkEnv homeEnv = Env {_asyncRadioStream = homeEnv ^. Home.audioStreamMVar}

handleRadioCommand :: AddMsg -> RadioCommand -> Handler Bool
handleRadioCommand addMsg Start = liftIO $ addMsg (ExHomeHandler $ defMessage @Home.StartRadio) >> pure True
handleRadioCommand addMsg Stop = liftIO $ addMsg (ExHomeHandler $ defMessage @Home.StopRadio) >> pure True

handleGetRadioStatus :: Env -> Handler Bool
handleGetRadioStatus env = do
    liftIO $ withMVar (env ^. asyncRadioStream) $ \case
      Nothing -> pure False
      Just stream -> isAsyncComputationRunning stream

handleConnectionCommand :: AddMsg -> Handler Bool
handleConnectionCommand addMsg = liftIO $ addMsg (ExHomeHandler $ defMessage @Home.ConnectTCP) >> pure True

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
