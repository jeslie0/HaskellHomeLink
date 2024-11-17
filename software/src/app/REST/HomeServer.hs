module REST.HomeServer (runApp) where

import Control.Exception (SomeAsyncException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.ProtoLens (defMessage)
import Home.Handler (ExHomeHandler (..))
import Network.Wai.Handler.Warp (run)
import Proto.Home qualified as Home
import REST.Api (Api, RadioCommand (..))
import Servant (
    Handler,
    Proxy (Proxy),
    Server,
    serve,
    (:<|>) (..),
 )

import Servant.Server (Application)

type AddMsg = ExHomeHandler -> IO ()

handleRadioCommand :: AddMsg -> RadioCommand -> Handler Bool
handleRadioCommand addMsg Start = liftIO $ addMsg (ExHomeHandler $ defMessage @Home.StartRadio) >> pure True
handleRadioCommand addMsg Stop = liftIO $ addMsg (ExHomeHandler $ defMessage @Home.StopRadio) >> pure True

handleConnectionCommand :: AddMsg -> Handler Bool
handleConnectionCommand addMsg = liftIO $ addMsg (ExHomeHandler $ defMessage @Home.ConnectTCP) >> pure True

server :: AddMsg -> Server Api
server addMsg =
    handleRadioCommand addMsg :<|> handleConnectionCommand addMsg

app :: AddMsg -> Application
app addMsg = serve (Proxy @Api) $ server addMsg

runApp :: AddMsg -> IO ()
runApp addMsg = do
    putStrLn $ "Starting HTTP server on port " <> show port
    runAppImpl `catch` handleAsyncException
  where
    runAppImpl =
      run port $ app addMsg

    port = 8080

    handleAsyncException (_ :: SomeAsyncException) = do
        putStrLn "Async exception caught. Killing HTTP server"
