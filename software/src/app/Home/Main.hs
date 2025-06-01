module Home.Main (main) where

import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import EventLoop (
  EventLoopT,
  MonadEventLoop (..),
  addMsg,
  runEventLoopT,
  setInterval,
 )
import Home.Env (Env, cleanupEnv, mkEnv)
import Home.Handler (ExHomeHandler (..), homeHandler)
import Home.Options (
  HomeConfiguration,
  HomeOptions,
  configPath,
 )
import Islands (Island (..))
import Lens.Micro
import Network.Socket (HostName, ServiceName)
import Options (runCommand)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto

startCheckMemoryPoll ::
  EventLoopT Env (Island, ExHomeHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

connectToProxyTLS ::
  HostName -> ServiceName -> EventLoopT Env (Island, ExHomeHandler) IO ()
connectToProxyTLS host port = do
  addMsg
    ( Home
    , ExHomeHandler
        ( defMessage @Proto.ConnectTCP
            & Proto.host
            .~ T.pack host
            & Proto.port
            .~ T.pack port
        )
    )

main :: IO ()
main = runCommand $ \(opts :: HomeOptions) _args -> do
  mConfig <- eitherDecodeFileStrict (opts ^. configPath)
  case mConfig of
    Left errs -> putStrLn $ "Could not parse configuration file: " <> errs
    Right config -> do
      bracket mkEnv cleanupEnv $ \env -> do
        runEventLoopT (action config) env
        writeFile "/mnt/normalexit" ""
 where
  action ::
    HomeConfiguration -> EventLoopT Env (Island, ExHomeHandler) IO ()
  action config = do
    env <- getEnv
    connectToProxyTLS "127.0.0.1" "8000"
    startCheckMemoryPoll
    start $ uncurry homeHandler
