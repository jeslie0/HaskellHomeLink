{-# LANGUAGE OverloadedStrings #-}

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
  proxyPort,
  proxyURL,
  tlsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
 )
import Islands (Island (..))
import Lens.Micro
import Network.Socket (HostName, ServiceName)
import Options (runCommand)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import System.Directory (doesFileExist)

startCheckMemoryPoll ::
  EventLoopT Env (Island, ExHomeHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

connectToProxyTLS ::
  HostName
  -> ServiceName
  -> FilePath
  -> FilePath
  -> FilePath
  -> EventLoopT Env (Island, ExHomeHandler) IO ()
connectToProxyTLS host port certPath keyPath caCertPath = do
  addMsg
    ( Home
    , ExHomeHandler
        ( defMessage @Proto.ConnectTLS
            & Proto.host
            .~ T.pack host
            & Proto.port
            .~ T.pack port
            & Proto.certPath
            .~ T.pack certPath
            & Proto.keyPath
            .~ T.pack keyPath
            & Proto.caCertPath
            .~ T.pack caCertPath
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
    connectToProxyTLS
      (config ^. proxyURL)
      (show $ config ^. proxyPort)
      (config ^. tlsCertificatePath)
      (config ^. tlsKeyPath)
      (config ^. tlsCACertificatePath)
    startCheckMemoryPoll
    start $ uncurry homeHandler
