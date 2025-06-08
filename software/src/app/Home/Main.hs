{-# LANGUAGE OverloadedStrings #-}

module Home.Main (main) where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecodeFileStrict)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Devices (Device (..))
import EventLoop (
  EventLoopT,
  MonadEventLoop (..),
  addMsg,
  runEventLoopT,
  setInterval,
 )
import Home.Env (Env, cleanupEnv, mkEnv)
import Home.Handler (
  EstablishTLSConnection (..),
  ExHomeHandler (..),
  homeHandler,
 )
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
import Lens.Micro
import Network.Socket (HostName, ServiceName)
import Options (runCommand)
import Proto.DeviceData qualified as Proto
import TLSHelper (setupTLSClientParams)

startCheckMemoryPoll ::
  EventLoopT Env (Device, ExHomeHandler) IO ()
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
  -> EventLoopT Env (Device, ExHomeHandler) IO Bool
connectToProxyTLS host port certPath keyPath caCertPath = do
  mParams <-
    liftIO $
      setupTLSClientParams
        certPath
        keyPath
        caCertPath
        "raspberrypi"
  case mParams of
    Nothing -> do
      liftIO $ putStrLn "Failed to unpack TLS server params"
      pure False
    Just params -> do
      addMsg
        ( Home
        , ExHomeHandler $ EstablishTLSConnection params (T.pack host) (T.pack port)
        )
      pure True

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
    HomeConfiguration -> EventLoopT Env (Device, ExHomeHandler) IO ()
  action config = do
    success <- connectToProxyTLS
      "127.0.0.1"
      "3000"
      -- (config ^. proxyURL)
      -- (show $ config ^. proxyPort)
      (config ^. tlsCertificatePath)
      (config ^. tlsKeyPath)
      (config ^. tlsCACertificatePath)
    when success $ do
        startCheckMemoryPoll
        start $ uncurry homeHandler
