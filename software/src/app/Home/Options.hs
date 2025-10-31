{-# LANGUAGE OverloadedStrings #-}

module Home.Options (
  HomeOptions (..),
  HomeConfiguration (..),
) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Network.Socket (HostName, PortNumber)
import Options (Options (..), simpleOption)

data HomeConfiguration = HomeConfiguration
  { httpsCertificatePath :: String
  , httpsKeyPath :: String
  , httpsCACertificatePath :: String
  , httpHostname :: HostName
  , httpPort :: PortNumber
  , tlsCertificatePath :: String
  , tlsKeyPath :: String
  , tlsCACertificatePath :: String
  , tlsHostname :: HostName
  , proxyURL :: String
  , proxyPort :: PortNumber
  }

instance FromJSON HomeConfiguration where
  parseJSON = withObject "HomeConfiguration" $ \v -> do
    http <- v .: "http"
    tls <- v .: "tls"
    proxy <- v .: "proxy"
    HomeConfiguration
      <$> http .: "cert"
      <*> http .: "key"
      <*> http .: "ca"
      <*> http .: "hostname"
      <*> (toEnum <$> http .: "port")
      <*> tls .: "cert"
      <*> tls .: "key"
      <*> tls .: "ca"
      <*> tls .: "hostname"
      <*> proxy .: "url"
      <*> (toEnum <$> proxy .: "port")

newtype HomeOptions = HomeOptions {configPath :: FilePath}

instance Options HomeOptions where
  defineOptions =
    HomeOptions <$> simpleOption "config" "./config.json" "Path to the config file"
