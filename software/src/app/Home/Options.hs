{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Home.Options (
  HomeOptions,
  configPath,
  HomeConfiguration,
  httpsCertificatePath,
  httpsKeyPath,
  httpsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
  tlsCACertificatePath,
  tlsHostname,
  proxyURL,
  proxyPort,
  httpPort,
  httpHostname,
) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Lens.Micro.TH (makeLenses)
import Network.Socket (HostName, PortNumber)
import Options (Options (..), simpleOption)

data HomeConfiguration = HomeConfiguration
  { _httpsCertificatePath :: String
  , _httpsKeyPath :: String
  , _httpsCACertificatePath :: String
  , _httpHostname :: HostName
  , _httpPort :: PortNumber
  , _tlsCertificatePath :: String
  , _tlsKeyPath :: String
  , _tlsCACertificatePath :: String
  , _tlsHostname :: HostName
  , _proxyURL :: String
  , _proxyPort :: PortNumber
  }

$(makeLenses ''HomeConfiguration)

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

newtype HomeOptions = HomeOptions {_configPath :: FilePath}

$(makeLenses ''HomeOptions)

instance Options HomeOptions where
  defineOptions =
    HomeOptions <$> simpleOption "config" "./config.json" "Path to the config file"
