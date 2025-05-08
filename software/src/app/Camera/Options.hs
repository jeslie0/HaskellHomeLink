{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Options (
  ProxyOptions,
  configPath,
  ProxyConfiguration,
  httpsCertificatePath,
  httpsKeyPath,
  httpsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
  tlsCACertificatePath,
  httpPort,
  tlsPort,
  httpHostname,
  tlsHostname,
) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Lens.Micro.TH (makeLenses)
import Network.Socket (PortNumber)
import Network.TLS (HostName)
import Options (Options (..), simpleOption)

data ProxyConfiguration = ProxyConfiguration
  { _httpsCertificatePath :: String
  , _httpsKeyPath :: String
  , _httpsCACertificatePath :: String
  , _httpHostname :: HostName
  , _httpPort :: PortNumber
  , _tlsCertificatePath :: String
  , _tlsKeyPath :: String
  , _tlsCACertificatePath :: String
  , _tlsHostname :: HostName
  , _tlsPort :: PortNumber
  }

$(makeLenses ''ProxyConfiguration)

instance FromJSON ProxyConfiguration where
  parseJSON = withObject "ProxyConfiguration" $ \v -> do
    http <- v .: "http"
    tls <- v .: "tls"
    ProxyConfiguration
      <$> http .: "cert"
      <*> http .: "key"
      <*> http .: "ca"
      <*> http .: "hostname"
      <*> (toEnum <$> http .: "port")
      <*> tls .: "cert"
      <*> tls .: "key"
      <*> tls .: "ca"
      <*> tls .: "hostname"
      <*> (toEnum <$> tls .: "port")

newtype ProxyOptions = ProxyOptions {_configPath :: FilePath}

$(makeLenses ''ProxyOptions)

instance Options ProxyOptions where
  defineOptions =
    ProxyOptions <$> simpleOption "config" "./config.json" "Path to the config file"
