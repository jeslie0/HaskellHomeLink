{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Options where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Lens.Micro.TH (makeLenses)
-- import Network.Socket (PortNumber)
-- import Network.TLS (HostName)
import Options (Options (..), simpleOption)

data CameraConfiguration = CameraConfiguration
  { _httpsCertificatePath :: String
  , _httpsKeyPath :: String
  , _httpsCACertificatePath :: String
  , _tlsCertificatePath :: String
  , _tlsKeyPath :: String
  , _tlsCACertificatePath :: String
  }

$(makeLenses ''CameraConfiguration)

-- instance FromJSON ProxyConfiguration where
--   parseJSON = withObject "ProxyConfiguration" $ \v -> do
--     http <- v .: "http"
--     tls <- v .: "tls"
--     ProxyConfiguration
--       <$> http .: "cert"
--       <*> http .: "key"
--       <*> http .: "ca"
--       <*> http .: "hostname"
--       <*> (toEnum <$> http .: "port")
--       <*> tls .: "cert"
--       <*> tls .: "key"
--       <*> tls .: "ca"
--       <*> tls .: "hostname"
--       <*> (toEnum <$> tls .: "port")

data CameraOptions = CameraOptions

$(makeLenses ''CameraOptions)

-- instance Options ProxyOptions where
--   defineOptions =
--     ProxyOptions <$> simpleOption "config" "./config.json" "Path to the config file"
