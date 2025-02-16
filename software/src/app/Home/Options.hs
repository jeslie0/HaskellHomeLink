{-# LANGUAGE TemplateHaskell #-}

module Home.Options (
  HomeOptions,
  httpsCertificatePath,
  httpsKeyPath,
  caCertificatePath,
  proxyURL,
  proxyPort,
) where

import Lens.Micro.TH (makeLenses)
import Options (Options (..), simpleOption)

data HomeOptions = HomeOptions
  { _httpsCertificatePath :: String
  , _httpsKeyPath :: String
  , _caCertificatePath :: String
  , _proxyURL :: String
  , _proxyPort :: String
  }

$(makeLenses ''HomeOptions)

instance Options HomeOptions where
  defineOptions =
    HomeOptions
      <$> simpleOption
        "https-cert-path"
        ""
        "Path to the certificate for the HTTPS server."
      <*> simpleOption "https-key-path" "" "Path to the key for the HTTPS server."
      <*> simpleOption "ca-cert-path" "" "Path to the CA file for TLS connections."
      <*> simpleOption "proxy-url" "" "URL of the proxy to try and connect to."
      <*> simpleOption "proxy-url" "" "URL of the proxy to try and connect to."
