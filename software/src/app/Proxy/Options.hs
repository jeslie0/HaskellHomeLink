{-# LANGUAGE TemplateHaskell #-}

module Proxy.Options (
  ProxyOptions,
  httpsCertificatePath,
  httpsKeyPath,
  tlsCertificatePath,
  tlsKeyPath,
) where

import Lens.Micro.TH (makeLenses)
import Options (Options (..), simpleOption)

data ProxyOptions = ProxyOptions
  { _httpsCertificatePath :: String
  , _httpsKeyPath :: String
  , _tlsCertificatePath :: String
  , _tlsKeyPath :: String
  }

$(makeLenses ''ProxyOptions)

instance Options ProxyOptions where
  defineOptions =
    ProxyOptions
      <$> simpleOption
        "https-cert-path"
        ""
        "Path to the certificate for the HTTPS server."
      <*> simpleOption "https-key-path" "" "Path to the key for the HTTPS server."
      <*> simpleOption "tls-cert-path" "" "Path to the certificate for TLS connections."
      <*> simpleOption "tls-key-path" "" "Path to the key for TLS connections."
