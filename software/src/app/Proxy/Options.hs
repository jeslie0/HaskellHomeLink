{-# LANGUAGE TemplateHaskell #-}

module Proxy.Options (
  ProxyOptions,
  httpsCertificatePath,
  httpsKeyPath,
  httpsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
  tlsCACertificatePath,
  httpPort,
  tlsPort,
  hostname,
) where

import Lens.Micro.TH (makeLenses)
import Network.Socket (PortNumber)
import Network.TLS (HostName)
import Options (Options (..), simpleOption)

data ProxyOptions = ProxyOptions
  { _httpsCertificatePath :: String
  , _httpsKeyPath :: String
  , _httpsCACertificatePath :: String
  , _tlsCertificatePath :: String
  , _tlsKeyPath :: String
  , _tlsCACertificatePath :: String
  , _httpPort :: PortNumber
  , _tlsPort :: PortNumber
  , _hostname :: HostName
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
      <*> simpleOption "https-ca-cert-path" "" "Path to the CA file for TLS connections."
      <*> simpleOption "tls-cert-path" "" "Path to the certificate for TLS connections."
      <*> simpleOption "tls-key-path" "" "Path to the key for TLS connections."
      <*> simpleOption "tls-ca-cert-path" "" "Path to the CA file for TLS connections."
      <*> (toEnum <$> simpleOption "http-port" 3000 "Port to host http server on")
      <*> (toEnum <$> simpleOption "tls-port" 8080 "Port to host tls server on")
      <*> simpleOption "hostname" "" "Hostname used by clients to connect to the servers"
