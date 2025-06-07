{-# LANGUAGE OverloadedStrings #-}

module TLSHelper where

import Data.Default.Class (def)
import Data.X509 as X509
import Data.X509.CertificateStore (
  CertificateStore,
  listCertificates,
  makeCertificateStore,
 )
import Data.X509.File (readKeyFile, readSignedObject)
import Data.X509.Validation (
  validateDefault,
 )
import Network.TLS as TLS
import Network.TLS.Extra (ciphersuite_strong)

loadCredentials :: FilePath -> FilePath -> IO (Maybe Credential)
loadCredentials certPath keyPath = do
  certs :: [SignedExact Certificate] <- readSignedObject certPath
  keys <- readKeyFile keyPath
  case keys of
    [key] -> pure . Just $ (CertificateChain certs, key)
    _ -> pure Nothing

loadCAStore :: FilePath -> IO CertificateStore
loadCAStore caCertPath = do
  certs <- readSignedObject caCertPath
  pure $ makeCertificateStore certs

setupTLSServerParams ::
  HostName ->
  FilePath
  -- ^ Certificate path
  -> FilePath
  -- ^ Key path
  -> FilePath
  -- ^ CA Certificate path
  -> IO (Maybe ServerParams)
setupTLSServerParams hostname certPath keyPath caCrtPath = do
  mCreds <- loadCredentials certPath keyPath
  caStore <- makeCertificateStore <$> readSignedObject caCrtPath

  case mCreds of
    Nothing -> pure Nothing
    Just creds ->
      pure . Just $
        def
          { TLS.serverWantClientCert = True
          , TLS.serverCACertificates = listCertificates caStore
          , TLS.serverShared = def {TLS.sharedCredentials = Credentials [creds]}
          , TLS.serverSupported =
              def
                { supportedCiphers = ciphersuite_strong
                , supportedVersions = [TLS13]
                }
          , TLS.serverHooks = mTLSHooks hostname caStore
          }

mTLSHooks :: HostName -> CertificateStore -> ServerHooks
mTLSHooks hostname caStore =
  def
    { TLS.onClientCertificate = validateClientCert
    }
 where
  validateClientCert (CertificateChain []) = do
    pure . CertificateUsageReject . CertificateRejectOther $
      "No client certificates provided."
  validateClientCert chain = do
    validationResult <-
      validateDefault
        caStore
        def
        (hostname, "")
        chain
    case validationResult of
      [] -> pure CertificateUsageAccept
      errors -> do
        print errors
        pure . CertificateUsageReject . CertificateRejectOther $
          "No valid client certificates provided."

setupTLSClientParams ::
  FilePath
  -- ^ Certificate path
  -> FilePath
  -- ^ Key path
  -> FilePath
  -- ^ CA Certificate path
  -> HostName
  -> IO (Maybe ClientParams)
setupTLSClientParams certPath keyPath caCertPath hostname = do
  mCredentials <- loadCredentials certPath keyPath
  case mCredentials of
    Nothing -> pure Nothing
    Just creds -> do
      caStore <- loadCAStore caCertPath
      let defaultParams = defaultParamsClient hostname ""
      pure . Just $
        defaultParams
          { clientSupported =
              def
                { supportedCiphers = ciphersuite_strong
                , supportedVersions = [TLS13]
                }
          , clientShared =
              def
                { sharedCredentials = Credentials [creds]
                , sharedCAStore = caStore
                }
          , clientUseServerNameIndication = False
          , clientHooks =
              def
                { onServerCertificate = \_ ->
                    validateDefault caStore
                , onCertificateRequest = \_ -> pure $ Just creds
                }
          }
