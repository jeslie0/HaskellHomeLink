{-# LANGUAGE OverloadedStrings #-}

module Connection.TLS where

import Connection.TCP as TCP
import Control.Exception (bracket)
import Data.ByteString qualified as B
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
import Network.Socket (ServiceName, accept, close)
import Network.TLS as TLS
import Network.TLS.Extra (ciphersuite_all)

aquireActiveServerSocketTLS ::
  TLS.TLSParams params =>
  params
  -> ServiceName
  -> (B.ByteString -> IO ())
  -- ^ Function to run when receiving
  -- messages on the socket.
  -> (TLS.Context -> IO ())
  -- ^ Function to run when socket has been acquired.
  -> IO ()
  -- ^ Cleanup action to run when socket is closed by server.
  -> IO ()
aquireActiveServerSocketTLS params port withBytes withConn cleanupCtx = do
  bracket
    (TCP.aquireBoundListeningServerSocket port)
    close
    withServerSock
 where
  withServerSock serverSock = do
    bracket (open serverSock) close $ \conn ->
      bracket (TLS.contextNew conn params) TLS.bye $ \ctx -> do
        handshake ctx
        withConn ctx
        recvFunc serverSock ctx

  open sock = do
    (conn, peer) <- accept sock
    putStrLn $ "Accepted connection from " <> show peer
    pure conn

  recvFunc serverSock ctx = do
    msg <- TLS.recvData ctx
    if B.null msg
      then do
        cleanupCtx
        withServerSock serverSock
      -- Socket dead
      else do
        withBytes msg
        recvFunc serverSock ctx

aquireActiveClientSocketTLS ::
  TLS.TLSParams params =>
  params
  -> HostName
  -> ServiceName
  -> (B.ByteString -> IO ())
  -- ^ Function to run when receiving
  -- messages on the socket.
  -> (TLS.Context -> IO ())
  -- ^ Function to run when socket has been acquired.
  -> (TLS.Context -> IO ())
  -- ^ Cleanup function to run when socket is closed by server.
  -> IO ()
aquireActiveClientSocketTLS params host port withBytes withSock cleanupCtx = do
  bracket (aquireConnectedClientSocket host port (pure ()) (pure ())) close $ \sock ->
    bracket (TLS.contextNew sock params) TLS.bye $ \ctx -> do
      handshake ctx
      withSock ctx
      recvFunc ctx
 where
  recvFunc ctx = do
    msg <- TLS.recvData ctx
    if B.null msg
      then do
        -- Socket dead
        cleanupCtx ctx
      else do
        withBytes msg
        recvFunc ctx

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
                { supportedCiphers = ciphersuite_all
                , supportedVersions = [TLS13, TLS12]
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
  -> IO (Maybe ClientParams)
setupTLSClientParams certPath keyPath caCertPath = do
  mCredentials <- loadCredentials certPath keyPath
  case mCredentials of
    Nothing -> pure Nothing
    Just creds -> do
      caStore <- loadCAStore caCertPath
      let defaultParams = defaultParamsClient "localhost" ""
      pure . Just $
        defaultParams
          { clientSupported =
              def
                { supportedCiphers = ciphersuite_all
                , supportedVersions = [TLS13, TLS12]
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
