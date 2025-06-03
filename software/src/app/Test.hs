module Test where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import Network.TLS
import Network.TLS.Extra.Cipher
import Data.Default.Class
import qualified Data.X509 as X509
import qualified Data.X509.CertificateStore as X509
import qualified Data.X509.File as X509

-- Step 1: Load server credentials (used here by the client socket!)
loadCredentials :: IO Credential
loadCredentials = do
    certs <- X509.readSignedObject "./configs/home/crypto/tls_server/Raspberry_Pi.crt"
    key   <- X509.readKeyFile "./configs/home/crypto/tls_server/TLS_Server_Raspberry_Pi_.pem"
    case key of
        [priv] -> return $ Credentials certs priv
        _      -> error "Could not read private key"

-- Step 2: Load CA store to verify client cert (for mTLS)
loadCAStore :: IO X509.CertificateStore
loadCAStore = X509.makeCertificateStore <$> X509.readCertificates "./configs/home/crypto/tls_server/TLS_Client_CA.crt"

-- Step 3: Connect as TCP client
connectTCP :: String -> String -> IO NS.Socket
connectTCP host port = do
    addrInfos <- NS.getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfos
    sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
    NS.connect sock (NS.addrAddress serverAddr)
    return sock

-- Step 4: Create Backend from client socket
socketBackend :: NS.Socket -> Backend
socketBackend sock = Backend
  { backendSend = NSB.sendAll sock
  , backendRecv = \n -> NSB.recv sock n
  , backendClose = NS.close sock
  }
 
mkServerParams :: Credentials -> X509.CertificateStore -> ServerParams
mkServerParams creds caStore =
  def
    { serverShared = def
        { sharedCredentials = creds
        , sharedCAStore = caStore
        }
    , serverSupported = def
        { supportedCiphers = ciphersuite_default
        }
    , serverWantClientCert = True
    }

-- Step 5: TLS ServerParams

-- Step 6: TLS context using ServerParams on a client socket
main :: IO ()
main = do
    sock <- connectTCP "peer-host" "4433"  -- or your custom port
    creds <- loadCredentials
    caStore <- loadCAStore
    ctx <- contextNew (socketBackend sock) (mkServerParams creds caStore)
    handshake ctx
    putStrLn "TLS handshake complete (server context on client socket)"
    -- Now you can recvData/sendData as usual
    msg <- recvData ctx
    BS.putStr msg
    bye ctx
