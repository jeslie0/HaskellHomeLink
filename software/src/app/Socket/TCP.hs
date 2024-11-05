module Socket.TCP (runTCPClient, runTCPServer) where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Exception (SomeException, bracket, bracketOnError, try)
import Control.Monad (forever, void)
import Data.ByteString qualified as B
import Network.Socket
import Network.Socket.ByteString (recv)

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) (\s -> putStrLn "Closing socket" >> close s) client
  where
    resolve = do
        let hints = defaultHints {addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) close loop
  where
    resolve = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $
        bracketOnError (accept sock) (close . fst) $
            \(conn, _peer) ->
                void $
                    -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
                    -- but 'E.bracketOnError' above will be necessary if some
                    -- non-atomic setups (e.g. spawning a subprocess to handle
                    -- @conn@) before proper cleanup of @conn@ is your case
                    forkFinally (server conn) (const $ gracefulClose conn 5000)

tcpClientTest :: HostName -> ServiceName -> (IO () -> Socket -> IO ()) -> IO ()
tcpClientTest host port client = do
    let hints = defaultHints {addrSocketType = Stream}
    addrInfo <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
    makeSocket addrInfo
  where
    makeSocket addrInfo =
        bracket
            (openSocket addrInfo)
            close
            $ \sock -> tryConnect sock addrInfo

    withSocket sock addrInfo = do
      client (makeSocket addrInfo) sock

    tryConnect sock addrInfo = do
        eitherFailSucc <- try @SomeException (connect sock $ addrAddress addrInfo)
        case eitherFailSucc of
            Left err -> do
                putStrLn "Couldn't connect to server. Trying again in 2 seconds"
                threadDelay 2000000
                tryConnect sock addrInfo
            Right _ -> withSocket sock addrInfo

-- tcpServeTest
