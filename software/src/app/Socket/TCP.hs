module Socket.TCP (runTCPClient, runTCPServer) where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)
import Network.Socket

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) close client
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
