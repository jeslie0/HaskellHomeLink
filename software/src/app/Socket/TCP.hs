module Socket.TCP (ClientSocket(..), mkClientSocket, ServerSocket, mkServerSocket) where

import Control.Concurrent (isEmptyMVar, newEmptyMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.Socket

data ClientSocket = ClientSocket
    { _runClient :: IO Bool
    , _addClient :: (Socket -> IO ()) -> IO ()
    }

{- | Create a client socket. A user of this is not responsible for
closing the socket, but they can if they want to.
-}
mkClientSocket :: HostName -> ServiceName -> IO ClientSocket
mkClientSocket host port = do
    clientRef <- newIORef Nothing
    let _addClient' client = writeIORef clientRef $ Just client
    let _runClient' = do
            mClient <- readIORef clientRef
            case mClient of
                Nothing -> pure False
                Just client -> do
                    let hints = defaultHints {addrSocketType = Stream}
                    addrInfo <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
                    bracket (openSocket addrInfo) close $ \sock -> do
                        connect sock $ addrAddress addrInfo
                        client sock
                        pure True
    return $
        ClientSocket
            { _runClient = _runClient'
            , _addClient = _addClient'
            }

data ServerSocket = ServerSocket
    { _runServer :: IO Bool
    , _addServer :: (Socket -> IO ()) -> IO ()
    , _shutdownServer :: IO ()
    }

{- | Make a server socket. A user of this is not responsible for
closing the socket, but they can if they want to.
-}
mkServerSocket :: ServiceName -> IO ServerSocket
mkServerSocket port = do
    serverRef <- newIORef Nothing
    serverAliveMVar <- newEmptyMVar @()
    let _addServer' server = writeIORef serverRef $ Just server
    let _runServer' = do
            mServer <- readIORef serverRef
            case mServer of
                Nothing -> pure False
                Just server -> do
                    let hints =
                            defaultHints
                                { addrFlags = [AI_PASSIVE]
                                , addrSocketType = Stream
                                }
                    addrInfo <- head <$> getAddrInfo (Just hints) Nothing (Just port)
                    bracket (openSocket addrInfo) close $ \sock -> do
                        setSocketOption sock ReuseAddr 1
                        bind sock $ addrAddress addrInfo
                        listen sock 1
                        loop serverAliveMVar server sock
                        pure True
    return $
        ServerSocket
            { _runServer = _runServer'
            , _addServer = _addServer'
            , _shutdownServer = putMVar serverAliveMVar ()
            }
  where
    loop serverAliveMVar server sock = do
        keepLooping <- isEmptyMVar serverAliveMVar
        when keepLooping $ do
            (newSock, _) <- accept sock
            void $ server newSock
            loop serverAliveMVar server sock
