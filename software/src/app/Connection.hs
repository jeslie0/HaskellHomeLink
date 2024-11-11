{-# LANGUAGE MonoLocalBinds #-}

module Connection (Connection (..), mkConnection) where

import Control.Concurrent (
    Chan,
    MVar,
    ThreadId,
    forkIO,
    killThread,
    newChan,
    readChan,
    readMVar,
    threadDelay,
 )
import Control.Exception (
    Exception (..),
    SomeException (SomeException),
    bracket,
    catch,
    throwIO,
 )
import Control.Monad (forever, void)
import Data.ByteString qualified as B
import EventLoop (EventLoop (..))
import Msg (Msg (..))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Socket (readHeader, recvNBytes)

data Connection = Connection
    { trySendMsg :: B.ByteString -> IO Bool
    , initialise :: IO ()
    , isActive :: MVar Bool
    }

-- | Create a (client) socket to connect to the given host and port.
mkAddrInfo :: HostName -> ServiceName -> IO AddrInfo
mkAddrInfo host port = do
    let hints = defaultHints {addrSocketType = Stream}
    results <- getAddrInfo (Just hints) (Just host) (Just port)
    case results of
        (addr : _) -> return addr
        [] -> ioError $ userError "No address information found"

{- | Safely build a socket from the given addr info, and run the
handler. The socket will close in the presence of an exception.
-}
runSocket :: AddrInfo -> (Socket -> IO ()) -> IO ()
runSocket addrInfo socketHandler = do
    bracket
        (openSocket addrInfo)
        (\s -> close s >> putStrLn "Closing socket down...")
        socketHandler

{- | Create a sender thread and a recv thread for a given socket. This
function will throw an exception if no bytes are received from the
socket (ie, it has been closed by the server.) This is unsafe, in
that the threads need to be cleaned up by the caller in the
presence of exceptions.
-}
establishSocketComms ::
    Socket
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> IO (ThreadId, ThreadId)
establishSocketComms sock sendChan recvHandler = do
    sendThread <- forkIO . forever $ do
        bytes <- readChan sendChan
        sendAll sock bytes

    recvThread <- forkIO . forever $ do
        mMsg <- recvMsg sock
        case mMsg of
            Nothing ->
                -- Socket dead
                throwIO SocketClosedException
            Just msg -> recvHandler msg

    return (sendThread, recvThread)

-- | Establish the required threads and connect to the server.
withSocket ::
    MVar ()
    -> SockAddr
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> Socket
    -> IO ()
withSocket blockMVar sockAddr sendChan recvHandler sock =
    handleSocket `catch` retryConnect
  where
    handleSocket = do
        putStrLn "Connecting to Socket"
        connect sock sockAddr `catch` failedToConnectHandle
        putStrLn "Connected to Socket"
        bracket
            (establishSocketComms sock sendChan recvHandler)
            ( \(th1, th2) -> killThread th1 >> killThread th2
            )
            $ \_ -> void $ readMVar blockMVar

    retryConnect = handleErr (withSocket blockMVar sockAddr sendChan recvHandler sock)

    failedToConnectHandle (SomeException err) = do
      putStrLn "Failed to connect to server. Retrying in 2s..."
      threadDelay 2000000
      handleSocket

data SocketClosedException = SocketClosedException deriving (Show)

instance Exception SocketClosedException

data SocketFailedToConnect

handleErr :: IO () -> SomeException -> IO ()
handleErr retry (SomeException ex) = do
    putStrLn $ "Exception occured: " <> displayException ex
    putStrLn "Connection to server failed. Trying again in 2s..."
    threadDelay 2000000
    retry

mkConnection ::
    (Msg msg) => EventLoop m msg -> MVar () -> HostName -> ServiceName -> IO ()
mkConnection loop blockMVar host port = do
    addrInfo <- mkAddrInfo host port
    sendChan <- newChan @B.ByteString
    runSocket addrInfo $
        withSocket
            blockMVar
            (addrAddress addrInfo)
            sendChan
            ( \bytes ->
                case fromBytes bytes of
                    Left err -> putStrLn err
                    Right msg -> _addMsg loop msg
            )

-- | Receive a message from the socket.
recvMsg :: Socket -> IO (Maybe B.ByteString)
recvMsg sock = do
    mLen <- readHeader sock
    case mLen of
        Nothing -> pure Nothing
        Just len -> do
            recvNBytes sock $ fromIntegral len
