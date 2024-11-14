{-# LANGUAGE MonoLocalBinds #-}

module Connection (Connection (..), mkConnection) where

import Control.Concurrent (
    Chan,
    MVar,
    isEmptyMVar,
    killThread,
    myThreadId,
    newChan,
    newEmptyMVar,
    putMVar,
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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as B
import EventLoop (EventLoop, addMsg)
import Msg (Msg (..))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Socket (readHeader, recvNBytes)
import ThreadPool (ThreadPool, addTask)

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
presence of exceptions. If this throws, it is because the connection
has been severed. The socket needs to be remade from scratch.
-}
establishSocketComms ::
    ThreadPool
    -> Socket
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> IO ()
establishSocketComms threadPool sock sendChan recvHandler = do
    connectionIsAliveMVar <- newEmptyMVar @()
    sendThreadMVar <- newEmptyMVar
    addTask threadPool (sendFunc sendThreadMVar connectionIsAliveMVar) sendErrHandle
    recvFunc sendThreadMVar connectionIsAliveMVar
  where
    sendFunc sendThreadMVar connectionIsAliveMVar = do
        myThreadId >>= putMVar sendThreadMVar
        sendFuncImpl
      where
        sendFuncImpl = do
            connectionIsAlive <- isEmptyMVar connectionIsAliveMVar
            when connectionIsAlive $ do
                bytes <- readChan sendChan
                sendAll sock bytes
                sendFuncImpl

    sendErrHandle ex@(SomeException err) = do
        putStrLn $ "AAA Thread Error!: " <> displayException err
        throwIO ex

    recvFunc sendThreadMVar connectionIsAliveMVar = do
        connectionIsAlive <- isEmptyMVar connectionIsAliveMVar
        when connectionIsAlive $ do
            mMsg <- recvMsg sock
            case mMsg of
                Nothing -> do
                    -- Socket dead
                    putMVar connectionIsAliveMVar ()
                    readMVar sendThreadMVar >>= killThread
                    throwIO $ userError "BBB Connection is dead"
                Just msg -> do
                  recvHandler msg
                  recvFunc sendThreadMVar connectionIsAliveMVar

-- | Establish the required threads and connect to the server.
withSocket ::
    ThreadPool
    -> SockAddr
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> Socket
    -> IO ()
withSocket threadPool sockAddr sendChan recvHandler sock = do
    tryConnect `catch` retryConnect
    establishSocketComms threadPool sock sendChan recvHandler
  where
    -- Attempt to connect to the socket. Return if the
    -- connection is successful, otherwise throw.
    tryConnect = do
        connect sock sockAddr
        putStrLn $ "Connected to server at: " <> show sockAddr

    -- If connection fails, try to connect again on the existing socket
    retryConnect =
        handleErr tryConnect

data SocketClosedException = SocketClosedException deriving (Show)

instance Exception SocketClosedException

handleErr :: IO a -> SomeException -> IO a
handleErr retry (SomeException _) = do
    putStrLn "Connection to server failed. Trying again in 2s..."
    threadDelay 2000000
    retry

mkConnection ::
    forall msg m.
    (Msg msg, MonadIO m) =>
    EventLoop m msg
    -> ThreadPool
    -> HostName
    -> ServiceName
    -> IO ()
mkConnection loop threadPool host port = do
    addrInfo <- mkAddrInfo host port
    sendChan <- newChan @B.ByteString
    executeSock addrInfo sendChan `catch` handleConnectionBreak
  where
    executeSock addrInfo sendChan =
        runSocket addrInfo $
            withSocket
                threadPool
                (addrAddress addrInfo)
                sendChan
                ( \bytes ->
                    case fromBytes @msg bytes of
                        Left err -> putStrLn err
                        Right msg -> addMsg loop msg
                )

    handleConnectionBreak (SomeException _) = do
        putStrLn "Failed to connect to server. Retrying in 2s..."
        threadDelay 2000000
        mkConnection loop threadPool host port

-- | Receive a message from the socket.
recvMsg :: Socket -> IO (Maybe B.ByteString)
recvMsg sock = do
    mLen <- readHeader sock
    case mLen of
        Nothing -> pure Nothing
        Just len -> do
            recvNBytes sock $ fromIntegral len
