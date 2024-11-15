{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Connection (Connection (..), mkConnection) where

import Control.Concurrent (
    Chan,
    MVar,
    isEmptyMVar,
    myThreadId,
    newChan,
    newEmptyMVar,
    newMVar,
    putMVar,
    readChan,
    readMVar,
    threadDelay,
    tryPutMVar,
    tryTakeMVar,
    writeChan,
 )
import Control.Exception (
    Exception (..),
    AsyncException(..),
    SomeException (SomeException),
    bracket,
    catch,
    throwIO, SomeAsyncException (..),
 )
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as B
import EventLoop (EventLoop, addMsg)
import Msg (Msg (..))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Socket (readHeader, recvNBytes)
import ThreadPool (
    ThreadPool,
    addTask,
    killAsyncComputation,
    spawnAsyncComputation,
 )

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
    Socket
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> IO ()
establishSocketComms sock sendChan recvHandler = do
    -- If recvFunc throws (like when a connection is terminated), kill
    -- the sending thread. We don't want to catch such an error here
    -- though - it should be handled higher up since we need to either
    -- kill the process or make a whole new socket.
    bracket (spawnAsyncComputation sendFunc) killAsyncComputation $
        const recvFunc
  where
    sendFunc = do
        bytes <- readChan sendChan
        sendAll sock bytes
        sendFunc

    recvFunc = do
        mMsg <- recvMsg sock
        case mMsg of
            Nothing -> do
                -- Socket dead
                throwIO SocketClosedException
            Just msg -> do
                recvHandler msg
                recvFunc

-- | Establish the required threads and connect to the server.
withSocket ::
    SockAddr
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> Socket
    -> IO ()
withSocket sockAddr sendChan recvHandler sock = do
    safeConnect
    establishSocketComms sock sendChan recvHandler
  where
    -- If connection fails, try to connect again on the existing socket
    safeConnect = do
        tryConnect `catch` handleErr safeConnect

    -- Attempt to connect to the socket. Return if the
    -- connection is successful, otherwise throw.
    tryConnect = do
        connect sock sockAddr
        putStrLn $ "Connected to server at: " <> show sockAddr

data SocketClosedException = SocketClosedException deriving (Show)

instance Exception SocketClosedException

handleErr :: IO a -> SomeException -> IO a
handleErr retry (SomeException err) = do
    putStrLn "Could not connect to server. Trying again in 2s..."
    threadDelay 2000000
    retry

mkConnection ::
    forall msg m.
    (Msg msg, MonadIO m) =>
    EventLoop m msg
    -> HostName
    -> ServiceName
    -> IO ()
mkConnection loop host port =
    mkConnectionImpl `catch` asyncExceptionHandler
  where
    mkConnectionImpl = do
        addrInfo <- mkAddrInfo host port
        sendChan <- newChan @B.ByteString

        executeSock addrInfo sendChan
            `catch` handleConnectionBreak

    executeSock addrInfo sendChan = do
        runSocket addrInfo $
            withSocket
                (addrAddress addrInfo)
                sendChan
                ( \bytes ->
                    case fromBytes @msg bytes of
                        Left err -> putStrLn err
                        Right msg -> addMsg loop msg
                )

    handleConnectionBreak SocketClosedException = do
        putStrLn "Connection to server terminated. Retrying in 2s..."
        threadDelay 2000000
        mkConnection loop host port

    asyncExceptionHandler (SomeAsyncException err)= do
      putStrLn "Async exception caught. Thread dying.."

-- | Receive a message from the socket.
recvMsg :: Socket -> IO (Maybe B.ByteString)
recvMsg sock = do
    mLen <- readHeader sock
    case mLen of
        Nothing -> pure Nothing
        Just len -> do
            recvNBytes sock $ fromIntegral len
