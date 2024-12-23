module Connection.TCP (
    mkClientConnection,
    mkServerConnection,
    aquireActiveClientSocket,
    aquireActiveServerSocket,
) where

import Control.Concurrent (
    Chan,
    readChan,
    threadDelay,
 )
import Control.Exception (
    Exception (..),
    IOException,
    SomeAsyncException (..),
    bracket,
    bracketOnError,
    catch,
    throwIO,
 )
import Data.ByteString qualified as B
import Data.Serialize (putWord32le, runPut)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Socket (readHeader, recvNBytes)
import Threads (
    killAsyncComputation,
    spawnAsyncComputation,
 )

-- | Create address info for to be used to create a client socket.
mkAddrInfo :: HostName -> ServiceName -> IO AddrInfo
mkAddrInfo host port = do
    let hints = defaultHints {addrSocketType = Stream}
    results <- getAddrInfo (Just hints) (Just host) (Just port)
    case results of
        (addr : _) -> return addr
        [] -> ioError $ userError "No address information found"

mkServerAddrInfo :: ServiceName -> IO AddrInfo
mkServerAddrInfo port = do
    let hints = defaultHints {addrSocketType = Stream, addrFlags = [AI_PASSIVE]}
    results <- getAddrInfo (Just hints) Nothing (Just port)
    case results of
        (addr : _) -> return addr
        [] -> ioError $ userError "No address information found"

{- | Safely build a socket from the given addr info, and run the
handler. The socket will close in the presence of an exception.
-}
runClientSocket :: AddrInfo -> (Socket -> IO ()) -> IO ()
runClientSocket addrInfo socketHandler = do
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
        sendAll sock $ runPut $ putWord32le $ fromIntegral (B.length bytes)
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
        tryConnect `catch` handleConnectionFail

    handleConnectionFail (_ :: IOException) = do
        putStrLn "Could not connect to server. Trying again in 2s..."
        threadDelay 2000000
        safeConnect

    -- Attempt to connect to the socket. Return if the
    -- connection is successful, otherwise throw.
    tryConnect = do
        connect sock sockAddr
        putStrLn $ "Connected to server at: " <> show sockAddr

data SocketClosedException = SocketClosedException deriving (Show)

instance Exception SocketClosedException

{- | Create a connection to the given host and port, using the passed
in function to handle received bytes. The bytes received must
follow the connection protocol - a 4byte Word32 (le) of a message
size, followed by the message.
-}
mkClientConnection ::
    HostName
    -> ServiceName
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> IO ()
mkClientConnection host port sendChan withBytes =
    mkConnectionImpl `catch` asyncExceptionHandler
  where
    mkConnectionImpl = do
        addrInfo <- mkAddrInfo host port

        executeSock addrInfo
            `catch` handleConnectionBreak

    executeSock addrInfo = do
        runClientSocket addrInfo $
            withSocket
                (addrAddress addrInfo)
                sendChan
                withBytes

    handleConnectionBreak SocketClosedException = do
        putStrLn "Connection to server terminated. Retrying in 2s..."
        threadDelay 2000000
        mkClientConnection host port sendChan withBytes

    asyncExceptionHandler (SomeAsyncException _) = do
        putStrLn "Async exception caught. Thread dying.."

mkServerConnection ::
    ServiceName
    -> Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> IO ()
mkServerConnection port sendChan withBytes =
    mkConnectionImpl `catch` asyncExceptionHandler
  where
    mkConnectionImpl = do
        addrInfo <- mkServerAddrInfo port
        runServerSocket addrInfo $
            withServerSocket
                sendChan
                withBytes

    asyncExceptionHandler (SomeAsyncException _) = do
        putStrLn "Async exception caught. Thread dying.."

withServerSocket ::
    Chan B.ByteString
    -> (B.ByteString -> IO ())
    -> Socket
    -> IO ()
withServerSocket sendChan recvHandler sock = do
    withServerSocketImpl `catch` handleConnectionBreak
  where
    withServerSocketImpl = do
        connectedSock <- open
        establishSocketComms connectedSock sendChan recvHandler

    handleConnectionBreak SocketClosedException = do
        putStrLn "Connection to client terminated"
        withServerSocket sendChan recvHandler sock

    -- Attempt to connect to the socket. Return if the
    -- connection is successful, otherwise throw.
    open = do
        (conn, peer) <- accept sock
        putStrLn $ "Accepted connection from " <> show peer
        pure conn

runServerSocket :: AddrInfo -> (Socket -> IO ()) -> IO ()
runServerSocket addrInfo socketHandler = do
    bracket
        openSock
        (\s -> close s >> putStrLn "Closing socket down...")
        socketHandler
  where
    openSock = do
        sock <-
            socket (addrFamily addrInfo) (addrSocketType addrInfo) (addrProtocol addrInfo)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addrInfo)
        listen sock 1
        putStrLn "Server listening..."
        pure sock

-- | Receive a message from the socket.
recvMsg :: Socket -> IO (Maybe B.ByteString)
recvMsg sock = do
    mLen <- readHeader sock
    case mLen of
        Nothing -> pure Nothing
        Just len -> do
            recvNBytes sock $ fromIntegral len

-- * Next

aquireBoundListeningServerSocket ::
    ServiceName
    -> IO Socket
aquireBoundListeningServerSocket port = do
    addrInfo <- mkServerAddrInfo port
    sock <-
        socket (addrFamily addrInfo) (addrSocketType addrInfo) (addrProtocol addrInfo)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addrInfo)
    listen sock 1
    putStrLn "Server listening..."
    pure sock

aquireActiveServerSocket ::
    ServiceName
    -> (B.ByteString -> IO ())
    -- ^ Function to run when receiving
    -- messages on the socket.
    -> (Socket -> IO ())
    -- ^ Function to run when socket has been acquired.
    -> IO ()
    -- ^ Cleanup action to run when socket is closed by server.
    -> IO ()
aquireActiveServerSocket port withBytes withSock cleanupSocket = do
    bracket
        (aquireBoundListeningServerSocket port)
        close
        withServerSock
  where
    withServerSock serverSock = do
        bracket (open serverSock) close $ \conn -> do
            withSock conn
            recvFunc serverSock conn

    open sock = do
        (conn, peer) <- accept sock
        putStrLn $ "Accepted connection from " <> show peer
        pure conn

    recvFunc serverSock sock = do
        mMsg <- recvMsg sock
        case mMsg of
            Nothing -> do
                cleanupSocket
                withServerSock serverSock
            -- Socket dead
            Just msg -> do
                withBytes msg
                recvFunc serverSock sock

aquireConnectedClientSocket ::
    HostName
    -> ServiceName
    -> IO Socket
aquireConnectedClientSocket host port = do
    addrInfo <- mkAddrInfo host port
    safeNewSocket addrInfo
  where
    newSocket addrInfo =
        bracketOnError
            (openSocket addrInfo)
            close
            $ \sock -> do
                safeConnect sock (addrAddress addrInfo)
                pure sock

    safeNewSocket addrInfo =
        newSocket addrInfo `catch` \(_ :: SocketClosedException) -> safeNewSocket addrInfo

    safeConnect sock sockAddr = do
        tryConnect sock sockAddr `catch` handleConnectionFail sock sockAddr

    -- If unable to connect to server, try again in 2 seconds.
    handleConnectionFail sock sockAddr (_ :: IOException) = do
        putStrLn "Could not connect to server. Trying again in 2s..."
        threadDelay 2000000
        safeConnect sock sockAddr

    -- Attempt to connect to the socket. Return if the
    -- connection is successful, otherwise throw.
    tryConnect sock sockAddr = do
        connect sock sockAddr
        putStrLn $ "Connected to server at: " <> show sockAddr

aquireActiveClientSocket ::
    HostName
    -> ServiceName
    -> (B.ByteString -> IO ())
    -- ^ Function to run when receiving
    -- messages on the socket.
    -> (Socket -> IO ())
    -- ^ Function to run when socket has been acquired.
    -> (Socket -> IO ())
    -- ^ Cleanup function to run when socket is closed by server.
    -> IO ()
aquireActiveClientSocket host port withBytes withSock cleanupSocket = do
    bracket (aquireConnectedClientSocket host port) close $ \sock -> do
        withSock sock
        recvFunc sock
  where
    recvFunc sock = do
        mMsg <- recvMsg sock
        case mMsg of
            Nothing -> do
                -- Socket dead
                cleanupSocket sock
            Just msg -> do
                withBytes msg
                recvFunc sock
