{-# LANGUAGE OverloadedStrings #-}

module Socket (
    recvNBytes,
    sendAll,
    readHeader,
    makeClientSocketHandler,
    makeServerSocketHandler,
    SocketHandler (),
    sendMsg,
    addSubscriber,
    killSocketHandler,
) where

import Data.Serialize qualified as Binary
import Control.Concurrent (
    Chan,
    MVar,
    ThreadId,
    forkIO,
    modifyMVar_,
    newChan,
    newEmptyMVar,
    newMVar,
    putMVar,
    readChan,
    takeMVar,
    writeChan, withMVar,
 )
import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.Serialize qualified as Binary
import Data.Word (Word32)
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket.ByteString (recv, sendAll)
import Socket.TCP (runTCPClient, runTCPServer)

recvNBytes :: Socket -> Int -> IO B.ByteString
recvNBytes sock n = do
    go n ""
  where
    go 0 acc = putStrLn ("Got " <> show n <> " bytes!:" <> show acc) >> pure acc
    go m acc = do
        bytes <- recv sock m
        print bytes
        let bytesToGo = n - B.length bytes
        go bytesToGo (acc <> bytes)

{- | We need to read 4 bytes from a socket to get the size of the
 incoming header.
-}
readHeader :: Socket -> IO Word32
readHeader sock = do
    hdr <- recvNBytes sock 4
    case Binary.runGet Binary.getWord32le hdr of
        Left str -> putStrLn ("Error: " <> str) >> pure 0
        Right n -> pure n

data SocketHandler = SocketHandler
    { sendThread :: ThreadId
    , recvThread :: ThreadId
    , sendChan :: Chan B.ByteString
    , subscribers :: MVar [B.ByteString -> IO ()]
    , killSocketHandler :: IO ()
    }

sendMsg :: SocketHandler -> B.ByteString -> IO ()
sendMsg (SocketHandler _ _ sendChan _ _) = writeChan sendChan

addSubscriber :: SocketHandler -> (B.ByteString -> IO ()) -> IO ()
addSubscriber (SocketHandler _ _ _ subscribers _) newSub = do
    modifyMVar_ subscribers $ \subs -> pure (newSub : subs)

makeGenericSocketHandler ::  ((Socket -> IO ()) -> IO ()) -> IO SocketHandler
makeGenericSocketHandler  withSocket = do
    sockMVar <- newEmptyMVar @Socket
    killMVar <- newMVar ()
    clientThread <- forkIO $ withSocket $ \socket -> do
        putMVar sockMVar socket
        putMVar killMVar ()
    socket <- takeMVar sockMVar
    sendChan <- newChan @B.ByteString
    subscribers <- newMVar []
    sendThread <- forkIO $ senderThreadAction sendChan socket
    recvThread <- forkIO $ recvThreadAction subscribers socket
    return $
        SocketHandler
            { sendThread = sendThread
            , recvThread = recvThread
            , sendChan = sendChan
            , subscribers = subscribers
            , killSocketHandler = takeMVar killMVar
            }
  where
    recvThreadAction subscribers socket = do
        hdr <- readHeader socket
        bytes <- recvNBytes socket (fromIntegral hdr)
        print bytes
        withMVar subscribers $ \subList -> forM_ subList ($ bytes)
        recvThreadAction subscribers socket

    senderThreadAction sendChan socket = do
        bytes <- readChan sendChan
        sendAll socket bytes
        senderThreadAction sendChan socket


makeClientSocketHandler ::
    HostName -> ServiceName -> IO SocketHandler
makeClientSocketHandler hostname port = do
  makeGenericSocketHandler $ runTCPClient hostname port


makeServerSocketHandler :: ServiceName -> IO SocketHandler
makeServerSocketHandler port =
  makeGenericSocketHandler $ runTCPServer Nothing port
