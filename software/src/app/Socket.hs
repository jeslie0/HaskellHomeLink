{-# LANGUAGE OverloadedStrings #-}

module Socket (
    recvNBytes,
    sendAll,
    readHeader,
) where

import Control.Concurrent (
    Chan,
    MVar,
    ThreadId,
    forkIO,
    isEmptyMVar,
    killThread,
    modifyMVar_,
    newChan,
    newEmptyMVar,
    newMVar,
    putMVar,
    readChan,
    readMVar,
    takeMVar,
    threadDelay,
    withMVar,
    writeChan,
 )
import Control.Exception (bracket)
import Control.Monad (forM_, void, when, (>=>))
import Data.ByteString qualified as B
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Serialize qualified as Binary
import Data.Word (Word32)
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket.ByteString (recv, sendAll)
import Socket.TCP (ClientSocket (..), mkClientSocket)

recvNBytes :: Socket -> Int -> IO (Maybe B.ByteString)
recvNBytes sock n = do
    go n ""
  where
    go 0 acc = pure $ Just acc
    go m acc = do
        bytes <- recv sock m
        if B.length bytes == 0
            then pure Nothing
            else
                let bytesToGo = n - B.length bytes
                 in go bytesToGo (acc <> bytes)

{- | We need to read 4 bytes from a socket to get the size of the
 incoming header.
-}
readHeader :: Socket -> IO (Maybe Word32)
readHeader sock = do
    mHdr <- recvNBytes sock 4
    case mHdr of
        Nothing -> pure Nothing
        Just hdr ->
            case Binary.runGet Binary.getWord32le hdr of
                Left str -> putStrLn ("Error: " <> str) >> pure Nothing
                Right n -> pure $ Just n

-- data SocketHandler = SocketHandler
--     { sendMsg :: Chan B.ByteString
--     , subscribers :: MVar [B.ByteString -> IO ()]
--     , killSocketHandler :: IO ()
--     }

-- data CommsMngr a = CommsMngr {_comms :: IORef a}

-- mkClientCommsMngr ::
--     HostName
--     -> ServiceName
--     -> (Chan B.ByteString -> IO ())
--     -> IO (CommsMngr ClientSocket)
-- mkClientCommsMngr hname port withBytes = do
--     let newClientSock commsRef = do
--             clientSock <- mkClientSocket hname port
--             _addClient clientSock $ client commsRef
--             return clientSock

--         updateClientSock commsRef = do
--             newCliSock <- newClientSock c
--             writeIORef commsRef newCliSock

--         client commsRef socket = do
--             connectionActiveMVar <- newEmptyMVar @()
--             sendChan <- newChan @B.ByteString
--             recvChan <- newChan @(Maybe B.ByteString)
--             bracket (forkIO $ sendHandler connectionActiveMVar socket sendChan) killThread $ \_ ->
--                 bracket (forkIO $ recvHandler connectionActiveMVar socket recvChan) killThread $ \_ -> do
--                     readMVar connectionActiveMVar
--                     putStrLn "Connection failed. Retrying in 2 seconds"
--                     updateClientSock commsRef

--     cliSock <- newClientSock
--     commsRef <- newIORef cliSock
--     pure $ CommsMngr {_comms = commsRef}
--   where
--     sendHandler connectionActiveMVar socket sendChan = do
--         keepGoing <- isEmptyMVar connectionActiveMVar
--         when keepGoing $ do
--             bytes <- readChan sendChan
--             sendAll socket bytes
--             sendHandler connectionActiveMVar socket sendChan

--     recvHandler connectionActiveMVar socket recvChan = do
--         keepGoing <- isEmptyMVar connectionActiveMVar
--         when keepGoing $ do
--             mHdr <- readHeader socket
--             case mHdr of
--                 Nothing -> closeConn connectionActiveMVar
--                 Just hdr -> do
--                     mBytes <- recvNBytes socket $ fromIntegral hdr
--                     case mBytes of
--                         Nothing -> closeConn connectionActiveMVar
--                         Just bytes -> do
--                             if B.length bytes == 0
--                                 then closeConn connectionActiveMVar
--                                 else do
--                                     writeChan recvChan $ Just bytes
--                                     recvHandler connectionActiveMVar socket recvChan

--     closeConn connectionActiveMVar = do
--         putMVar connectionActiveMVar ()

-- -- sendMsg :: SocketHandler -> B.ByteString -> IO ()
-- -- sendMsg (SocketHandler _ _ sendChan _ _) = writeChan sendChan

-- -- addSubscriber :: SocketHandler -> (B.ByteString -> IO ()) -> IO ()
-- -- addSubscriber (SocketHandler _ _ _ subscribers _) newSub = do
-- --     modifyMVar_ subscribers $ \subs -> pure (newSub : subs)

-- -- makeGenericSocketHandler :: ((Socket -> IO ()) -> IO ()) -> IO SocketHandler
-- -- makeGenericSocketHandler withSocket = do
-- --     sockMVar <- newEmptyMVar @Socket
-- --     killMVar <- newMVar ()
-- --     clientThread <- forkIO $ withSocket $ \socket -> do
-- --         putMVar sockMVar socket
-- --         putMVar killMVar ()
-- --     socket <- takeMVar sockMVar
-- --     sendChan <- newChan @B.ByteString
-- --     subscribers <- newMVar []
-- --     sendThread <- forkIO $ senderThreadAction sendChan socket
-- --     recvThread <- forkIO $ recvThreadAction subscribers socket
-- --     return $
-- --         SocketHandler
-- --             { sendThread = sendThread
-- --             , recvThread = recvThread
-- --             , sendChan = sendChan
-- --             , subscribers = subscribers
-- --             , killSocketHandler = do
-- --                 takeMVar killMVar
-- --                 forM_ [clientThread, sendThread, recvThread] killThread
-- --             }
-- --   where
-- --     recvThreadAction subscribers socket = do
-- --         hdr <- readHeader socket
-- --         bytes <- recvNBytes socket (fromIntegral hdr)
-- --         print bytes
-- --         withMVar subscribers $ \subList -> forM_ subList ($ bytes)
-- --         recvThreadAction subscribers socket

-- --     senderThreadAction sendChan socket = do
-- --         bytes <- readChan sendChan
-- --         sendAll socket bytes
-- --         senderThreadAction sendChan socket

-- -- makeClientSocketHandler ::
-- --     HostName -> ServiceName -> IO SocketHandler
-- -- makeClientSocketHandler hostname port = do
-- --     makeGenericSocketHandler $ runTCPClient hostname port

-- -- makeServerSocketHandler :: ServiceName -> IO SocketHandler
-- -- makeServerSocketHandler port =
-- --     makeGenericSocketHandler $ runTCPServer Nothing port

-- -- foo = do
-- --     mngr <- mkClientCommsMngr "127.0.0.1" "3000" $ readChan >=> print
-- --     _
