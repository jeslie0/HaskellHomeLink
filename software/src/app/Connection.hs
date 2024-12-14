module Connection (
    Connection,
    sendMsg,
    killConnection,
    isConnectionAlive,
    mkTCPClientConnection,
    mkTCPServerConnection,
) where

import Connection.TCP qualified as TCP
import Control.Concurrent (newChan, writeChan, ThreadId)
import Data.ByteString qualified as B
import Network.Socket (HostName, ServiceName)
import Threads (
    AsyncComputation,
    isAsyncComputationRunning,
    killAsyncComputation,
    spawnAsyncComputation,
 )

data Connection = Connection
    { _sendMsg :: B.ByteString -> IO ()
    , _connectionThread :: AsyncComputation
    }

sendMsg :: Connection -> B.ByteString -> IO ()
sendMsg = _sendMsg

killConnection :: Connection -> IO ()
killConnection (Connection _ _connectionThread) =
    killAsyncComputation _connectionThread

isConnectionAlive :: Connection -> IO Bool
isConnectionAlive (Connection _ _connectionThread) =
    isAsyncComputationRunning _connectionThread

mkTCPServerConnection :: ServiceName -> (B.ByteString -> IO ()) -> IO Connection
mkTCPServerConnection port withBytes = do
    sendChan <- newChan @B.ByteString
    connectionThread <-
        spawnAsyncComputation $ TCP.mkServerConnection port sendChan withBytes
    pure $
        Connection
            { _sendMsg = writeChan sendChan
            , _connectionThread = connectionThread
            }

mkTCPClientConnection ::
    HostName
    -> ServiceName
    -> (B.ByteString -> IO ())
    -> IO Connection
mkTCPClientConnection host port withBytes = do
    sendChan <- newChan @B.ByteString
    connectionThread <-
        spawnAsyncComputation $ TCP.mkClientConnection host port sendChan withBytes
    pure $
        Connection
            { _sendMsg = writeChan sendChan
            , _connectionThread = connectionThread
            }
