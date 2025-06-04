module RxTx.Connection (
  SomeConnection (..),
  mkConnection,
  Connection (..),
  AsyncSomeConnection (..),
) where

import Control.Concurrent (ThreadId)
import RxTx (Rx (..), RxTx, Tx (..))

data Connection msg rxErr txErr = Connection
  { send :: msg -> IO (Either txErr ())
  , recvAndDispatch :: IO (Either rxErr msg)
  , cleanup :: IO ()
  }

mkConnection ::
  forall msg chan rxErr txErr.
  RxTx msg chan rxErr txErr =>
  chan
  -> (chan -> IO ())
  -> IO (Connection msg rxErr txErr)
mkConnection chan cleanup = do
  pure $
    Connection
      { send = sendMsg chan
      , recvAndDispatch = recvMsg chan
      , cleanup = cleanup chan
      }

data SomeConnection msg
  = forall rxErr txErr. SomeConnection (Connection msg rxErr txErr)

data AsyncSomeConnection msg = AsyncSomeConnection
  { _recvThread :: ThreadId
  , _someConn :: SomeConnection msg
  }
