module RxTx.Socket (
  RxError (..),
  SocketRxError (..),
  SocketTxError (..),
) where

import Data.Text qualified as T
import Foreign.C (CInt)
import Network.Socket (Socket)

data RxError
  = InsufficientHeader
  | FailedToGetBody
  | FailedToParseBody T.Text
  | ConnectionClosed
  | RxIOError CInt
  deriving (Eq, Show)

data SocketRxError = SocketRxError Socket RxError
  deriving (Show)

newtype SocketTxError = TxIOError CInt
  deriving (Show)
