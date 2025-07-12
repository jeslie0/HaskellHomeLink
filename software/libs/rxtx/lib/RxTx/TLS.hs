module RxTx.TLS (
  TLSRxError (..),
  TLSTxError (..),
  RxError (..),
  TxError (..),
) where

import Data.Text qualified as T
import Foreign.C.Types (CInt)
import Network.TLS (Context, TLSException)

data RxError
  = FailedToParseBody T.Text
  | ConnectionClosed
  | IOException CInt
  | RxTLSError TLSException
  deriving (Show)

data TLSRxError = TLSRxError RxError deriving Show

data TxError
  = TxTLSError TLSException
  | TxIOError CInt
  deriving (Show)

data TLSTxError = TLSTxError TxError

instance Show TLSTxError where
  show (TLSTxError err) = show err
