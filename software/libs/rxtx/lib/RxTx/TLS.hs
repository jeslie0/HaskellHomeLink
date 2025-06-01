module RxTx.TLS (
  TLSRxError (..),
  TLSTxError (..),
  RxError (..),
  TxError (..),
) where

import Data.Text qualified as T
import Foreign.C.Types (CInt)
import Network.TLS (TLSException, Context)

data TLSRxError = TLSRxError Context RxError

data RxError
  = FailedToParseBody T.Text
  | ConnectionClosed
  | IOException CInt
  | RxTLSError TLSException

data TLSTxError = TLSTxError Context TxError
data TxError
  = TxTLSError TLSException
  | TxIOError CInt
