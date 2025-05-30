module RxTx.TLS (
  TLSRxError (..),
  TLSTxError (..),
) where

import Data.Text qualified as T
import Foreign.C.Types (CInt)
import Network.TLS (TLSException)

data TLSRxError
  = FailedToParseBody T.Text
  | ConnectionClosed
  | IOException CInt
  | RxTLSError TLSException

data TLSTxError
  = TxTLSError TLSException
  | TxIOError CInt
