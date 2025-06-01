module RxTx.Connection.TLS where

import Data.Serialize (Serialize)
import Network.Socket (Socket)
import Network.TLS (TLSParams, bye, contextNew)
import RxTx.Connection (Connection (..), mkConnection)
import RxTx.TLS (TLSRxError, TLSTxError)

upgradeSocket ::
  (TLSParams params, Serialize msg) =>
  params
  -> Socket
  -> IO (Connection msg TLSRxError TLSTxError)
upgradeSocket params sock = do
  ctx <- contextNew sock params
  mkConnection ctx bye
