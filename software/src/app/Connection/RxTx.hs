module Connection.RxTx where

import Connection.Rx (ChannelRxError, Rx (..))
import Connection.Tx (Tx (..))
import Control.Concurrent (Chan)

type RxTx conn msgIn msgOut err = (Rx conn msgIn err, Tx conn msgOut)

newtype RxTxChan a b = RxTxChan (Chan a, Chan b)

instance Rx (RxTxChan a b) a ChannelRxError where
  recv (RxTxChan (chan, _)) = recv chan

instance Tx (RxTxChan a b) b where
  send (RxTxChan (_, chan)) = send chan
