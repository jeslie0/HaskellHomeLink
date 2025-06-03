module RxTx.Connection.TLS where

import Control.Exception (SomeException (..), catch)
import Network.Socket (Socket)
import Network.TLS (
  Context,
  TLSException,
  TLSParams,
  bye,
  contextNew,
  handshake,
 )
import RxTx ( RxTx )
import RxTx.Connection (Connection (..), mkConnection)
import RxTx.TLS (TLSRxError, TLSTxError)

upgradeSocket ::
  forall params msg.
  (TLSParams params, RxTx msg Context TLSRxError TLSTxError) =>
  params
  -> Socket
  -> IO (Maybe (Connection msg TLSRxError TLSTxError))
upgradeSocket params sock = do
  ctx <- contextNew sock params
  putStrLn "Upgrading socket"
  handshakeRes <-
    (handshake ctx >> pure Nothing)
      `catch` handleTlsErr
      `catch` handleIOError
  case handshakeRes of
    Nothing -> do
      putStrLn "Handshake done!"
      Just <$> mkConnection @msg @Context @TLSRxError @TLSTxError ctx bye
    Just err -> do
      print err
      pure Nothing
 where
  handleTlsErr (err :: TLSException) = pure (Just $ SomeException err)
  handleIOError (err :: IOError) = pure (Just $ SomeException err)
