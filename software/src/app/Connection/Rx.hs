{-# LANGUAGE MonoLocalBinds #-}

module Connection.Rx (Rx (..), SocketRxError (..), ChannelRxError) where

import Connection.Socket (readHeader, recvNBytes)
import Control.Concurrent (Chan, readChan)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Msg (Msg (..))
import Network.Socket (Socket)
import Network.TLS (Context, recvData)
import Utilities.Either (toEither)

class Rx conn a err where
  recv :: MonadIO m => conn -> m (Either err a)

data SocketRxError
  = InsufficientHeader
  | FailedToGetBody
  | FailedToParseBody T.Text
  | ConnectionClosed

-- instance Msg m => Rx Socket m SocketRxError where
--   recv sock =
--     runExceptT $ do
--       header <- ExceptT . liftIO $ toEither InsufficientHeader <$> readHeader sock
--       body <-
--         ExceptT . liftIO $
--           toEither FailedToGetBody <$> recvNBytes sock (fromIntegral header)
--       ExceptT . pure $ first (FailedToParseBody . T.pack) (fromBytes body)

newtype TLSRxError
  = FailedToParseTLSBody T.Text

instance Rx Socket B.ByteString SocketRxError where
  recv sock =
    runExceptT $ do
      header <- ExceptT . liftIO $ toEither InsufficientHeader <$> readHeader sock
      ExceptT . liftIO $
        toEither FailedToGetBody <$> recvNBytes sock (fromIntegral header)

instance Msg m => Rx Context m TLSRxError where
  recv ctx = do
    bytes <- liftIO . recvData $ ctx
    pure $ first (FailedToParseTLSBody . T.pack) (fromBytes bytes)

data ChannelRxError

instance Rx (Chan a) a ChannelRxError where
  recv chan = Right <$> (liftIO . readChan $ chan)

instance Rx (Chan a, Chan b) a ChannelRxError where
  recv (chan, _) = Right <$> (liftIO . readChan $ chan)
