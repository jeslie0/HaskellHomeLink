{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Router (
  Router,
  MessagePackage (..),
  thisDevice,
  connectionsRegistry,
  mkRouter,
  trySendMessage,
  handleBytes,
  tryForwardMessage,
) where

import Control.Concurrent (withMVar)
import Control.Monad (void, when)
import Data.ByteString qualified as B
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..), decode, encode)
import Devices (Device (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Data.Time.Clock
import Data.Time.Format
import RxTx.Connection (
  AsyncSomeConnection (..),
  SomeConnection (..),
  send,
  showTxErr,
 )
import RxTx.ConnectionRegistry (
  AsyncConnectionRegistry (..),
  mkConnectionRegistry,
 )
import RxTx.Tx (showTxErr)

data Router = Router
  { _thisDevice :: Device
  , _connectionsRegistry :: AsyncConnectionRegistry Device B.ByteString
  }

$(makeLenses ''Router)

-- | Determine the next island to hop to. Returns Nothing if we are on
-- the final island
nextHop ::
  Device
  -- ^ Source
  -> Device
  -- ^ Destination
  -> Maybe Device
nextHop Home _ = Nothing
nextHop _ Home = Nothing
nextHop src dest
  | src == dest = Just dest
  | otherwise = Just Home

mkRouter :: Device -> IO Router
mkRouter island = do
  Router island <$> mkConnectionRegistry

data MessagePackage msg = MessagePackage
  { source :: {-# UNPACK #-} !Device
  , destination :: {-# UNPACK #-} !Device
  , msg :: msg
  }

instance Serialize msg => Serialize (MessagePackage msg) where
  get = do
    src <- get @Device
    dest <- get @Device
    msg <- get @msg
    pure $ MessagePackage src dest msg

  put (MessagePackage src dest msg) = do
    put src
    put dest
    put msg

trySendMsg ::
  forall msg.
  Serialize msg =>
  AsyncConnectionRegistry Device B.ByteString
  -> Device
  -- ^ Source of msg
  -> Device
  -- ^ Target of msg
  -> Device
  -- ^ Next hop for message
  -> msg
  -> IO Bool
trySendMsg (AsyncConnectionRegistry mvar) src finalDest nextDest msg = do
  withMVar mvar $ \connMap -> case Map.lookup nextDest connMap of
    Just (AsyncSomeConnection _ (SomeConnection conn)) -> do
      eErr <- send conn $ encode $ MessagePackage src finalDest msg
      case eErr of
        Left eErr -> do
          putStrLn $ "Error sending message: " <> RxTx.Connection.showTxErr conn eErr
          pure False
        Right _ -> pure True
    _ -> do
      putStrLn $ "Failed to send message: " <> show src <> ", " <> show finalDest
      pure False

trySendMessage ::
  Serialize msg => Router -> Device -> msg -> IO Bool
trySendMessage (Router island connMgr) dest msg =
  let hop = fromMaybe dest $ nextHop island dest
  in trySendMsg connMgr island dest hop msg

tryForwardMessage ::
  Serialize msg =>
  Router
  -> Device
  -> Device
  -> msg
  -> IO Bool
tryForwardMessage (Router island connMgr) src dest msg =
  let hop = fromMaybe dest $ nextHop island dest
  in do
    now <- getCurrentTime
    -- print $ formatTime defaultTimeLocale "%H:%M:%S" now <> " Forwarding message"
    trySendMsg connMgr src dest hop msg

handleBytes ::
  forall msg.
  Serialize msg =>
  B.ByteString
  -> Router
  -> (Device -> msg -> IO ())
  -> IO ()
handleBytes bytes rtr handleMsg = do
  print $ "got " <> show (B.length bytes) <> " bytes"
  case decode bytes of
    Left errStr -> putStrLn $ "Error extracting bytes: " <> errStr
    Right (MessagePackage src dest msg) ->
      if dest == rtr ^. thisDevice
        then handleMsg src msg
        else do
          print $ "Forwarding " <> show (B.length bytes) <> "bytes to " <> show dest
          void $ tryForwardMessage rtr src dest msg
