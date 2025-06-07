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
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..), encode, decode)
import Devices (Device (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import RxTx.Connection (AsyncSomeConnection (..), SomeConnection (..), send)
import RxTx.ConnectionRegistry (
  AsyncConnectionRegistry (..),
  mkConnectionRegistry,
 )

data Router = Router
  { _thisDevice :: Device
  , _connectionsRegistry :: AsyncConnectionRegistry Device B.ByteString
  }

$(makeLenses ''Router)

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
  (Serialize msg) =>
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
        Left _ -> pure False
        Right _ -> pure True
    _ -> pure False

trySendMessage ::
  (Serialize msg) => Router -> Device -> msg -> IO Bool
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
  in trySendMsg connMgr src dest hop msg

handleBytes ::
  forall msg.
  (Serialize msg) =>
   B.ByteString
  -> Router
  -> (Device -> msg -> IO ())
  -> IO ()
handleBytes bytes rtr handleMsg = do
  case decode bytes of
    Left errStr -> putStrLn $ "Error extracting bytes: " <> errStr
    Right (MessagePackage src dest msg) ->
      if dest == rtr ^. thisDevice
        then handleMsg src msg
        else void $ tryForwardMessage rtr src dest msg
