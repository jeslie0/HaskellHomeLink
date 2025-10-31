{-# LANGUAGE MonoLocalBinds #-}

module Router (
  Router,
  Sender (..),
  MessagePackage(..),
  thisDevice,
  connectionsRegistry,
  mkRouter,
  removeDevice,
  trySendMessage,
  tryForwardMessage,
  addDevice,
  nextHop,
  getDevice,
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Serialize (Serialize (..))
import Devices (Device (..))
import HAsio.Error.ErrorStack (ErrorStack)

newtype Sender = Sender
  {sendMsg :: forall msg. Serialize msg => msg -> ExceptT ErrorStack IO ()}

data Router = Router
  { thisDevice :: Device
  , -- This contains all reachable devices, including via jumps
    connectionsRegistry :: IORef (Map.Map Device Sender)
  }

addDevice :: Router -> Device -> Sender -> IO ()
addDevice router dev sender = do
  devMap <- readIORef (connectionsRegistry router)
  writeIORef (connectionsRegistry router) $ Map.insert dev sender devMap

getDevice :: Router -> Device
getDevice = thisDevice

removeDevice :: Router -> Device -> IO ()
removeDevice router dev = do
  devMap <- readIORef (connectionsRegistry router)
  writeIORef (connectionsRegistry router) $ Map.delete dev devMap

-- | Determine the next island to hop to. Returns Nothing if we are on
-- the final island (no more hops)
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
mkRouter  island = do
  Router island <$> newIORef Map.empty

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

trySendMsgImpl ::
  forall msg.
  Serialize msg =>
  Map.Map Device Sender
  -> Device
  -> msg
  -> ExceptT ErrorStack IO ()
trySendMsgImpl devMap dest msg = do
  case Map.lookup dest devMap of
    Just sender ->
      case sender of
        Sender sendMsg -> do
          sendMsg msg
    _ -> do
      liftIO $ putStrLn $ "Failed to send message to : " <> show dest

trySendMessage ::
  Serialize msg => Router -> Device -> msg -> ExceptT ErrorStack IO ()
trySendMessage router dest msg = do
  devMap <- liftIO $ readIORef (connectionsRegistry router)
  trySendMsgImpl devMap dest (MessagePackage (thisDevice router) dest msg)

tryForwardMessage ::
  Serialize msg =>
  Router
  -> Device
  -> msg
  -> ExceptT ErrorStack IO ()
tryForwardMessage router dest msg = do
  devMap <- liftIO $ readIORef (connectionsRegistry router)
  trySendMsgImpl devMap dest msg

-- handleBytes ::
--   forall msg.
--   Serialize msg =>
--   B.ByteString
--   -> Router
--   -> (Device -> msg -> IO ())
--   -> IO ()
-- handleBytes bytes rtr handleMsg = do
--   print $ "got " <> show (B.length bytes) <> " bytes"
--   case decode bytes of
--     Left errStr -> putStrLn $ "Error extracting bytes: " <> errStr
--     Right (MessagePackage src dest msg) ->
--       if dest == rtr ^. thisDevice
--         then handleMsg src msg
--         else do
--           print $ "Forwarding " <> show (B.length bytes) <> "bytes to " <> show dest
--           void $ tryForwardMessage rtr src dest msg
