{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Router (
  Router,
  Routable (..),
  MessagePackage (..),
  thisIsland,
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
import Data.Serialize (Serialize (..), runGet)
import Data.Typeable (Typeable)
import Islands (Island (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import RxTx.Connection (AsyncSomeConnection (..), SomeConnection (..), send)
import RxTx.ConnectionRegistry (
  AsyncConnectionRegistry (..),
  mkConnectionRegistry,
 )

data Routable = forall msg. (Typeable msg, Serialize msg) => Routable msg
  deriving (Typeable)

instance Serialize Routable where
  get = fail "Deserialisation not implemented"

  put (Routable msg) =
    put msg

data Router = Router
  { _thisIsland :: Island
  , _connectionsRegistry :: AsyncConnectionRegistry Island Routable
  }

$(makeLenses ''Router)

nextHop ::
  Island
  -- ^ Source
  -> Island
  -- ^ Destination
  -> Maybe Island
nextHop Home _ = Nothing
nextHop _ Home = Nothing
nextHop src dest
  | src == dest = Just dest
  | otherwise = Just Home

mkRouter :: Island -> IO Router
mkRouter island = do
  Router island <$> mkConnectionRegistry

data MessagePackage msg = MessagePackage
  { source :: !Island
  , destination :: !Island
  , msg :: !msg
  }

instance Serialize msg => Serialize (MessagePackage msg) where
  get = do
    src <- get @Island
    dest <- get @Island
    msg <- get @msg
    pure $ MessagePackage src dest msg

  put (MessagePackage src dest msg) = do
    put src
    put dest
    put msg

trySendMsg ::
  forall msg.
  (Typeable msg, Serialize msg) =>
  AsyncConnectionRegistry Island Routable
  -> Island
  -- ^ Source of msg
  -> Island
  -- ^ Target of msg
  -> Island
  -- ^ Next hop for message
  -> msg
  -> IO Bool
trySendMsg (AsyncConnectionRegistry mvar) src finalDest nextDest msg = do
  withMVar mvar $ \connMap -> case Map.lookup nextDest connMap of
    Just (AsyncSomeConnection _ (SomeConnection conn)) -> do
      eErr <- send conn $ Routable $ MessagePackage src finalDest msg
      case eErr of
        Left _ -> pure False
        Right _ -> pure True
    _ -> pure False

trySendMessage ::
  (Typeable msg, Serialize msg) => Router -> Island -> msg -> IO Bool
trySendMessage (Router island connMgr) dest msg =
  let hop = fromMaybe dest $ nextHop island dest
  in trySendMsg connMgr island dest hop msg

tryForwardMessage ::
  (Typeable msg, Serialize msg) =>
  Router
  -> Island
  -> Island
  -> msg
  -> IO Bool
tryForwardMessage (Router island connMgr) src dest msg =
  let hop = fromMaybe dest $ nextHop island dest
  in trySendMsg connMgr src dest hop msg

handleBytes ::
  forall msg.
  (Typeable msg, Serialize msg) =>
  (Island -> msg -> IO ())
  -> Router
  -> B.ByteString
  -> IO ()
handleBytes handleMsg rtr bytes = do
  case runGet get bytes of
    Left errStr -> putStrLn $ "Error extracting bytes: " <> errStr
    Right (MessagePackage src dest msg) ->
      if dest == rtr ^. thisIsland
        then handleMsg src msg
        else void $ tryForwardMessage rtr src dest msg
