{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Router (
  Router,
  thisIsland,
  connectionsManager,
  mkRouter,
  trySendMessage,
  handleBytes,
  tryForwardMessage,
) where

import ConnectionManager (
  ConnectionManager,
  mkConnectionManager,
  trySendMsg,
 )
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Islands (Island (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg (..))

data Router = Router
  { _thisIsland :: Island
  , _connectionsManager :: ConnectionManager
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
  Router island <$> mkConnectionManager

trySendMessage :: Msg msg => Router -> Island -> msg -> IO Bool
trySendMessage (Router island connMgr) dest msg =
  let hop = fromMaybe dest $ nextHop island dest
  in trySendMsg connMgr island dest hop msg

tryForwardMessage :: Msg msg => Router -> Island -> Island -> msg -> IO Bool
tryForwardMessage (Router island connMgr) src dest msg =
  let hop = fromMaybe dest $ nextHop island dest
  in trySendMsg connMgr src dest hop msg

handleBytes ::
  forall msg.
  Msg msg =>
  ((Island, msg) -> IO ())
  -> Router
  -> B.ByteString
  -> IO ()
handleBytes handleMsg rtr bytes = do
  case fromBytes bytes of
    Left errStr -> putStrLn $ "Error extracting bytes: " <> errStr
    Right (src :: Island, dest :: Island, msg :: msg) ->
      if dest == rtr ^. thisIsland
      then handleMsg (src, msg)
        else void $ tryForwardMessage rtr src dest msg
