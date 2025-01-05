{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Router (
  Router,
  thisIsland,
  connectionsManager,
  mkRouter,
  trySendMessage,
  forwardMsg,
  handleBytes,
) where

import ConnectionManager (
  ConnectionManager,
  Island (..),
  getSrcDest,
  mkConnectionManager,
  trySendBytes,
 )
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..), runPut)
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
trySendMessage (Router island connMgr) dest msg = do
  -- Get next hop
  let
    hop = fromMaybe dest $ nextHop island dest
    bytes = toBytes msg
    addressedBytes = runPut (put island) <> runPut (put dest) <> bytes
  trySendBytes connMgr hop addressedBytes

forwardMsg :: Router -> Island -> B.ByteString -> IO Bool
forwardMsg (Router _island connMgr) dest bytes = do
  trySendBytes connMgr dest bytes

handleBytes ::
  forall msg.
  Msg msg =>
  ((Island, msg) -> IO ())
  -> Router
  -> B.ByteString
  -> IO ()
handleBytes handleMsg rtr bytes = do
  let (srcDest, payload) = B.splitAt 2 bytes
  case getSrcDest srcDest of
    Nothing -> putStrLn "Could not get source or dest from message"
    Just (src, dest) -> do
      if dest == rtr ^. thisIsland
        then case fromBytes @msg payload of
          Left err -> putStrLn err
          Right envelope -> handleMsg (src, envelope)
        else void $ forwardMsg rtr dest bytes
