{-# LANGUAGE TemplateHaskell #-}

module Proxy.Handler (
  ProxyHandler (..),
  ExProxyHandler (..),
) where

import Control.Concurrent (modifyMVar_)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Envelope (toEnvelope)
import EventLoop (EventLoop)
import Islands (Island (..))
import Lens.Micro ((^.))
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (..), toMessage)
import Proxy.Env (EnvT, memoryMap, router, streamStatusState, systemMap)
import Router (trySendMessage)
import State (fulfilPromise)
import System.Memory (getMemoryInformation)
import TH (makeInstance)

class ProxyHandler msg where
  proxyHandler ::
    EventLoop EnvT (Island, ExProxyHandler) -> Island -> msg -> EnvT ()

data ExProxyHandler = forall a. ProxyHandler a => ExProxyHandler a

instance ProxyHandler ExProxyHandler where
  proxyHandler loop island (ExProxyHandler msg) = proxyHandler loop island msg

-- * Message instances

-- Make ProxyHandler Proto.ProxyRecieveEnvelope instance.
$( makeInstance
    ''ProxyHandler
    ''Proto.ProxyEnvelope
    'Proto.maybe'payload
    ''Proto.ProxyEnvelope'Payload
 )

-- | Received acknowledgement from Home for ModifyRadioRequest
instance ProxyHandler Proto.RadioStatusUpdate where
  proxyHandler _ _ resp = do
    env <- ask
    liftIO $
      fulfilPromise
        (fromMessage $ resp ^. Proto.status, resp ^. Proto.currentStationId)
        (env ^. streamStatusState)

-- | Received acknowledgement from Home for ModifyRadioRequest. Update
-- promise so HTTP Handler can return.
instance ProxyHandler Proto.GetRadioStatusResponse where
  proxyHandler _ _ resp = do
    env <- ask
    liftIO $
      fulfilPromise
        (fromMessage $ resp ^. Proto.status, resp ^. Proto.currentStationId)
        (env ^. streamStatusState)

instance ProxyHandler Proto.IslandSystemData where
  proxyHandler _ _ resp = do
    env <- ask
    liftIO $ modifyMVar_ (env ^. systemMap) $ \sysMap ->
      let
        island = fromMessage $ resp ^. Proto.island
        sysData = fromMessage $ resp ^. Proto.systemData
      in
        pure $ Map.insert island sysData sysMap

instance ProxyHandler Proto.MemoryInformation where
  proxyHandler _ src resp = do
    liftIO $ putStrLn $ "meminfo from " <> show src
    env <- ask
    liftIO $ modifyMVar_ (env ^. memoryMap) $ \memMap ->
      let
        secondsPerDay = 1440 :: Int
        alterFunc Nothing = Just $ V.singleton (fromMessage resp)
        alterFunc (Just vec) =
          Just $
            if V.length vec < secondsPerDay * 2
              then
                V.snoc vec (fromMessage resp)
              else V.snoc (V.unsafeDrop 1 vec) (fromMessage resp)
      in
        pure $ Map.alter alterFunc src memMap

instance ProxyHandler Proto.CheckMemoryUsage where
  proxyHandler _ _ _ = do
    env <- ask
    mMemInfo <- liftIO getMemoryInformation
    case mMemInfo of
      Nothing -> liftIO . putStrLn $ "Failed to get memory info"
      Just memInfo -> do
        void . liftIO . trySendMessage (env ^. router) Home $
          toEnvelope $
            toMessage @Proto.MemoryInformation memInfo
