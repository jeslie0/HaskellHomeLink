{-# LANGUAGE TemplateHaskell #-}

module Proxy.Handler (
  ProxyHandler (..),
  ExProxyHandler (..),
) where

import Control.Concurrent (modifyMVar_)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
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
import System.Memory (MemoryInformation, getMemoryInformation)
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
    env <- ask
    liftIO $ modifyMVar_ (env ^. memoryMap) $ \memMap ->
      let
        secondsPerDay = 1440 :: Int
        alterFunc ::
          Maybe (V.Vector MemoryInformation) -> IO (Maybe (V.Vector MemoryInformation))
        alterFunc Nothing = pure . Just $ V.singleton (fromMessage resp)
        alterFunc (Just vec) =
          if V.length vec < secondsPerDay * 2
            then
              pure . Just $ V.snoc vec (fromMessage resp)
            else fmap
              Just
              $ do
                mVec <- V.unsafeThaw vec
                forM_ [0 .. VM.length mVec - 2] $ \(i :: Int) ->
                  VM.read mVec (i + 1) >>= VM.write mVec i
                VM.write mVec (VM.length mVec - 1) (fromMessage resp)
                V.unsafeFreeze mVec
      in
        liftIO $ Map.alterF alterFunc src memMap

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
