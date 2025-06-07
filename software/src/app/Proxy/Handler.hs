{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.Handler (
  ProxyHandler (..),
  ExProxyHandler (..),
) where

import Control.Concurrent (modifyMVar_)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Envelope (toEnvelope)
import EventLoop (EventLoopT, getEnv)
import Devices (Device (..))
import Lens.Micro ((^.))
import Logger (LogLevel (..), addLog, reportLog)
import ProtoHelper (FromMessage (..), toMessage)
import Proxy.Env (Env, logs, memoryMap, router, streamStatusState, deviceMap)
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import Proto.Radio qualified as Proto
import Proto.Radio_Fields qualified as Proto
import Proto.Logging qualified as Proto
import Proto.Logging_Fields qualified as Proto
import Proto.DeviceData qualified as Proto
import Proto.DeviceData_Fields qualified as Proto
import Router (trySendMessage)
import State (fulfilPromise)
import System.Memory (MemoryInformation, getMemoryInformation)
import TH (makeInstance)

class ProxyHandler msg where
  proxyHandler ::
    Device -> msg -> EventLoopT Env (Device, ExProxyHandler) IO ()

data ExProxyHandler = forall a. ProxyHandler a => ExProxyHandler a

instance ProxyHandler ExProxyHandler where
  proxyHandler device' (ExProxyHandler msg) = proxyHandler device' msg

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
  proxyHandler _ resp = do
    env <- getEnv
    liftIO $
      fulfilPromise
        (fromMessage $ resp ^. Proto.status, resp ^. Proto.currentStationId)
        (env ^. streamStatusState)

-- | Received acknowledgement from Home for ModifyRadioRequest. Update
-- promise so HTTP Handler can return.
instance ProxyHandler Proto.GetRadioStatusResponse where
  proxyHandler _ resp = do
    env <- getEnv
    liftIO $
      fulfilPromise
        (fromMessage $ resp ^. Proto.status, resp ^. Proto.currentStationId)
        (env ^. streamStatusState)

instance ProxyHandler Proto.DeviceData where
  proxyHandler device resp = do
    env <- getEnv
    liftIO $ modifyMVar_ (env ^. deviceMap) $ \sysMap ->
      let
        sysData = fromMessage resp 
      in
        pure $ Map.insert device sysData sysMap

instance ProxyHandler Proto.MemoryInformation where
  proxyHandler src resp = do
    env <- getEnv
    liftIO $ modifyMVar_ (env ^. memoryMap) $ \memMap ->
      let
        secondsPerDay = 1440 :: Int
        alterFunc ::
          Maybe (V.Vector MemoryInformation) -> IO (Maybe (V.Vector MemoryInformation))
        alterFunc Nothing = pure . Just $ V.singleton (fromMessage resp)
        alterFunc (Just vec) =
          if V.length vec < secondsPerDay * 2
            then do
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
  proxyHandler _ _ = do
    env <- getEnv
    mMemInfo <- liftIO getMemoryInformation
    case mMemInfo of
      Nothing -> liftIO $ reportLog (env ^. router) Error "Failed to get memory info"
      Just memInfo -> do
        void . liftIO . trySendMessage (env ^. router) Home $
          toEnvelope $
            toMessage @Proto.MemoryInformation memInfo

instance ProxyHandler Proto.AddLog where
  proxyHandler _ req = do
    env <- getEnv
    liftIO $ addLog (env ^. logs) (req ^. Proto.log)
