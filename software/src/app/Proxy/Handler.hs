{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.Handler (
  ProxyHandler (..),
  ExProxyHandler (..),
  RemoveWSConn(..)
) where

import Control.Concurrent (modifyMVar_, withMVar)
import Control.Exception (IOException, catch, SomeAsyncException)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict qualified as Map
import Data.Int (Int32)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Devices (Device (..))
import Envelope (wrapHomeMsg)
import EventLoop (EventLoop, EventLoopT, getEnv, addMsgIO, getLoop)
import Lens.Micro ((^.))
import Logger (LogLevel (..), addLog, reportLog)
import Network.WebSockets qualified as WS
import Proto.Camera qualified as Proto
import Proto.Camera_Fields qualified as Proto
import Proto.DeviceData qualified as Proto
import Proto.DeviceData_Fields qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import Proto.Logging qualified as Proto
import Proto.Logging_Fields qualified as Proto
import Proto.Radio qualified as Proto
import Proto.Radio_Fields qualified as Proto
import ProtoHelper (FromMessage (..), toMessage)
import Proxy.Env (
  Env,
  cameraStreamInitialChunk,
  deviceMap,
  logs,
  memoryMap,
  router,
  streamStatusState,
  websocketsMap,
 )
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
      let sysData = fromMessage resp
      in pure $ Map.insert device sysData sysMap

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
          wrapHomeMsg $
            toMessage @Proto.MemoryInformation memInfo

instance ProxyHandler Proto.AddLog where
  proxyHandler _ req = do
    env <- getEnv
    liftIO $ addLog (env ^. logs) (req ^. Proto.log)

instance ProxyHandler Proto.InitialStreamMetaDataChunk where
  proxyHandler _ req = do
    liftIO $ putStrLn "initial stream metadata!"
    loop <- getLoop
    env <- getEnv
    liftIO $
      modifyMVar_ (env ^. cameraStreamInitialChunk) $
        \_ -> pure . Just $ req ^. Proto.metadata
    liftIO $ withMVar (env ^. websocketsMap) $ \wsMap -> broadcast loop wsMap (req ^. Proto.metadata)

instance ProxyHandler Proto.StreamChunk where
  proxyHandler _ req = do
    loop <- getLoop
    liftIO $ putStrLn "got chunk!"
    env <- getEnv
    liftIO $ withMVar (env ^. websocketsMap) $ \wsMap -> broadcast loop wsMap (req ^. Proto.chunk)

broadcast ::
  EventLoop (Device, ExProxyHandler)
  -> Map.Map Int32 WS.Connection
  -> B.ByteString
  -> IO ()
broadcast loop wsMap bytes = do
  now <- getCurrentTime
  void $ Map.traverseWithKey func wsMap
 where
  func key ws = do
    putStrLn $ "Sending data to " <> show key
    WS.sendBinaryData ws bytes `catch`
      \(e :: SomeAsyncException) -> do
        putStrLn $ "Caught exception in WS send."
        addMsgIO (Proxy, ExProxyHandler $ RemoveWSConn key) loop

newtype RemoveWSConn = RemoveWSConn Int32

instance ProxyHandler RemoveWSConn where
  proxyHandler _ (RemoveWSConn key) = do
    env <- getEnv
    liftIO $ modifyMVar_ (env ^. websocketsMap) $ pure . Map.delete key
