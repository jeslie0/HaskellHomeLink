{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : Home.Handler
-- Description : Defines the typeclass and instance for messages being
-- handled by the Home application.
module Home.Handler (
  HomeHandler (..),
  ExHomeHandler (..),
) where

import Control.Concurrent (forkFinally, killThread)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Foldable (forM_)
import Data.IORef (atomicModifyIORef')
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Envelope (ToProxyEnvelope (..))
import EventLoop (MonadEventLoop(..), addMsg, EventLoopT, getLoop, addMsgIO)
import Home.AudioStream (startAudioStream)
import Home.AudioStreamTypes (StationId, StreamStatus (..))
import Home.Env (addRemoteProxyConnection, audioStreamRef, router, Env)
import Islands (Island (..), islands, proxies)
import Lens.Micro ((&), (.~), (?~), (^.), _1, _2, _3)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (ToMessage (..))
import Router (Router, tryForwardMessage, trySendMessage)
import System.Memory (getMemoryInformation)
import TH (makeInstance)
import Logger (reportLog, LogLevel (..))

class HomeHandler msg where
  homeHandler ::
     Island -> msg -> EventLoopT Env (Island, ExHomeHandler) IO ()

data ExHomeHandler = forall a. HomeHandler a => ExHomeHandler a

instance HomeHandler ExHomeHandler where
  homeHandler island (ExHomeHandler msg) = homeHandler island msg

-- * Message instances

-- Make HomeHandler Home.Envelope instance.
$( makeInstance
    ''HomeHandler
    ''Proto.HomeEnvelope
    'Proto.maybe'payload
    ''Proto.HomeEnvelope'Payload
 )

notifyProxyRadioStatus ::
  Router -> Island -> StreamStatus -> StationId -> IO Bool
notifyProxyRadioStatus rtr island status stationId =
  trySendMessage
    rtr
    island
    ( toProxyEnvelope $
        defMessage @Proto.RadioStatusUpdate
          & Proto.status
          .~ toMessage status
          & Proto.currentStationId
          .~ stationId
    )

-- | Try to make a new asynchronous audio stream in a separate
-- thread. If one exists, report the error.
instance HomeHandler Proto.ModifyRadioRequest where
  homeHandler src req = do
    env <- getEnv 
    let notify = do
          (_, status, stationId) <- atomicModifyIORef' (env ^. audioStreamRef) $ \a -> (a, a)
          void $ notifyProxyRadioStatus (env ^. router) src status stationId
    (mThreadId, _, _) <- liftIO . atomicModifyIORef' (env ^. audioStreamRef) $ \a -> (a, a)
    case (mThreadId, req ^. Proto.maybe'station) of
      (Just _, Just _) -> do
        liftIO $ reportLog (env ^. router) Info "Audio stream already exists"
      (Just threadId, Nothing) -> do
        liftIO $ killThread threadId
      (Nothing, Nothing) -> liftIO $ reportLog (env ^. router) Info "Radio not playing"
      (Nothing, Just station) -> do
        let updateStreamStatus status = do
              atomicModifyIORef' (env ^. audioStreamRef) $ \st -> (st, ()) & _1 . _2 .~ status
              notify
        threadId <-
          liftIO $
            forkFinally
              (startAudioStream (env ^. router) (station ^. Proto.url) updateStreamStatus)
              ( \_ -> do
                  atomicModifyIORef' (env ^. audioStreamRef) $ \st ->
                    (st, ())
                      & _1
                      . _1
                      .~ Nothing
                      & _1
                      . _2
                      .~ Off
                  void notify
                  reportLog (env ^. router) Info "Stopping radio stream"
              )
        liftIO
          . atomicModifyIORef'
            (env ^. audioStreamRef)
          $ \st ->
            (st, ())
              & _1
              . _1
              ?~ threadId
              & _1
              . _3
              .~ (station ^. Proto.id)
    void . liftIO $ notify

-- | Spawn a new thread and try to connect to the given TCP server.
instance HomeHandler Proto.ConnectTCP where
  homeHandler _ msg = do
    env <- getEnv
    loop <- getLoop
    liftIO $
      addRemoteProxyConnection @Proto.HomeEnvelope
        (\ (island, msg') -> addMsgIO (island, ExHomeHandler msg') loop )
        (T.unpack $ msg ^. Proto.host)
        (T.unpack $ msg ^. Proto.port)
        (env ^. router)

--
instance HomeHandler Proto.SystemData where
  homeHandler _ msg = do
    env <- getEnv
    liftIO . forM_ (filter (/= Home) islands) $ \island ->
      trySendMessage (env ^. router) island msg

instance HomeHandler Proto.MemoryInformation where
  homeHandler src msg = do
    env <- getEnv
    liftIO $ reportLog (env ^. router) Error $ "Got mem info from " <> T.pack (show src)
    forM_ proxies $ \proxy -> do
      liftIO $ do
        bool <- tryForwardMessage (env ^. router) src proxy $ toProxyEnvelope msg
        putStrLn $ show proxy <> "bool: " <> show bool

instance HomeHandler Proto.CheckMemoryUsage where
  homeHandler _ _ = do
    env <- getEnv
    mMemInfo <- liftIO getMemoryInformation
    case mMemInfo of
      Nothing -> liftIO $ reportLog (env ^. router) Error "Failed to get memory info"
      Just memInfo -> do
        liftIO $ reportLog (env ^. router) Debug "Got log!"
        addMsg (Home, ExHomeHandler $ toMessage @Proto.MemoryInformation memInfo)
