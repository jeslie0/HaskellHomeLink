{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module : Home.Handler
Description : Defines the typeclass and instance for messages being
handled by the Home application.
-}
module Home.Handler (
    HomeHandler (..),
    ExHomeHandler (..),
) where

import ConnectionManager (Island (..), islands)
import Control.Concurrent (forkFinally, killThread)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.Foldable (forM_)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Envelope (ToProxyEnvelope (..))
import EventLoop (EventLoop, addMsg)
import Home.AudioStream (StreamId, StreamStatus (..), startAudioStream)
import Home.Env (EnvT, addRemoteProxyConnection, audioStreamRef, router)
import Lens.Micro ((&), (.~), (?~), (^.), _1, _2)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (ToMessage (..))
import Router (Router, trySendMessage)
import TH (makeInstance)

class HomeHandler msg where
    homeHandler ::
        EventLoop EnvT (Island, ExHomeHandler) -> Island -> msg -> EnvT ()

data ExHomeHandler = forall a. (HomeHandler a) => ExHomeHandler a

instance HomeHandler ExHomeHandler where
    homeHandler loop island (ExHomeHandler msg) = homeHandler loop island msg

-- * Message instances

-- Make HomeHandler Home.Envelope instance.
$( makeInstance
    ''HomeHandler
    ''Proto.HomeEnvelope
    'Proto.maybe'payload
    ''Proto.HomeEnvelope'Payload
 )

notifyProxyRadioStatus ::
    Router -> Island -> StreamStatus -> StreamId -> IO Bool
notifyProxyRadioStatus rtr island status streamId =
    trySendMessage
        rtr
        island
        ( toProxyEnvelope $
            defMessage @Proto.RadioStatusUpdate
                & Proto.status
                .~ toMessage status
                & Proto.currentStationId
                .~ streamId
        )

{- | Try to make a new asynchronous audio stream in a separate
thread. If one exists, report the error.
-}
instance HomeHandler Proto.ModifyRadioRequest where
    homeHandler _ src req = do
        env <- ask
        let notify = do
                (_, status, stationId) <- readIORef (env ^. audioStreamRef)
                notifyProxyRadioStatus (env ^. router) src status stationId
        (mThreadId, _, _) <- liftIO . readIORef $ env ^. audioStreamRef
        case (mThreadId, req ^. Proto.maybe'station) of
            (Just _, Just _) -> do
                liftIO $ putStrLn "Audio stream already exists"
            (Just threadId, Nothing) -> do
                liftIO $ killThread threadId
            (Nothing, Nothing) -> liftIO $ putStrLn "Radio not playing"
            (Nothing, Just station) -> do
                let updateStreamStatus status =
                        atomicModifyIORef' (env ^. audioStreamRef) $ \st -> (st, ()) & _1 . _2 .~ status
                threadId <-
                    liftIO $
                        forkFinally
                            (startAudioStream (station ^. Proto.url) updateStreamStatus)
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
                            )
                liftIO
                    . atomicModifyIORef'
                        (env ^. audioStreamRef)
                    $ \st -> (st, ()) & _1 . _1 ?~ threadId
        void . liftIO $ notify

-- -- | Stop playing an audio stream if one is playing.
-- instance HomeHandler Proto.StopRadio where
--     homeHandler _ src _ = do
--         env <- ask
--         liftIO . void $
--             readIORef (env ^. audioStreamRef) >>= \case
--                 Nothing -> do
--                     putStrLn "Radio not playing"
--                 Just (threadId, _) -> do
--                     killThread threadId
--                     writeIORef (env ^. audioStreamRef) Nothing

--         void . liftIO $
--             notifyProxyRadioStatus (env ^. router) src (StreamStatus Nothing)

-- | Spawn a new thread and try to connect to the given TCP server.
instance HomeHandler Proto.ConnectTCP where
    homeHandler loop _ msg = do
        env <- ask
        liftIO $
            addRemoteProxyConnection @Proto.HomeEnvelope
                (addMsg loop . second ExHomeHandler)
                (T.unpack $ msg ^. Proto.host)
                (T.unpack $ msg ^. Proto.port)
                (env ^. router)

--
instance HomeHandler Proto.SystemData where
    homeHandler _ _ msg = do
        env <- ask
        liftIO . forM_ (filter (/= Home) islands) $ \island ->
            trySendMessage (env ^. router) island msg
