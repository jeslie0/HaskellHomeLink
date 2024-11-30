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
    ToEnvelope (..),
) where

import ConnectionManager (Island (..))
import Control.Concurrent (
    modifyMVar_,
 )
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import EventLoop (EventLoop, addMsg)
import Home.AudioStream (mkAsyncAudioStream, start)
import Home.Env (EnvT, addRemoteProxyConnection, audioStreamMVar, router)
import Lens.Micro
import Proto.Home qualified as Home
import Proto.Home_Fields qualified as Home
import TH (makeInstance, makeToEnvelopeInstances)
import Threads (
    killAsyncComputation,
    spawnAsyncComputation,
 )

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
    ''Home.Envelope
    'Home.maybe'payload
    ''Home.Envelope'Payload
 )

{- | Try to make a new asynchronous audio stream in a separate
thread. If one exists, report the error.
-}
instance HomeHandler Home.StartRadio where
    homeHandler _ _ _ = do
        liftIO $ putStrLn "START"
        env <- ask
        liftIO $ modifyMVar_ (env ^. audioStreamMVar) $ \case
            Just stream -> do
                putStrLn "Audio stream already exists"
                pure $ Just stream
            Nothing -> do
                asyncAudioStream <- mkAsyncAudioStream
                Just <$> spawnAsyncComputation (start asyncAudioStream)

-- | Stop playing an audio stream if one is playing.
instance HomeHandler Home.StopRadio where
    homeHandler _ _ _ = do
        env <- ask
        liftIO $ modifyMVar_ (env ^. audioStreamMVar) $ \case
            Nothing -> do
                putStrLn "Radio not playing"
                pure Nothing
            Just asyncStream -> do
                killAsyncComputation asyncStream
                pure Nothing

-- | Spawn a new thread and try to connect to the given TCP server.
instance HomeHandler Home.ConnectTCP where
    homeHandler loop _ msg = do
        env <- ask
        liftIO $
            addRemoteProxyConnection @Home.Envelope
                (addMsg loop . second ExHomeHandler)
                (T.unpack $ msg ^. Home.host)
                (T.unpack $ msg ^. Home.port)
                (env ^. router)

--
class ToEnvelope msg where
    toEnvelope :: msg -> Home.Envelope

$( makeToEnvelopeInstances
    ''ToEnvelope
    ''Home.Envelope
    ''Home.Envelope'Payload
    'Home.maybe'payload
 )
