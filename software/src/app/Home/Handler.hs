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

import Connection (mkConnection)
import Control.Concurrent (
    putMVar,
    takeMVar,
    tryTakeMVar,
 )
import Control.Monad (void)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import EventLoop (EventLoop, addMsg)
import Home.AudioStream (mkAsyncAudioStream, start)
import Home.Env (EnvT, audioStreamMVar, connectionMVar)
import Lens.Micro
import Msg (Msg (..))
import Proto.Home qualified as Home
import Proto.Home_Fields qualified as Home
import TH (makeInstance, makeToEnvelopeInstances)
import Threads (killAsyncComputation, spawnAsyncComputationWithNotify)

class HomeHandler msg where
    homeHandler ::
        EventLoop EnvT ExHomeHandler -> msg -> EnvT ()

data ExHomeHandler = forall a. (HomeHandler a) => ExHomeHandler a

instance HomeHandler ExHomeHandler where
    homeHandler loop (ExHomeHandler msg) = homeHandler loop msg

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
    homeHandler _ _ = do
        env <- ask
        mAudioStream <- liftIO $ tryTakeMVar (env ^. audioStreamMVar)
        case mAudioStream of
            Just _ -> liftIO $ putStrLn "Audio stream already exists"
            Nothing -> do
                asyncAudioStream <- liftIO mkAsyncAudioStream
                asyncComputation <-
                    liftIO $
                        spawnAsyncComputationWithNotify
                            (start asyncAudioStream)
                            (void . tryTakeMVar $ env ^. audioStreamMVar)
                liftIO $ putMVar (env ^. audioStreamMVar) asyncComputation

-- | Stop playing an audio stream if one is playing.
instance HomeHandler Home.StopRadio where
    homeHandler _ _ = do
        env <- ask
        mAudioStream <- liftIO $ tryTakeMVar (env ^. audioStreamMVar)
        case mAudioStream of
            Just audioStream -> liftIO . killAsyncComputation $ audioStream
            Nothing -> liftIO $ putStrLn "Radio not playing"

-- | Spawn a new thread and try to connect to the given TCP server.
instance HomeHandler Home.ConnectTCP where
    homeHandler loop msg = do
        env <- ask
        asyncConnection <-
            liftIO $
                spawnAsyncComputationWithNotify
                    ( mkConnection
                        (T.unpack $ msg ^. Home.host)
                        (T.unpack $ msg ^. Home.port)
                        withBytes
                    )
                    (void . takeMVar $ env ^. connectionMVar)
        liftIO $ putMVar (env ^. connectionMVar) asyncConnection
      where
        -- | Receive a Home.Envelope and add, parse and add to the
        -- event loop.
        withBytes bytes =
            case fromBytes @Home.Envelope bytes of
                Left err -> putStrLn err
                Right envelope -> addMsg loop $ ExHomeHandler envelope

--
class ToEnvelope msg where
    toEnvelope :: msg -> Home.Envelope

$( makeToEnvelopeInstances
    ''ToEnvelope
    ''Home.Envelope
    ''Home.Envelope'Payload
    'Home.maybe'payload
 )
