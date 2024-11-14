{-# LANGUAGE TemplateHaskell #-}

{-|
Module : Home.Handler
Description : Defines the typeclass and instance for messages being
handled by the Home application.
-}
module Home.Handler (
    HomeHandler (..),
    -- ExHomeHandler (..),
    ToEnvelope (..),
) where

import Connection (mkConnection)
import Control.Concurrent (isEmptyMVar, myThreadId, killThread)
import Control.Exception (SomeException (..), displayException)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (EventLoop)
import Home.AudioStream (start, stop)
import Home.Env (Env (..), EnvT)
import Lens.Micro
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio
import TH
import ThreadPool (addTask, addTaskUnmasked)

class HomeHandler msg where
    homeHandler ::
        EventLoop EnvT Radio.Envelope -> msg -> EnvT ()

-- data ExHomeHandler = forall a. (HomeHandler a) => ExHomeHandler a

-- instance HomeHandler (ExHomeHandler msg) where
--     homeHandler loop (ExHomeHandler msg) = homeHandler loop msg

-- * Message instances

instance HomeHandler Radio.StartRadio where
    homeHandler _ _ = do
        Env {_audioStream} <- ask
        liftIO $ start _audioStream

instance HomeHandler Radio.StopRadio where
    homeHandler _ _ = do
        Env {_audioStream} <- ask
        liftIO $ stop _audioStream

instance HomeHandler Radio.ConnectTCP where
    homeHandler loop _ = do
        env <- ask
        let connectionMVar = _connectionMVar env
            threadPool = _threadPool env
        connectionExists <- liftIO $ isEmptyMVar connectionMVar
        if not connectionExists
            then liftIO
                $ addTaskUnmasked
                    threadPool
                    (mkConnection loop threadPool "127.0.0.1" "3000")
                -- $ \(SomeException err) -> do
                --     thisThread <- myThreadId
                --     putStrLn $ "ZZZZZ" <> displayException err
                --     killThread thisThread
            else do
                liftIO $ putStrLn "Connection thread already exists!"
                pure ()

$( makeInstance
    ''HomeHandler
    ''Radio.Envelope
    'Radio.maybe'payload
    ''Radio.Envelope'Payload
 )

--
class ToEnvelope msg where
    toEnvelope :: msg -> Radio.Envelope

$( makeToEnvelopeInstances
    ''ToEnvelope
    ''Radio.Envelope
    ''Radio.Envelope'Payload
    'Radio.maybe'payload
 )
