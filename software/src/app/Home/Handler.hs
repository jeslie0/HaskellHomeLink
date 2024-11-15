{-# LANGUAGE OverloadedStrings #-}
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
import Control.Concurrent (
    putMVar,
    takeMVar,
 )
import Control.Monad (void)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import EventLoop (EventLoop)
import Home.AudioStream (start, stop)
import Home.Env (EnvT, audioStream, connectionMVar)
import Lens.Micro
import Proto.Home qualified as Home
import Proto.Home_Fields qualified as Home
import TH (makeInstance, makeToEnvelopeInstances)
import Threads (spawnAsyncComputationWithNotify)

class HomeHandler msg where
    homeHandler ::
        EventLoop EnvT Home.Envelope -> msg -> EnvT ()

-- data ExHomeHandler = forall a. (HomeHandler a) => ExHomeHandler a

-- instance HomeHandler (ExHomeHandler msg) where
--     homeHandler loop (ExHomeHandler msg) = homeHandler loop msg

-- * Message instances

instance HomeHandler Home.StartRadio where
    homeHandler _ _ = do
        env <- ask
        liftIO . start $ env ^. audioStream

instance HomeHandler Home.StopRadio where
    homeHandler _ _ = do
        env <- ask
        liftIO . stop $ env ^. audioStream

instance HomeHandler Home.ConnectTCP where
    homeHandler loop msg = do
        env <- ask
        asyncConnection <-
            liftIO $
                spawnAsyncComputationWithNotify
                    ( mkConnection
                        loop
                        (T.unpack $ msg ^. Home.host)
                        (T.unpack $ msg ^. Home.port)
                    )
                    (void . takeMVar $ env ^. connectionMVar)
        liftIO $ putMVar (env ^. connectionMVar) asyncConnection

$( makeInstance
    ''HomeHandler
    ''Home.Envelope
    'Home.maybe'payload
    ''Home.Envelope'Payload
 )

--
class ToEnvelope msg where
    toEnvelope :: msg -> Home.Envelope

$( makeToEnvelopeInstances
    ''ToEnvelope
    ''Home.Envelope
    ''Home.Envelope'Payload
    'Home.maybe'payload
 )
