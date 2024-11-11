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

import Connection (Connection (..), mkConnection)
import Control.Concurrent (forkIO, isEmptyMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Lifted (fork)
import Control.Monad (void)
import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import EventLoop (EventLoop (..))
import Home.AudioStream (start, stop)
import Home.Env (Env (..), EnvT)
import Lens.Micro
import Msg (Msg (..))
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio
import TH

class HomeHandler msg where
    homeHandler ::
        EventLoop EnvT Radio.Envelope -> msg -> EnvT ()

data ExHomeHandler env = forall a. (HomeHandler a) => ExHomeHandler a

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
        let (threadMVar, blockMVar) = _connThread env
        threadMVarEmpty <- liftIO $ isEmptyMVar threadMVar
        blockMVarFull <- liftIO $ not <$> isEmptyMVar blockMVar
        if threadMVarEmpty && blockMVarFull
            then do
                threadID <- fork $ do
                    void . liftIO $ mkConnection loop blockMVar "127.0.0.1" "3000"
                liftIO $ putMVar threadMVar threadID
                void . liftIO $ takeMVar blockMVar
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
