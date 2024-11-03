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

import Control.Monad.Reader
import Data.ProtoLens (defMessage)
import Home.AudioStream (start, stop)
import Home.Env (Env (..))
import Lens.Micro
import Proto.Radio qualified as Radio
import Proto.Radio_Fields qualified as Radio
import TH

class HomeHandler env a where
    homeHandler :: a -> ReaderT env IO ()

data ExHomeHandler env = forall a. (HomeHandler env a) => ExHomeHandler a

instance HomeHandler env (ExHomeHandler env) where
    homeHandler (ExHomeHandler a) = homeHandler a

-- * Message instances

instance HomeHandler Env Radio.StartRadio where
    homeHandler _ = do
        Env {_audioStream} <- ask
        liftIO $ start _audioStream

instance HomeHandler Env Radio.StopRadio where
    homeHandler _ = do
        Env {_audioStream} <- ask
        liftIO $ stop _audioStream

$( makeInstance
    ''HomeHandler
    ''Env
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
