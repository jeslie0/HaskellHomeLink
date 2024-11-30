{-# LANGUAGE TemplateHaskell #-}

module Envelope (ToEnvelope(..), ToProxyEnvelope(..)) where

import Data.ProtoLens (defMessage)
import Lens.Micro ((?~))
import Proto.Home qualified as Home
import Proto.Home_Fields qualified as Home
import Proto.Proxy qualified as Proxy
import Proto.Proxy_Fields qualified as Proxy
import TH (makeToEnvelopeInstances)

class ToEnvelope msg where
    toEnvelope :: msg -> Home.Envelope

$( makeToEnvelopeInstances
    ''ToEnvelope
    ''Home.Envelope
    ''Home.Envelope'Payload
    'Home.maybe'payload
 )

class ToProxyEnvelope msg where
    toProxyEnvelope :: msg -> Proxy.ProxyRecieveEnvelope

$( makeToEnvelopeInstances
    ''ToProxyEnvelope
    ''Proxy.ProxyRecieveEnvelope
    ''Proxy.ProxyRecieveEnvelope'Payload
    'Proxy.maybe'payload
 )
