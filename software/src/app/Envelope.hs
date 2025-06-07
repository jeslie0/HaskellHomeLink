{-# LANGUAGE TemplateHaskell #-}

module Envelope (ToEnvelope (..), ToProxyEnvelope (..)) where

import Data.ProtoLens (defMessage)
import Lens.Micro ((?~))
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import TH (makeToEnvelopeInstances)

class ToEnvelope msg where
  toEnvelope :: msg -> Proto.HomeEnvelope

$( makeToEnvelopeInstances
    ''ToEnvelope
    ''Proto.HomeEnvelope
    ''Proto.HomeEnvelope'Payload
    'Proto.maybe'payload
 )

class ToProxyEnvelope msg where
  toProxyEnvelope :: msg -> Proto.ProxyEnvelope

$( makeToEnvelopeInstances
    ''ToProxyEnvelope
    ''Proto.ProxyEnvelope
    ''Proto.ProxyEnvelope'Payload
    'Proto.maybe'payload
 )
