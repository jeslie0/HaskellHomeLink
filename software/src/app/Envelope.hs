{-# LANGUAGE TemplateHaskell #-}

module Envelope (wrapHomeMsg, wrapProxyMsg, wrapCameraMsg) where

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

wrapHomeMsg :: ToEnvelope msg => msg -> Proto.WrappedEnvelope
wrapHomeMsg msg =
  ( Proto.maybe'wrappedPayload
      ?~ Proto.WrappedEnvelope'HomeMsg (toEnvelope msg)
  )
    defMessage

class ToProxyEnvelope msg where
  toProxyEnvelope :: msg -> Proto.ProxyEnvelope

$( makeToEnvelopeInstances
    ''ToProxyEnvelope
    ''Proto.ProxyEnvelope
    ''Proto.ProxyEnvelope'Payload
    'Proto.maybe'payload
 )

wrapProxyMsg :: ToProxyEnvelope msg => msg -> Proto.WrappedEnvelope
wrapProxyMsg msg =
  ( Proto.maybe'wrappedPayload
      ?~ Proto.WrappedEnvelope'ProxyMsg (toProxyEnvelope msg)
  )
    defMessage

class ToCameraEnvelope msg where
  toCameraEnvelope :: msg -> Proto.CameraEnvelope

$( makeToEnvelopeInstances
    ''ToCameraEnvelope
    ''Proto.CameraEnvelope
    ''Proto.CameraEnvelope'Payload
    'Proto.maybe'payload
 )

wrapCameraMsg :: ToCameraEnvelope msg => msg -> Proto.WrappedEnvelope
wrapCameraMsg msg =
  ( Proto.maybe'wrappedPayload
      ?~ Proto.WrappedEnvelope'CameraMsg (toCameraEnvelope msg)
  )
    defMessage
