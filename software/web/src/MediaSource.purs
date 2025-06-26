module MediaSource where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console as Console
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (addEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML.HTMLMediaElement (play, setSrc)
import Web.HTML.HTMLVideoElement (HTMLVideoElement, toHTMLMediaElement)
import Web.Socket.WebSocket as WS

foreign import data MediaSource :: Type
foreign import data SourceBuffer :: Type

foreign import createMediaSource :: Effect MediaSource
foreign import isTypeSupported :: String -> Boolean
foreign import addSourceBuffer :: MediaSource -> String -> Effect SourceBuffer
foreign import urlCreateObjectURL :: MediaSource -> Effect String
foreign import appendBuffer :: SourceBuffer -> ArrayBuffer -> Effect Unit

toEventTarget :: MediaSource -> EventTarget
toEventTarget = unsafeCoerce

foreign import playVideo :: WS.WebSocket -> HTMLVideoElement -> Effect Unit
