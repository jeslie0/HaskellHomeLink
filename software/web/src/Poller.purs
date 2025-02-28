module Poller (Poller(..), mkPoller) where

import Prelude

import Constants (getApiUrl)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Types (DataView)
import Data.Foldable (for_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Fetch (fetch)

type Poller =
  { start :: Effect Unit
  , stop :: Effect Unit
  , force :: Effect Unit
  }

mkPoller :: String -> Int -> (DataView -> Aff Unit) -> Effect (Poller)
mkPoller endpoint timeoutMs handle = do
  apiUrl <- getApiUrl
  timeoutRef <- Ref.new Nothing

  let
    requestUrl = apiUrl <> endpoint

    request = launchAff_ do
      { arrayBuffer } <- fetch requestUrl { method: GET }

      timeout <- liftEffect $ setTimeout timeoutMs request
      liftEffect $ Ref.write (Just timeout) timeoutRef

      body <- whole <$> arrayBuffer
      handle body

    stop = do
      mTimeout <- Ref.read timeoutRef
      for_ mTimeout clearTimeout

    start = do
      request
      timeout <- setTimeout timeoutMs request
      Ref.write (Just timeout) timeoutRef

    force =
      stop *> start

  pure $ { start, stop, force }
