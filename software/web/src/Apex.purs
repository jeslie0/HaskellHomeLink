module Apex where

import Prelude

import Apexcharts (Apexoptions, createChartEl, render, updateOptions)
import Chart (defaultChartOptions)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Options (Options)
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify)
import Deku.DOM (HTMLDivElement)
import Deku.DOM.Self as Self
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Poll (Poll, listen_)

apexchart
  :: (Poll (Options Apexoptions))
  -> Array (Poll (Attribute (HTMLDivElement ())))
  -> Nut
apexchart optionsPoll attrs =
  elementify Nothing "apexchart" (Array.cons init attrs) []
  where
  init = Self.self_ \e -> launchAff_ do
    liftEffect $ Console.log "SELF"
    delay (Milliseconds 0.0)
    chart <- liftEffect $ createChartEl e (defaultChartOptions 10.0)
    liftEffect $ render chart
    unsub <- liftEffect $ listen_ optionsPoll $ \op -> do
      liftEffect $ Console.log "UPDATES"
      updateOptions op chart
    pure unit

-- apexchart :: Array Nut -> Nut
-- apexchart arr = ?a
