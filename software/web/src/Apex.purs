module Apex (apexchart) where

import Prelude

import Apexcharts (Apexchart, Apexoptions, createChartEl, render, updateOptions)
import Chart (defaultChartOptions)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Options (Options)
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify)
import Deku.DOM (HTMLDivElement)
import Deku.DOM.Self as Self
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Poll (Poll, listen_)

type ApexChartDeku =
  { chart :: Nut
  , subscribe :: Poll (Options Apexoptions) -> Effect Unit
  , unsubscribe :: Effect Unit
  }

apexchart :: Array (Poll (Attribute (HTMLDivElement ()))) -> Effect ApexChartDeku
apexchart attrs = do
  chartRef <- Ref.new (Nothing :: Maybe Apexchart)
  chartUnsubRef <- Ref.new (pure unit :: Effect Unit)

  let
    nut = elementify Nothing "apexchart" (Array.cons (init chartRef) attrs) []
    subscribe options = do
      unsub <- listen_ options
        ( \op -> do
            mchart <- Ref.read chartRef
            case mchart of
              Nothing -> pure unit
              Just chart -> updateOptions op chart
        )
      Ref.write (unsub) chartUnsubRef

    unsubscribe = do
      Ref.write Nothing chartRef
      unsub <- Ref.read chartUnsubRef
      unsub

  pure $ { chart: nut, subscribe, unsubscribe }

  where
  init chartRef = Self.self_ \el -> launchAff_ do
    delay (Milliseconds 0.0)
    liftEffect do
      chart <- createChartEl el (defaultChartOptions 10.0)
      render chart
      Ref.write (Just chart) chartRef
