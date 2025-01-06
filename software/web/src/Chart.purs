module Chart where

import Prelude

import Apexcharts (Apexchart, Apexoptions, createChart, render)
import Apexcharts.Chart as C
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.DataLabels as DL
import Apexcharts.Series as SE
import Apexcharts.Stroke (Curve(..))
import Apexcharts.Stroke as S
import Apexcharts.Title as T
import Apexcharts.Xaxis as X
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Deku.Control (elementify)
import Deku.Core (Nut(..))
import Deku.DOM (Attribute, HTMLDivElement)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Effect (Effect)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

-- Apex charts work by finding DOM elements with a given label, then
-- modifying them appropriately. For us, this means that our DOM will
-- just consist of empty nodes specified by DEKU. Our Api handlers
-- will be in charge of updating the DOM when required.

-- We use the "createChart" function to make a new chart element,
-- which we can store in the API. Then, we use "updateOptions" to
-- modify the chart.

type ChartID = String

example :: ChartID -> Effect Apexchart
example chartId = createChart ("#" <> chartId)
  ( C.chart := (C.type' := CC.Area
                <> C.height := 350.0
                <> C.width := "400.0"
                <> Z.zoom := (Z.enabled := false))
      <> SE.series := [ SE.name := "STOCK ABC" <> SE.data' := [ 31, 40, 28, 51, 42, 109, 100 ] ]
      <> T.title := T.text := "Hi"
      <> S.stroke := S.curve := Smooth
      <> DL.dataLabels := (DL.enabled := false)
      <> X.xaxis :=
        ( X.type' := X.Datetime <> X.categories :=
            [ "2018-09-19T00:00:00.000Z"
            , "2018-09-19T01:30:00.000Z"
            , "2018-09-19T02:30:00.000Z"
            , "2018-09-19T03:30:00.000Z"
            , "2018-09-19T04:30:00.000Z"
            , "2018-09-19T05:30:00.000Z"
            , "2018-09-19T06:30:00.000Z"
            ]
        )

  )

newOptions :: Options Apexoptions
newOptions =
  ( SE.series := [ SE.name := "STOCK ABC" <> SE.data' := [ 31, 40, 28, 51, 42, 109, 100 ] ]
  )
