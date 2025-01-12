module Chart (updateChartData, defaultChartOptions) where

import Prelude

import Apexcharts (Apexchart, Apexoptions, updateOptions)
import Apexcharts.Chart as C
import Apexcharts.Chart.Animations as A
import Apexcharts.Chart.Toolbar as TB
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.DataLabels as DL
import Apexcharts.NoData as ND
import Apexcharts.Series as SE
import Apexcharts.Stroke as S
import Apexcharts.Tooltip as TT
import Apexcharts.Xaxis (AxisType(..), type', xaxis) as X
import Apexcharts.Xaxis.Title as XT
import Apexcharts.Yaxis as Y
import Apexcharts.Yaxis.Labels as YL
import Apexcharts.Yaxis.Title as YT
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Options (Options, (:=))
import Effect (Effect)

-- Apex charts work by finding DOM elements with a given label, then
-- modifying them appropriately. For us, this means that our DOM will
-- just consist of empty nodes specified by DEKU. Our Api handlers
-- will be in charge of updating the DOM when required.

-- We use the "createChart" function to make a new chart element,
-- which we can store in the API. Then, we use "updateOptions" to
-- modify the chart.

updatedChartOptions :: Array (Array Number) -> Options Apexoptions
updatedChartOptions xyData = do
  SE.series := [ SE.data' := xyData ]

updateChartData :: Apexchart -> Array (Array Number) -> Effect Unit
updateChartData chart xyData = do
  updateOptions (updatedChartOptions xyData) chart

defaultChartOptions :: Number -> Options Apexoptions
defaultChartOptions max =
  ( C.chart :=
      ( C.type' := CC.Area
          <> C.height := 350.0
          <> C.width := "100%"
          <> Z.zoom := (Z.enabled := false)
          <> TB.toolbar := TB.show := false
          <> A.animations := A.enabled := false
      )
      <> TT.tooltip := (TT.enabled := false)
      <> DL.dataLabels := (DL.enabled := false)
      <> S.stroke := S.curve := S.Smooth
      <> X.xaxis :=
        ( XT.title := (XT.text := "Time")
            <> X.type' := X.Datetime
        )
      <> SE.series @Number := []
      <> ND.noData := ND.text := "Loading..."
      <> Y.yaxis :=
        ( Y.min := 0.0
            <> Y.max := max
            <> YT.title := (YT.text := "Memory used (GB)")
            <> YL.labels :=
              ( YL.formatter := \strF -> case Number.fromString strF of
                  Just f -> show <<< Number.round $ f / 1000000.0
                  Nothing -> strF
              )
        )
  )
