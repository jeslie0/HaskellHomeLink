module Chart where

import Prelude

import Apexcharts (Apexchart, Apexoptions, updateOptions)
import Apexcharts.Chart as C
import Apexcharts.Chart.Animations as A
import Apexcharts.Chart.Toolbar as TB
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.DataLabels as DL
import Apexcharts.Series as SE
import Apexcharts.Stroke (Curve(..))
import Apexcharts.Stroke as S
import Apexcharts.Tooltip as TT
import Apexcharts.Xaxis (AxisType(..), type', xaxis) as X
import Apexcharts.Xaxis.Title as XT
import Apexcharts.Yaxis as Y
import Apexcharts.Yaxis.Labels as YL
import Apexcharts.Yaxis.Title as YT
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Console as Console
import System (Island(..))

-- Apex charts work by finding DOM elements with a given label, then
-- modifying them appropriately. For us, this means that our DOM will
-- just consist of empty nodes specified by DEKU. Our Api handlers
-- will be in charge of updating the DOM when required.

-- We use the "createChart" function to make a new chart element,
-- which we can store in the API. Then, we use "updateOptions" to
-- modify the chart.

type ChartID = String

islandChartName :: Island -> ChartID
islandChartName = case _ of
  Home -> "home-memory-chart"
  LocalHTTP -> "local-http-memory-chart"
  RemoteProxy -> "remote-proxy-memory-chart"
  UnknownIsland -> "unknown-memory-chart"

-- createIslandChart :: Island -> Array UInt -> Array UInt -> Effect Apexchart
-- createIslandChart island xData yData = do
--   Console.logShow $ islandChartName island
--   createChart ("#" <> islandChartName island) options
--   where
--   options =
--     ( C.chart :=
--         ( C.type' := CC.Area
--             <> C.height := 350.0
--             <> C.width := "100%"
--             <> Z.zoom := (Z.enabled := false)
--             <> TB.toolbar := (TB.show := false)
--         )
--         <> T.title := T.text := (show island <> " Memory")
--         <> TT.tooltip := (TT.enabled := false)
--         <> S.stroke := S.curve := Smooth
--         <> DL.dataLabels := (DL.enabled := false)
--         <> (updatedChartOptions xData yData)
--         <> X.xaxis :=
--           ( XT.title := (XT.text := "Time")
--               <> X.type' := X.Datetime
--           )
--         <> Y.yaxis :=
--           ( Y.min := 0.0
--               <> Y.max := 1.05 * 16.0
--               <> YT.title := (YT.text := "Memory used (GB)")
--           )
--     )

updatedChartOptions :: Array (Array Number) -> Options Apexoptions
updatedChartOptions xyData = do
  ( C.chart :=
      ( C.type' := CC.Area
          <> C.height := 350.0
          <> C.width := "100%"
          <> Z.zoom := Z.enabled := false
          <> TB.toolbar := TB.show := false
          <> A.animations := A.enabled := false
      )
      <> TT.tooltip := (TT.enabled := false)
      <> S.stroke := S.curve := Smooth
      <> DL.dataLabels := (DL.enabled := false)
      <>
        ( SE.series := [ SE.name := "Memory used (GB)" <> SE.data' := xyData ]
        )

      <> X.xaxis :=
        ( XT.title := (XT.text := "Time")
            <> X.type' := X.Datetime
        )
      <> Y.yaxis :=
        ( Y.min := 0.0
            <> Y.max := 1.05 * getMaxVal xyData
            <> YT.title := (YT.text := "Memory used (GB)")
            <> YL.labels :=
              ( YL.formatter := \strF -> case Number.fromString strF of
                  Just f -> show <<< Number.round $ f / 1000000.0
                  Nothing -> strF
              )
        )
  )

getMaxVal :: Array (Array Number) -> Number
getMaxVal pairs =
  Array.foldl (\acc pair -> max acc $ fromMaybe acc (pair Array.!! 1)) 0.0 pairs

updateChartData :: Apexchart -> Array (Array Number) -> Effect Unit
updateChartData chart xyData = do
  Console.log $ "Updating Chart options: " <> show xyData
  updateOptions (updatedChartOptions xyData) chart
