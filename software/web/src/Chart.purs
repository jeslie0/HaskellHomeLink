module Chart (updatedChartOptions, defaultChartOptions) where

import Prelude

import Apexcharts (Apexoptions)
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
import Apexcharts.Tooltip.X as TTX
import Apexcharts.Xaxis (AxisType(..), type', xaxis) as X
import Apexcharts.Xaxis.Title as XT
import Apexcharts.Yaxis as Y
import Apexcharts.Yaxis.Labels as YL
import Apexcharts.Yaxis.Title as YT
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Options (Options, (:=))
import Partial.Unsafe (unsafePartial)

-- Apex charts work by finding DOM elements with a given label, then
-- modifying them appropriately. For us, this means that our DOM will
-- just consist of empty nodes specified by DEKU. Our Api handlers
-- will be in charge of updating the DOM when required.

-- We use the "createChart" function to make a new chart element,
-- which we can store in the API. Then, we use "updateOptions" to
-- modify the chart.

updatedChartOptions :: Array (Array Number) -> Options Apexoptions
updatedChartOptions xyData = do
  let
    ymax = 1.1 * ST.run do
      maxRef <- STRef.new 0.0
      ST.foreach xyData $ \pair -> do
        void $ STRef.modify (\n -> max (unsafePartial $ Array.unsafeIndex pair 1) n) maxRef
      STRef.read maxRef

  SE.series := [ SE.name := "Memory used (GB)" <> SE.data' := xyData ]
     <> Y.yaxis :=
       ( Y.max := ymax
           <> YT.title := (YT.text := "Memory used (GB)")
           <> YL.labels :=
             ( YL.formatter := \strF -> case Number.fromString strF of
                 Just f -> show $ (Number.round $ f / 1000.0) / 1000.0
                 Nothing -> strF
             )
       )
    

defaultChartOptions :: Number -> Options Apexoptions
defaultChartOptions max =
  ( C.chart :=
      ( C.type' := CC.Area
          <> C.height := "100%"
          <> C.width := "100%"
          <> Z.zoom := (Z.enabled := false)
          <> TB.toolbar := TB.show := false
          <> A.animations := A.enabled := false
      )
      <> TT.tooltip :=
        ( TT.enabled := true
            <> TTX.x :=
              ( TTX.format := "HH:mm:ss dd-MMM-yyyy"
              )
        )
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
                  Just f -> show $ (Number.round $ f / 1000.0) / 1000.0
                  Nothing -> strF
              )
        )
  )
