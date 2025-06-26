module Api (Api(..), mkApi) where

import Prelude

import Apex (apexchart)
import Apexcharts (Apexoptions)
import Chart (defaultChartOptions)
import Data.Maybe (Maybe(..))
import Data.Options (Options)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut)
import Deku.DOM.Attributes as DA
import Deku.Effect as DE
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Poll (Poll)
import Logs (Log)
import Radio (Stream(..), StreamStatus(..))
import Requests (AppPollers, mkLogsPoller, mkMemoryChartOptionsPoller, mkStreamStatusPoller, mkSystemsDataPoller, modifyStream)
import System (DeviceData)
import Web.Socket.WebSocket as WS

type Api =
  { islandState ::
      { home ::
          { systemData :: Poll (Maybe DeviceData)
          , memoryChartOptions :: Poll (Options Apexoptions)
          , chart :: Nut
          }
      , proxy ::
          { systemData :: Poll (Maybe DeviceData)
          , memoryChartOptions :: Poll (Options Apexoptions)
          , chart :: Nut
          }
      , camera ::
          { systemData :: Poll (Maybe DeviceData)
          , memoryChartOptions :: Poll (Options Apexoptions)
          , chart :: Nut
          }
      }
  , charters ::
      { home ::
          { subscribe :: Poll (Options Apexoptions) -> Effect Unit
          , unsubscribe :: Effect Unit
          }
      , proxy ::
          { subscribe :: Poll (Options Apexoptions) -> Effect Unit
          , unsubscribe :: Effect Unit
          }
      , camera ::
          { subscribe :: Poll (Options Apexoptions) -> Effect Unit
          , unsubscribe :: Effect Unit
          }
      }
  , polls ::
      { streamStatusPoll :: Poll StreamStatus
      , selectedStreamPoll :: Poll Stream
      }
  , requests ::
      { modifyStream :: Maybe Stream -> Effect Unit
      }
  , setters ::
      { selectStream :: Stream -> Effect Unit
      }
  , logging ::
      { logsPoll :: Poll (Array Log)
      , setLogsPoll :: Array Log -> Effect Unit
      }
  , pollers :: AppPollers
  , websocket :: Ref.Ref (Maybe WS.WebSocket)
  }

mkApi :: Effect Api
mkApi = do
  -- Systems Polls
  setHomeDeviceDataPoll /\ homeDeviceDataPoll <- DE.useState Nothing
  _ /\ setHomeMemoryChartOptionsPoll /\ homeMemoryChartOptionsPoll <- DE.useHot (defaultChartOptions 10.0)

  setProxyDeviceDataPoll /\ proxyDeviceDataPoll <- DE.useState Nothing
  _ /\ setProxyMemoryChartOptionsPoll /\ proxyMemoryChartOptionsPoll <- DE.useHot (defaultChartOptions 10.0)

  setCameraDeviceDataPoll /\ cameraDeviceDataPoll <- DE.useState Nothing
  _ /\ setCameraMemoryChartOptionsPoll /\ cameraMemoryChartOptionsPoll <- DE.useHot (defaultChartOptions 10.0)

  homeChartStuff <- apexchart [ DA.style_ "display: block;" ]
  proxyChartStuff <- apexchart [ DA.style_ "display: block;" ]
  cameraChartStuff <- apexchart [ DA.style_ "display: block;" ]

  -- Stream Polls
  _ /\ setStreamStatusPoll /\ streamStatusPoll <- DE.useHot Off
  _ /\ setSelectedStreamPoll /\ selectedStreamPoll <- DE.useHot ClassicFM
  selectedStreamRef <- Ref.new ClassicFM
  let selectStream stream = Ref.write stream selectedStreamRef >>= \_ -> setSelectedStreamPoll stream

  -- Log Polls
  setLogsPoll /\ logsPoll <- DE.useState []

  -- StateId Refs
  streamStateIdRef <- Ref.new 0

  -- Pollers
  streamStatusPoller <- mkStreamStatusPoller streamStateIdRef selectStream setStreamStatusPoll
  systemsDataPoller <- mkSystemsDataPoller { setHomeDeviceDataPoll, setProxyDeviceDataPoll, setCameraDeviceDataPoll }
  memoryDataPoller <- mkMemoryChartOptionsPoller { setHomeMemoryChartOptionsPoll, setProxyMemoryChartOptionsPoll, setCameraMemoryChartOptionsPoll }
  logsPoller <- mkLogsPoller setLogsPoll

  websocket <- Ref.new Nothing

  streamStatusPoller.start

  pure
    { islandState:
        { home:
            { systemData: homeDeviceDataPoll
            , memoryChartOptions: homeMemoryChartOptionsPoll
            , chart: homeChartStuff.chart
            }
        , proxy:
            { systemData: proxyDeviceDataPoll
            , memoryChartOptions: proxyMemoryChartOptionsPoll
            , chart: proxyChartStuff.chart
            }
        , camera:
            { systemData: cameraDeviceDataPoll
            , memoryChartOptions: cameraMemoryChartOptionsPoll
            , chart: cameraChartStuff.chart
            }
        }
    , polls: { streamStatusPoll, selectedStreamPoll }
    , requests: { modifyStream: modifyStream streamStateIdRef streamStatusPoller }
    , setters: { selectStream }
    , logging: { logsPoll, setLogsPoll }
    , pollers:
        { streamStatusPoller
        , memoryDataPoller
        , systemsDataPoller
        , logsPoller
        }
    , charters:
        { home:
            { subscribe: homeChartStuff.subscribe
            , unsubscribe: homeChartStuff.unsubscribe
            }
        , proxy:
            { subscribe: proxyChartStuff.subscribe
            , unsubscribe: proxyChartStuff.unsubscribe
            }
        , camera:
            { subscribe: cameraChartStuff.subscribe
            , unsubscribe: cameraChartStuff.unsubscribe
            }
        }
    , websocket
    }
