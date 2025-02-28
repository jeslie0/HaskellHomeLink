module Api (Api(..), mkApi) where

import Prelude

import Apexcharts (Apexchart)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Deku.Effect as DE
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Poll (Poll)
import Logs (Log)
import Poller (Poller)
import Radio (Stream(..), StreamStatus(..))
import Requests (mkLogsPoller, mkMemoryDataPoller, mkStreamStatusPoller, mkSystemsDataPoller, modifyStream)
import System (Island, IslandsSystemData(..))

type Api =
  { polls ::
      { systemsDataPoll :: Poll IslandsSystemData
      , streamStatusPoll :: Poll StreamStatus
      , selectedStreamPoll :: Poll Stream
      }
  , requests ::
      { modifyStream :: Maybe Stream -> Effect Unit
      }
  , setters ::
      { selectStream :: Stream -> Effect Unit
      }
  , memoryCharts ::
      { apexchartsRef :: Ref.Ref (Map.Map Island Apexchart)
      }
  , logging ::
      { logsPoll :: Poll (Array Log)
      , setLogsPoll :: Array Log -> Effect Unit
      }
  , pollers ::
      { streamStatusPoller :: Poller
      , memoryDataPoller :: Poller
      , systemsDataPoller :: Poller
      , logsPoller :: Poller
      }
  }

mkApi :: Effect Api
mkApi = do
  -- Polls
  _ /\ setSystemsDataPoll /\ systemsDataPoll <- DE.useHot $ IslandsSystemData { allSystemData: [] }
  _ /\ setStreamStatusPoll /\ streamStatusPoll <- DE.useHot Off

  _ /\ setSelectedStreamPoll /\ selectedStreamPoll <- DE.useHot ClassicFM
  selectedStreamRef <- Ref.new ClassicFM
  let selectStream stream = Ref.write stream selectedStreamRef >>= \_ -> setSelectedStreamPoll stream

  -- Chart ref and poll
  apexchartsRef <- Ref.new Map.empty

  -- Log Polls
  _ /\ setLogsPoll /\ logsPoll <- DE.useHot []

  -- StateId Refs
  streamStateIdRef <- Ref.new 0

  -- Pollers
  streamStatusPoller <- mkStreamStatusPoller streamStateIdRef selectStream setStreamStatusPoll
  memoryDataPoller <- mkMemoryDataPoller apexchartsRef
  systemsDataPoller <- mkSystemsDataPoller setSystemsDataPoll
  logsPoller <- mkLogsPoller setLogsPoll

  streamStatusPoller.start
  memoryDataPoller.start
  systemsDataPoller.start   
  logsPoller.start
  
  pure
    { polls: { systemsDataPoll, streamStatusPoll, selectedStreamPoll }
    , requests: { modifyStream: modifyStream streamStateIdRef streamStatusPoller }
    , setters: { selectStream }
    , memoryCharts: { apexchartsRef }
    , logging: { logsPoll, setLogsPoll }
    , pollers: { streamStatusPoller, memoryDataPoller, systemsDataPoller, logsPoller }
    }
