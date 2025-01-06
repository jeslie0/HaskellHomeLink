module Api (Api(..), mkApi) where

import Prelude

import Apexcharts (Apexchart)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Deku.Effect as DE
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import FRP.Poll (Poll)
import Radio (Stream(..), StreamStatus(..))
import Requests (fetchMemoryData, fetchStreamStatus, fetchSystemsData, modifyStream)
import System (Island, IslandsSystemData(..))

type Api =
  { polls ::
      { systemsDataPoll :: Poll IslandsSystemData
      , streamStatusPoll :: Poll StreamStatus
      , selectedStreamPoll :: Poll Stream
      }
  , requests :: { modifyStream :: Maybe Stream -> Effect Unit }
  , setters :: { selectStream :: Stream -> Effect Unit }
  , memoryCharts ::
      { apexchartsRef :: Ref.Ref (Array (Tuple Island Apexchart))
      , existingApexCharts :: Poll (Array Island)
      , setExistingApexCharts :: Array Island -> Effect Unit
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
  _ /\ setExistingApexCharts /\ existingApexCharts <- DE.useHot []
  apexchartsRef <- Ref.new []

  -- StateId Refs
  streamStateIdRef <- Ref.new 0

  -- Pollers
  fetchStreamStatus streamStateIdRef selectStream setStreamStatusPoll
  _ <- setInterval 2000 $ fetchStreamStatus streamStateIdRef selectStream setStreamStatusPoll

  fetchMemoryData setExistingApexCharts apexchartsRef
  _ <- setInterval 2000 $ fetchMemoryData  setExistingApexCharts apexchartsRef

  fetchSystemsData setSystemsDataPoll

  pure
    { polls: { systemsDataPoll, streamStatusPoll, selectedStreamPoll }
    , requests: { modifyStream: modifyStream streamStateIdRef selectStream setStreamStatusPoll }
    , setters: { selectStream: selectStream }
    , memoryCharts: { apexchartsRef, existingApexCharts, setExistingApexCharts }
    }
