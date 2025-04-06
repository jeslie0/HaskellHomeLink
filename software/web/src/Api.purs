module Api (Api(..), mkApi) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Effect as DE
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Poll (Poll)
import Logs (Log)
import Poller (Poller)
import Radio (Stream(..), StreamStatus(..))
import Requests (mkLogsPoller, mkMemoryDataPoller, mkStreamStatusPoller, mkSystemsDataPoller, modifyStream)
import System (SystemData)

type Api =
  { islandState ::
      { home ::
          { systemData :: Poll (Maybe SystemData)
          , memoryData :: Poll (Array (Array Number))
          }
      , proxy ::
          { systemData :: Poll (Maybe SystemData)
          , memoryData :: Poll (Array (Array Number))
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
  , pollers ::
      { streamStatusPoller :: Poller
      , memoryDataPoller :: Poller
      , systemsDataPoller :: Poller
      , logsPoller :: Poller
      }
  }

mkApi :: Effect Api
mkApi = do
  -- Systems Polls
  setHomeSystemDataPoll /\ homeSystemDataPoll <- DE.useState Nothing
  setHomeMemoryDataPoll /\ homeMemoryDataPoll <- DE.useState []

  setProxySystemDataPoll /\ proxySystemDataPoll <- DE.useState Nothing
  setProxyMemoryDataPoll /\ proxyMemoryDataPoll <- DE.useState []

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
  systemsDataPoller <- mkSystemsDataPoller { setHomeSystemDataPoll, setProxySystemDataPoll }
  memoryDataPoller <- mkMemoryDataPoller { setHomeMemoryDataPoll, setProxyMemoryDataPoll }
  logsPoller <- mkLogsPoller setLogsPoll

  streamStatusPoller.start
  memoryDataPoller.start
  systemsDataPoller.start
  logsPoller.start

  pure
    { islandState:
        { home:
            { systemData: homeSystemDataPoll
            , memoryData: homeMemoryDataPoll
            }
        , proxy:
            { systemData: proxySystemDataPoll
            , memoryData: proxyMemoryDataPoll
            }
        }
    , polls: { streamStatusPoll, selectedStreamPoll }
    , requests: { modifyStream: modifyStream streamStateIdRef streamStatusPoller }
    , setters: { selectStream }
    , logging: { logsPoll, setLogsPoll }
    , pollers: { streamStatusPoller, memoryDataPoller, systemsDataPoller, logsPoller }
    }
