module Api (Api(..), mkApi) where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Effect as DE
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import FRP.Poll (Poll)
import Radio (Stream(..))
import Requests (fetchStreamStatus, fetchSystemsData, modifyStream)
import System (IslandsSystemData(..))

type Api =
  { polls ::
      { systemsDataPoll :: Poll IslandsSystemData
      , streamActivePoll :: Poll Boolean
      , selectedStreamPoll :: Poll Stream
      }
  , requests :: { modifyStream :: Boolean -> Effect Unit }
  , setters :: { selectStream :: Stream -> Effect Unit }
  }

mkApi :: Effect Api
mkApi = do
  -- Polls
  _ /\ setSystemsDataPoll /\ systemsDataPoll <- DE.useHot $ IslandsSystemData { allSystemData: [] }
  _ /\ setStreamActivePoll /\ streamActivePoll <- DE.useHot false

  _ /\ setSelectedStreamPoll /\ selectedStreamPoll <- DE.useHot ClassicFM
  selectedStreamRef <- Ref.new ClassicFM
  let selectStream stream = Ref.write stream selectedStreamRef >>= \_ -> setSelectedStreamPoll stream

  -- StateId Refs
  streamStateIdRef <- Ref.new 0

  -- Pollers
  fetchStreamStatus setStreamActivePoll (\n -> Ref.write n streamStateIdRef) selectStream
  _ <- setInterval 2000 $ fetchStreamStatus setStreamActivePoll (\n -> Ref.write n streamStateIdRef) selectStream

  fetchSystemsData setSystemsDataPoll

  pure
    { polls: { systemsDataPoll, streamActivePoll, selectedStreamPoll }
    , requests: { modifyStream: modifyStream streamStateIdRef selectedStreamRef setStreamActivePoll selectStream }
    , setters: { selectStream: selectStream }
    }
