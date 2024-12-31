module Api (Api(..), mkApi) where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Effect as DE
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import FRP.Poll (Poll)
import Requests (fetchStreamStatus, modifyStream)
import System (IslandsSystemData(..))

type Api =
  { polls ::
      { systemsDataPoll :: Poll IslandsSystemData
      , streamActivePoll :: Poll Boolean
      }
  , requests :: { modifyStream :: Boolean -> Effect Unit }
  }

mkApi :: Effect Api
mkApi = do
  -- Polls
  _ /\ setSystemsDataPoll /\ systemsDataPoll <- DE.useHot $ IslandsSystemData { allSystemData: [] }
  _ /\ setStreamActivePoll /\ streamActivePoll <- DE.useHot false

  -- StateId Refs
  streamStateIdRef <- Ref.new 0

  -- Pollers
  fetchStreamStatus setStreamActivePoll (\n -> Ref.write n streamStateIdRef)
  _ <- setInterval 2000 $ fetchStreamStatus setStreamActivePoll (\n -> Ref.write n streamStateIdRef)

  pure
    { polls: { systemsDataPoll, streamActivePoll }
    , requests: { modifyStream: modifyStream streamStateIdRef setStreamActivePoll }
    }
