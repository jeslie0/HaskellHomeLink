module Api where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Effect as DE
import Effect (Effect)
import FRP.Poll (Poll)
import Pages (initialSystemPageState)
import Pages.System (SystemPageState)

data Api = Api { systemPagePoll :: Poll SystemPageState }

mkApi :: Effect Api
mkApi = do
  _ /\ setSystemPagePoll /\ systemPagePoll <- DE.useHot initialSystemPageState
  pure $ Api { systemPagePoll }
