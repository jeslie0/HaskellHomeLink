module Api where

import Prelude

import FRP.Poll (Poll)
import Pages.System (SystemPageState)

data Api = Api { systemPagePoll :: Poll SystemPageState }

mkApi :: Effect Api
mkApi = do
  _ /\ setSystemPageState /\ systemPageState <- DE.useHot initialSystemPageState
  pure $ Api { systemPagePoll}
