module ProtoHelper (protoRadioStatusResponseToStreamStatus, streamStatusToprotoRadioStatusResponse) where

import Data.ProtoLens (defMessage)
import Home.AudioStream (StreamStatus (..))
import Lens.Micro ((&), (.~), (^.))
import Proto.Proxy qualified as Proxy
import Proto.Proxy_Fields qualified as Proxy
import State (StateId)

protoRadioStatusResponseToStreamStatus ::
    Proxy.GetRadioStatusResponse -> StreamStatus
protoRadioStatusResponseToStreamStatus resp =
    if resp ^. Proxy.radioOn then Active else Inactive

streamStatusToprotoRadioStatusResponse ::
    StateId -> StreamStatus -> Proxy.GetRadioStatusResponse
streamStatusToprotoRadioStatusResponse statusId status =
    defMessage
        & Proxy.radioOn
        .~ ( case status of
                Active -> True
                Inactive -> False
           )
        & (Proxy.stateId .~ statusId)
