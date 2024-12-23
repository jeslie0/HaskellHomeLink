module ProtoHelper (protoRadioStatusResponseToStreamStatus, streamStatusToprotoRadioStatusResponse) where

import Data.ProtoLens (defMessage)
import Home.AudioStream (StreamStatus (..))
import Lens.Micro ((&), (.~), (^.))
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import State (StateId)

protoRadioStatusResponseToStreamStatus ::
    Proto.GetRadioStatusResponse -> StreamStatus
protoRadioStatusResponseToStreamStatus resp =
    if resp ^. Proto.radioOn then Active else Inactive

streamStatusToprotoRadioStatusResponse ::
    StateId -> StreamStatus -> Proto.GetRadioStatusResponse
streamStatusToprotoRadioStatusResponse statusId status =
    defMessage
        & Proto.radioOn
        .~ ( case status of
                Active -> True
                Inactive -> False
           )
        & (Proto.stateId .~ statusId)
