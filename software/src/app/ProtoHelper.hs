{-# LANGUAGE FunctionalDependencies #-}

module ProtoHelper (
    FromMessage (..),
    ToMessage (..),
    protoRadioStatusResponseToStreamStatus,
    streamStatusToprotoRadioStatusResponse,
) where

import ConnectionManager (Island (..))
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Home.AudioStream (StreamStatus (..))
import Lens.Micro ((&), (.~), (^.))
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import State (StateId)
import System (SystemData, systemDataToMessage)

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

class FromMessage msg type_ err | type_ -> err where
    fromMessage :: msg -> Either err type_

class ToMessage msg type_ where
    toMessage :: type_ -> msg

instance FromMessage Proto.ISLAND Island Int where
    fromMessage Proto.HOME = Right Home
    fromMessage Proto.LOCAL_HTTP = Right LocalHTTP
    fromMessage Proto.REMOTE_PROXY = Right RemoteProxy
    fromMessage _ = Left 0

instance ToMessage Proto.ISLAND Island where
    toMessage Home = Proto.HOME
    toMessage LocalHTTP = Proto.LOCAL_HTTP
    toMessage RemoteProxy = Proto.REMOTE_PROXY

instance ToMessage Proto.IslandsSystemData (Map.Map Island SystemData) where
    toMessage mp =
        defMessage
            & Proto.allSystemData
            .~ ( Map.toList mp
                    & fmap
                        ( \(island, sysData) ->
                            defMessage
                                & Proto.island
                                .~ toMessage island
                                & Proto.systemData
                                .~ systemDataToMessage sysData
                        )
               )
