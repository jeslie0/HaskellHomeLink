-- | Generated by __protobuf__ from file `proto/proxy.proto`
module Proto.Proxy
( ModifyRadioRequest(..), ModifyRadioRequestRow, ModifyRadioRequestR, parseModifyRadioRequest, putModifyRadioRequest, defaultModifyRadioRequest, mkModifyRadioRequest, mergeModifyRadioRequest
, ModifyRadioResponse(..), ModifyRadioResponseRow, ModifyRadioResponseR, parseModifyRadioResponse, putModifyRadioResponse, defaultModifyRadioResponse, mkModifyRadioResponse, mergeModifyRadioResponse
, GetRadioStatusRequest(..), GetRadioStatusRequestRow, GetRadioStatusRequestR, parseGetRadioStatusRequest, putGetRadioStatusRequest, defaultGetRadioStatusRequest, mkGetRadioStatusRequest, mergeGetRadioStatusRequest
, GetRadioStatusResponse(..), GetRadioStatusResponseRow, GetRadioStatusResponseR, parseGetRadioStatusResponse, putGetRadioStatusResponse, defaultGetRadioStatusResponse, mkGetRadioStatusResponse, mergeGetRadioStatusResponse
, ProxyRecieveEnvelope(..), ProxyRecieveEnvelopeRow, ProxyRecieveEnvelopeR, parseProxyRecieveEnvelope, putProxyRecieveEnvelope, defaultProxyRecieveEnvelope, mkProxyRecieveEnvelope, mergeProxyRecieveEnvelope, ProxyRecieveEnvelope_Payload(..)
)
where
import Protobuf.Internal.Prelude
import Protobuf.Internal.Prelude as Prelude




-- | Message generated by __protobuf__ from `ModifyRadioRequest`
-- | 
-- | Received from HTTP Server.
newtype ModifyRadioRequest = ModifyRadioRequest ModifyRadioRequestR
type ModifyRadioRequestRow =
  ( start :: Prelude.Maybe Boolean
  , __unknown_fields :: Array Prelude.UnknownField
  )
type ModifyRadioRequestR = Record ModifyRadioRequestRow
derive instance genericModifyRadioRequest :: Prelude.Generic ModifyRadioRequest _
derive instance newtypeModifyRadioRequest :: Prelude.Newtype ModifyRadioRequest _
derive instance eqModifyRadioRequest :: Prelude.Eq ModifyRadioRequest
instance showModifyRadioRequest :: Prelude.Show ModifyRadioRequest where show x = Prelude.genericShow x

putModifyRadioRequest :: forall m. Prelude.MonadEffect m => ModifyRadioRequest -> Prelude.PutM m Prelude.Unit
putModifyRadioRequest (ModifyRadioRequest r) = do
  Prelude.putOptional 1 r.start Prelude.isDefault Prelude.encodeBoolField
  Prelude.traverse_ Prelude.putFieldUnknown r.__unknown_fields

parseModifyRadioRequest :: forall m. Prelude.MonadEffect m => Prelude.MonadRec m => Prelude.ByteLength -> Prelude.ParserT Prelude.DataView m ModifyRadioRequest
parseModifyRadioRequest length = Prelude.label "ModifyRadioRequest / " $
  Prelude.parseMessage ModifyRadioRequest defaultModifyRadioRequest parseField length
 where
  parseField
    :: Prelude.FieldNumberInt
    -> Prelude.WireType
    -> Prelude.ParserT Prelude.DataView m (Prelude.Builder ModifyRadioRequestR ModifyRadioRequestR)
  parseField 1 Prelude.VarInt = Prelude.label "start / " $ do
    x <- Prelude.decodeBool
    pure $ Prelude.modify (Prelude.Proxy :: Prelude.Proxy "start") $ \_ -> Prelude.Just x
  parseField fieldNumber wireType = Prelude.parseFieldUnknown fieldNumber wireType

defaultModifyRadioRequest :: ModifyRadioRequestR
defaultModifyRadioRequest =
  { start: Prelude.Nothing
  , __unknown_fields: []
  }

mkModifyRadioRequest :: forall r1 r3. Prelude.Union r1 ModifyRadioRequestRow r3 => Prelude.Nub r3 ModifyRadioRequestRow => Record r1 -> ModifyRadioRequest
mkModifyRadioRequest r = ModifyRadioRequest $ Prelude.merge r defaultModifyRadioRequest

mergeModifyRadioRequest :: ModifyRadioRequest -> ModifyRadioRequest -> ModifyRadioRequest
mergeModifyRadioRequest (ModifyRadioRequest l) (ModifyRadioRequest r) = ModifyRadioRequest
  { start: Prelude.alt l.start r.start
  , __unknown_fields: r.__unknown_fields <> l.__unknown_fields
  }


-- | Message generated by __protobuf__ from `ModifyRadioResponse`
-- | 
-- | Sent to HTTP Server
newtype ModifyRadioResponse = ModifyRadioResponse ModifyRadioResponseR
type ModifyRadioResponseRow =
  ( __unknown_fields :: Array Prelude.UnknownField
  )
type ModifyRadioResponseR = Record ModifyRadioResponseRow
derive instance genericModifyRadioResponse :: Prelude.Generic ModifyRadioResponse _
derive instance newtypeModifyRadioResponse :: Prelude.Newtype ModifyRadioResponse _
derive instance eqModifyRadioResponse :: Prelude.Eq ModifyRadioResponse
instance showModifyRadioResponse :: Prelude.Show ModifyRadioResponse where show x = Prelude.genericShow x

putModifyRadioResponse :: forall m. Prelude.MonadEffect m => ModifyRadioResponse -> Prelude.PutM m Prelude.Unit
putModifyRadioResponse (ModifyRadioResponse r) = do

  Prelude.traverse_ Prelude.putFieldUnknown r.__unknown_fields

parseModifyRadioResponse :: forall m. Prelude.MonadEffect m => Prelude.MonadRec m => Prelude.ByteLength -> Prelude.ParserT Prelude.DataView m ModifyRadioResponse
parseModifyRadioResponse length = Prelude.label "ModifyRadioResponse / " $
  Prelude.parseMessage ModifyRadioResponse defaultModifyRadioResponse parseField length
 where
  parseField
    :: Prelude.FieldNumberInt
    -> Prelude.WireType
    -> Prelude.ParserT Prelude.DataView m (Prelude.Builder ModifyRadioResponseR ModifyRadioResponseR)

  parseField fieldNumber wireType = Prelude.parseFieldUnknown fieldNumber wireType

defaultModifyRadioResponse :: ModifyRadioResponseR
defaultModifyRadioResponse =
  { __unknown_fields: []
  }

mkModifyRadioResponse :: forall r1 r3. Prelude.Union r1 ModifyRadioResponseRow r3 => Prelude.Nub r3 ModifyRadioResponseRow => Record r1 -> ModifyRadioResponse
mkModifyRadioResponse r = ModifyRadioResponse $ Prelude.merge r defaultModifyRadioResponse

mergeModifyRadioResponse :: ModifyRadioResponse -> ModifyRadioResponse -> ModifyRadioResponse
mergeModifyRadioResponse (ModifyRadioResponse l) (ModifyRadioResponse r) = ModifyRadioResponse
  { __unknown_fields: r.__unknown_fields <> l.__unknown_fields
  }


-- | Message generated by __protobuf__ from `GetRadioStatusRequest`
-- | 
-- | Received from HTTP Server.
newtype GetRadioStatusRequest = GetRadioStatusRequest GetRadioStatusRequestR
type GetRadioStatusRequestRow =
  ( __unknown_fields :: Array Prelude.UnknownField
  )
type GetRadioStatusRequestR = Record GetRadioStatusRequestRow
derive instance genericGetRadioStatusRequest :: Prelude.Generic GetRadioStatusRequest _
derive instance newtypeGetRadioStatusRequest :: Prelude.Newtype GetRadioStatusRequest _
derive instance eqGetRadioStatusRequest :: Prelude.Eq GetRadioStatusRequest
instance showGetRadioStatusRequest :: Prelude.Show GetRadioStatusRequest where show x = Prelude.genericShow x

putGetRadioStatusRequest :: forall m. Prelude.MonadEffect m => GetRadioStatusRequest -> Prelude.PutM m Prelude.Unit
putGetRadioStatusRequest (GetRadioStatusRequest r) = do

  Prelude.traverse_ Prelude.putFieldUnknown r.__unknown_fields

parseGetRadioStatusRequest :: forall m. Prelude.MonadEffect m => Prelude.MonadRec m => Prelude.ByteLength -> Prelude.ParserT Prelude.DataView m GetRadioStatusRequest
parseGetRadioStatusRequest length = Prelude.label "GetRadioStatusRequest / " $
  Prelude.parseMessage GetRadioStatusRequest defaultGetRadioStatusRequest parseField length
 where
  parseField
    :: Prelude.FieldNumberInt
    -> Prelude.WireType
    -> Prelude.ParserT Prelude.DataView m (Prelude.Builder GetRadioStatusRequestR GetRadioStatusRequestR)

  parseField fieldNumber wireType = Prelude.parseFieldUnknown fieldNumber wireType

defaultGetRadioStatusRequest :: GetRadioStatusRequestR
defaultGetRadioStatusRequest =
  { __unknown_fields: []
  }

mkGetRadioStatusRequest :: forall r1 r3. Prelude.Union r1 GetRadioStatusRequestRow r3 => Prelude.Nub r3 GetRadioStatusRequestRow => Record r1 -> GetRadioStatusRequest
mkGetRadioStatusRequest r = GetRadioStatusRequest $ Prelude.merge r defaultGetRadioStatusRequest

mergeGetRadioStatusRequest :: GetRadioStatusRequest -> GetRadioStatusRequest -> GetRadioStatusRequest
mergeGetRadioStatusRequest (GetRadioStatusRequest l) (GetRadioStatusRequest r) = GetRadioStatusRequest
  { __unknown_fields: r.__unknown_fields <> l.__unknown_fields
  }


-- | Message generated by __protobuf__ from `GetRadioStatusResponse`
-- | 
-- | Sent to HTTP Server
newtype GetRadioStatusResponse = GetRadioStatusResponse GetRadioStatusResponseR
type GetRadioStatusResponseRow =
  ( radioOn :: Prelude.Maybe Boolean
  , stateId :: Prelude.Maybe Prelude.UInt
  , __unknown_fields :: Array Prelude.UnknownField
  )
type GetRadioStatusResponseR = Record GetRadioStatusResponseRow
derive instance genericGetRadioStatusResponse :: Prelude.Generic GetRadioStatusResponse _
derive instance newtypeGetRadioStatusResponse :: Prelude.Newtype GetRadioStatusResponse _
derive instance eqGetRadioStatusResponse :: Prelude.Eq GetRadioStatusResponse
instance showGetRadioStatusResponse :: Prelude.Show GetRadioStatusResponse where show x = Prelude.genericShow x

putGetRadioStatusResponse :: forall m. Prelude.MonadEffect m => GetRadioStatusResponse -> Prelude.PutM m Prelude.Unit
putGetRadioStatusResponse (GetRadioStatusResponse r) = do
  Prelude.putOptional 1 r.radioOn Prelude.isDefault Prelude.encodeBoolField
  Prelude.putOptional 2 r.stateId Prelude.isDefault Prelude.encodeUint32Field
  Prelude.traverse_ Prelude.putFieldUnknown r.__unknown_fields

parseGetRadioStatusResponse :: forall m. Prelude.MonadEffect m => Prelude.MonadRec m => Prelude.ByteLength -> Prelude.ParserT Prelude.DataView m GetRadioStatusResponse
parseGetRadioStatusResponse length = Prelude.label "GetRadioStatusResponse / " $
  Prelude.parseMessage GetRadioStatusResponse defaultGetRadioStatusResponse parseField length
 where
  parseField
    :: Prelude.FieldNumberInt
    -> Prelude.WireType
    -> Prelude.ParserT Prelude.DataView m (Prelude.Builder GetRadioStatusResponseR GetRadioStatusResponseR)
  parseField 1 Prelude.VarInt = Prelude.label "radioOn / " $ do
    x <- Prelude.decodeBool
    pure $ Prelude.modify (Prelude.Proxy :: Prelude.Proxy "radioOn") $ \_ -> Prelude.Just x
  parseField 2 Prelude.VarInt = Prelude.label "stateId / " $ do
    x <- Prelude.decodeUint32
    pure $ Prelude.modify (Prelude.Proxy :: Prelude.Proxy "stateId") $ \_ -> Prelude.Just x
  parseField fieldNumber wireType = Prelude.parseFieldUnknown fieldNumber wireType

defaultGetRadioStatusResponse :: GetRadioStatusResponseR
defaultGetRadioStatusResponse =
  { radioOn: Prelude.Nothing
  , stateId: Prelude.Nothing
  , __unknown_fields: []
  }

mkGetRadioStatusResponse :: forall r1 r3. Prelude.Union r1 GetRadioStatusResponseRow r3 => Prelude.Nub r3 GetRadioStatusResponseRow => Record r1 -> GetRadioStatusResponse
mkGetRadioStatusResponse r = GetRadioStatusResponse $ Prelude.merge r defaultGetRadioStatusResponse

mergeGetRadioStatusResponse :: GetRadioStatusResponse -> GetRadioStatusResponse -> GetRadioStatusResponse
mergeGetRadioStatusResponse (GetRadioStatusResponse l) (GetRadioStatusResponse r) = GetRadioStatusResponse
  { radioOn: Prelude.alt l.radioOn r.radioOn
  , stateId: Prelude.alt l.stateId r.stateId
  , __unknown_fields: r.__unknown_fields <> l.__unknown_fields
  }


-- | Message generated by __protobuf__ from `ProxyRecieveEnvelope`
newtype ProxyRecieveEnvelope = ProxyRecieveEnvelope ProxyRecieveEnvelopeR
type ProxyRecieveEnvelopeRow =
  ( payload :: Prelude.Maybe ProxyRecieveEnvelope_Payload
  , __unknown_fields :: Array Prelude.UnknownField
  )
type ProxyRecieveEnvelopeR = Record ProxyRecieveEnvelopeRow
derive instance genericProxyRecieveEnvelope :: Prelude.Generic ProxyRecieveEnvelope _
derive instance newtypeProxyRecieveEnvelope :: Prelude.Newtype ProxyRecieveEnvelope _
derive instance eqProxyRecieveEnvelope :: Prelude.Eq ProxyRecieveEnvelope
instance showProxyRecieveEnvelope :: Prelude.Show ProxyRecieveEnvelope where show x = Prelude.genericShow x

putProxyRecieveEnvelope :: forall m. Prelude.MonadEffect m => ProxyRecieveEnvelope -> Prelude.PutM m Prelude.Unit
putProxyRecieveEnvelope (ProxyRecieveEnvelope r) = do
  case r.payload of
    Prelude.Nothing -> pure Prelude.unit
    Prelude.Just (ProxyRecieveEnvelope_Payload_M1 x) -> Prelude.putOptional 1 (Prelude.Just x) (\_ -> false) $ Prelude.putLenDel putModifyRadioResponse
    Prelude.Just (ProxyRecieveEnvelope_Payload_M2 x) -> Prelude.putOptional 2 (Prelude.Just x) (\_ -> false) $ Prelude.putLenDel putGetRadioStatusResponse
  Prelude.traverse_ Prelude.putFieldUnknown r.__unknown_fields

parseProxyRecieveEnvelope :: forall m. Prelude.MonadEffect m => Prelude.MonadRec m => Prelude.ByteLength -> Prelude.ParserT Prelude.DataView m ProxyRecieveEnvelope
parseProxyRecieveEnvelope length = Prelude.label "ProxyRecieveEnvelope / " $
  Prelude.parseMessage ProxyRecieveEnvelope defaultProxyRecieveEnvelope parseField length
 where
  parseField
    :: Prelude.FieldNumberInt
    -> Prelude.WireType
    -> Prelude.ParserT Prelude.DataView m (Prelude.Builder ProxyRecieveEnvelopeR ProxyRecieveEnvelopeR)
  parseField 1 Prelude.LenDel = Prelude.label "m1 / " $ do
    x <- Prelude.parseLenDel parseModifyRadioResponse
    pure $ Prelude.modify (Prelude.Proxy :: Prelude.Proxy "payload") $ mergeProxyRecieveEnvelope_Payload (Prelude.Just (ProxyRecieveEnvelope_Payload_M1 x))
  parseField 2 Prelude.LenDel = Prelude.label "m2 / " $ do
    x <- Prelude.parseLenDel parseGetRadioStatusResponse
    pure $ Prelude.modify (Prelude.Proxy :: Prelude.Proxy "payload") $ mergeProxyRecieveEnvelope_Payload (Prelude.Just (ProxyRecieveEnvelope_Payload_M2 x))
  parseField fieldNumber wireType = Prelude.parseFieldUnknown fieldNumber wireType

defaultProxyRecieveEnvelope :: ProxyRecieveEnvelopeR
defaultProxyRecieveEnvelope =
  { payload: Prelude.Nothing
  , __unknown_fields: []
  }

mkProxyRecieveEnvelope :: forall r1 r3. Prelude.Union r1 ProxyRecieveEnvelopeRow r3 => Prelude.Nub r3 ProxyRecieveEnvelopeRow => Record r1 -> ProxyRecieveEnvelope
mkProxyRecieveEnvelope r = ProxyRecieveEnvelope $ Prelude.merge r defaultProxyRecieveEnvelope
data ProxyRecieveEnvelope_Payload
  = ProxyRecieveEnvelope_Payload_M1 ModifyRadioResponse
  | ProxyRecieveEnvelope_Payload_M2 GetRadioStatusResponse

derive instance genericProxyRecieveEnvelope_Payload :: Prelude.Generic ProxyRecieveEnvelope_Payload _
derive instance eqProxyRecieveEnvelope_Payload :: Prelude.Eq ProxyRecieveEnvelope_Payload
instance showProxyRecieveEnvelope_Payload :: Prelude.Show ProxyRecieveEnvelope_Payload where show = Prelude.genericShow

mergeProxyRecieveEnvelope_Payload :: Prelude.Maybe ProxyRecieveEnvelope_Payload -> Prelude.Maybe ProxyRecieveEnvelope_Payload -> Prelude.Maybe ProxyRecieveEnvelope_Payload
mergeProxyRecieveEnvelope_Payload l r = case Prelude.Tuple l r of
  Prelude.Tuple (Prelude.Just (ProxyRecieveEnvelope_Payload_M1 l')) (Prelude.Just (ProxyRecieveEnvelope_Payload_M1 r')) -> Prelude.map ProxyRecieveEnvelope_Payload_M1 $ Prelude.mergeWith mergeModifyRadioResponse (Prelude.Just l') (Prelude.Just r')
  Prelude.Tuple (Prelude.Just (ProxyRecieveEnvelope_Payload_M2 l')) (Prelude.Just (ProxyRecieveEnvelope_Payload_M2 r')) -> Prelude.map ProxyRecieveEnvelope_Payload_M2 $ Prelude.mergeWith mergeGetRadioStatusResponse (Prelude.Just l') (Prelude.Just r')
  _ -> Prelude.alt l r

mergeProxyRecieveEnvelope :: ProxyRecieveEnvelope -> ProxyRecieveEnvelope -> ProxyRecieveEnvelope
mergeProxyRecieveEnvelope (ProxyRecieveEnvelope l) (ProxyRecieveEnvelope r) = ProxyRecieveEnvelope
  { payload: mergeProxyRecieveEnvelope_Payload l.payload r.payload
  , __unknown_fields: r.__unknown_fields <> l.__unknown_fields
  }


