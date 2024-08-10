{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TinyServant.API.ContentTypes where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON, encode)
import Data.Aeson.Types (parseEither)
import Data.Aeson.Parser (json)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Data.Text qualified as TextS
import Data.Text.Encoding qualified as TextS
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits qualified as TL
import Network.HTTP.Media (MediaType, RenderHeader (..), mapAcceptMedia, mapContentMedia, matchAccept, (//), (/:))
import Web.FormUrlEncoded (ToForm, urlEncodeAsForm, urlDecodeAsForm)
import Web.Internal.FormUrlEncoded (FromForm)
import Control.Arrow (left)
import Data.Bifunctor (bimap)
import Network.Wai (Request (..))
import Network.HTTP.Types (hAccept)

-- * Provided content types

data JSON deriving (Typeable)

data PlainText deriving (Typeable)

data FormUrlEncoded deriving (Typeable)

data OctetStream deriving (Typeable)

-- * Accept class

-- | Instances of 'Accept' represent mimetypes. They are used for matching
-- against the @Accept@ HTTP header of the request, and for setting the
-- @Content-Type@ header of the response
--
-- Example:
--
-- >>> import Network.HTTP.Media ((//), (/:))
-- >>> data HTML
-- >>> :{
-- instance Accept HTML where
--    contentType _ = "text" // "html" /: ("charset", "utf-8")
-- :}
class Accept ctype where
  contentType :: Proxy ctype -> MediaType
  contentType = NE.head . contentTypes

  contentTypes :: Proxy ctype -> NE.NonEmpty MediaType
  contentTypes = (NE.:| []) . contentType

  {-# MINIMAL contentType | contentTypes #-}

-- | @application/json@
instance Accept JSON where
  contentTypes _ =
    "application" // "json" /: ("charset", "utf-8")
      NE.:| ["application" // "json"]

-- | @application/x-www-form-urlencoded@
instance Accept FormUrlEncoded where
  contentType _ = "application" // "x-www-form-urlencoded"

-- | @text/plain;charset=utf-8@
instance Accept PlainText where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

-- | @application/octet-stream@
instance Accept OctetStream where
  contentType _ = "application" // "octet-stream"

newtype AcceptHeader = AcceptHeader B.ByteString
  deriving (Eq, Show, Read, Typeable, Generic)

-- * Render (serializing)

-- | Instantiate this class to register a way of serializing a type based
-- on the @Accept@ header.
--
-- Example:
--
-- > data MyContentType
-- >
-- > instance Accept MyContentType where
-- >    contentType _ = "example" // "prs.me.mine" /: ("charset", "utf-8")
-- >
-- > instance Show a => MimeRender MyContentType a where
-- >    mimeRender _ val = pack ("This is MINE! " ++ show val)
-- >
-- > type MyAPI = "path" :> Get '[MyContentType] Int
class (Accept ctype) => MimeRender ctype a where
  mimeRender :: Proxy ctype -> a -> BL.ByteString

class (AllMime list) => AllCTRender (list :: [Type]) a where
  -- If the Accept header can be matched, returns (Just) a tuple of the
  -- Content-Type and response (serialization of @a@ into the appropriate
  -- mimetype).
  handleAcceptH :: Proxy list -> AcceptHeader -> a -> Maybe (BL.ByteString, BL.ByteString)

instance {-# OVERLAPPABLE #-} (Accept ct, AllMime cts, AllMimeRender (ct ': cts) a) => AllCTRender (ct ': cts) a where
  handleAcceptH _ (AcceptHeader accept) val = mapAcceptMedia lkup accept
    where
      pctyps = Proxy :: Proxy (ct ': cts)
      amrs = allMimeRender pctyps val
      lkup = fmap (\(a, b) -> (a, (BL.fromStrict $ renderHeader a, b))) amrs

instance
  (TL.TypeError ('TL.Text "No instance for (), use NoContent instead.")) =>
  AllCTRender '[] ()
  where
  handleAcceptH _ _ _ = error "unreachable"

--------------------------------------------------------------------------

-- * Unrender

-- | Instantiate this class to register a way of deserializing a type based
-- on the request's @Content-Type@ header.
--
-- >>> import Network.HTTP.Media hiding (Accept)
-- >>> import qualified Data.ByteString.Lazy.Char8 as BSC
-- >>> data MyContentType = MyContentType String
--
-- >>> :{
-- instance Accept MyContentType where
--    contentType _ = "example" // "prs.me.mine" /: ("charset", "utf-8")
-- :}
--
-- >>> :{
-- instance Read a => MimeUnrender MyContentType a where
--    mimeUnrender _ bs = case BSC.take 12 bs of
--      "MyContentType" -> return . read . BSC.unpack $ BSC.drop 12 bs
--      _ -> Left "didn't start with the magic incantation"
-- :}
--
-- >>> type MyAPI = "path" :> ReqBody '[MyContentType] Int :> Get '[JSON] Int
class (Accept ctype) => MimeUnrender ctype a where
  mimeUnrender :: Proxy ctype -> BL.ByteString -> Either String a
  mimeUnrender p = mimeUnrenderWithType p (contentType p)

  -- | Variant which is given the actual 'MediaType' provided by the other party.
  --
  -- In the most cases you don't want to branch based on the 'MediaType'.
  -- See <https://github.com/haskell-servant/servant/pull/552 pr552> for a motivating example.
  mimeUnrenderWithType :: Proxy ctype -> MediaType -> BL.ByteString -> Either String a
  mimeUnrenderWithType p _ = mimeUnrender p

  {-# MINIMAL mimeUnrender | mimeUnrenderWithType #-}

class AllCTUnrender (list :: [Type]) a where
  canHandleCTypeH ::
    Proxy list ->
    BL.ByteString -> -- Content-Type header
    Maybe (BL.ByteString -> Either String a)

  handleCTypeH ::
    Proxy list ->
    BL.ByteString -> -- Content-Type header
    BL.ByteString -> -- Request body
    Maybe (Either String a)
  handleCTypeH p ctypeH body = ($ body) `fmap` canHandleCTypeH p ctypeH

instance (AllMimeUnrender ctyps a) => AllCTUnrender ctyps a where
  canHandleCTypeH p ctypeH =
    mapContentMedia (allMimeUnrender p) (cs ctypeH)

--------------------------------------------------------------------------

-- * Utils (Internal)

class AllMime (list :: [Type]) where
  allMime :: Proxy list -> [MediaType]

instance AllMime '[] where
  allMime _ = []

instance (Accept ctyp, AllMime ctyps) => AllMime (ctyp ': ctyps) where
  allMime _ = NE.toList (contentTypes pctyp) ++ allMime pctyps
    where
      pctyp = Proxy :: Proxy ctyp
      pctyps = Proxy :: Proxy ctyps

canHandleAcceptH :: (AllMime list) => Proxy list -> AcceptHeader -> Bool
canHandleAcceptH p (AcceptHeader h) = isJust $ matchAccept (allMime p) h

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeRender
--------------------------------------------------------------------------
class (AllMime list) => AllMimeRender (list :: [Type]) a where
  allMimeRender ::
    Proxy list ->
    a -> -- value to serialize
    [(MediaType, BL.ByteString)] -- content-types/response pairs

instance {-# OVERLAPPABLE #-} (MimeRender ctyp a) => AllMimeRender '[ctyp] a where
  allMimeRender _ a = map (,bs) $ NE.toList $ contentTypes pctyp
    where
      bs = mimeRender pctyp a
      pctyp = Proxy :: Proxy ctyp

instance
  {-# OVERLAPPABLE #-}
  ( MimeRender ctyp a,
    AllMimeRender (ctyp' ': ctyps) a
  ) =>
  AllMimeRender (ctyp ': ctyp' ': ctyps) a
  where
  allMimeRender _ a =
    map (,bs) (NE.toList $ contentTypes pctyp)
      ++ allMimeRender pctyps a
    where
      bs = mimeRender pctyp a
      pctyp = Proxy :: Proxy ctyp
      pctyps = Proxy :: Proxy (ctyp' ': ctyps)

-- Ideally we would like to declare a 'MimeRender a NoContent' instance, and
-- then this would be taken care of. However there is no more specific instance
-- between that and 'MimeRender JSON a', so we do this instead
instance {-# OVERLAPPING #-} (Accept ctyp) => AllMimeRender '[ctyp] NoContent where
  allMimeRender _ NoContent = map (,"") $ NE.toList $ contentTypes pctyp
    where
      pctyp = Proxy :: Proxy ctyp

instance
  {-# OVERLAPPING #-}
  ( AllMime (ctyp ': ctyp' ': ctyps)
  ) =>
  AllMimeRender (ctyp ': ctyp' ': ctyps) NoContent
  where
  allMimeRender p _ = zip (allMime p) (repeat "")

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeUnrender
--------------------------------------------------------------------------
class (AllMime list) => AllMimeUnrender (list :: [Type]) a where
  allMimeUnrender ::
    Proxy list ->
    [(MediaType, BL.ByteString -> Either String a)]

instance AllMimeUnrender '[] a where
  allMimeUnrender _ = []

instance
  ( MimeUnrender ctyp a,
    AllMimeUnrender ctyps a
  ) =>
  AllMimeUnrender (ctyp ': ctyps) a
  where
  allMimeUnrender _ =
    map mk (NE.toList $ contentTypes pctyp)
      ++ allMimeUnrender pctyps
    where
      mk ct = (ct, mimeUnrenderWithType pctyp ct)
      pctyp = Proxy :: Proxy ctyp
      pctyps = Proxy :: Proxy ctyps

--------------------------------------------------------------------------

-- * MimeRender Instances

-- | `encode`
instance {-# OVERLAPPABLE #-} (ToJSON a) => MimeRender JSON a where
  mimeRender _ = encode

-- | @urlEncodeAsForm@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance {-# OVERLAPPABLE #-} (ToForm a) => MimeRender FormUrlEncoded a where
  mimeRender _ = urlEncodeAsForm

-- | `TextL.encodeUtf8`
instance MimeRender PlainText TextL.Text where
  mimeRender _ = TextL.encodeUtf8

-- | @fromStrict . TextS.encodeUtf8@
instance MimeRender PlainText TextS.Text where
  mimeRender _ = BL.fromStrict . TextS.encodeUtf8

-- | @BC.pack@
instance MimeRender PlainText String where
  mimeRender _ = TextL.encodeUtf8 . TextL.pack

-- | @id@
instance MimeRender OctetStream BL.ByteString where
  mimeRender _ = id

-- | `fromStrict`
instance MimeRender OctetStream B.ByteString where
  mimeRender _ = BL.fromStrict

-- | A type for responses without content-body.
data NoContent = NoContent
  deriving (Show, Eq, Read, Generic)

instance NFData NoContent

--------------------------------------------------------------------------

-- * MimeUnrender Instances

-- | Like 'Data.Aeson.eitherDecode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Will handle trailing whitespace, but not trailing junk. ie.
--
-- >>> eitherDecodeLenient "1 " :: Either String Int
-- Right 1
--
-- >>> eitherDecodeLenient "1 junk" :: Either String Int
-- Left "trailing junk after valid JSON: endOfInput"
eitherDecodeLenient :: (FromJSON a) => BL.ByteString -> Either String a
eitherDecodeLenient input =
  parseOnly parser (cs input) >>= parseEither parseJSON
  where
    parser =
      skipSpace
        *> Data.Aeson.Parser.json
        <* skipSpace
        <* (endOfInput <?> "trailing junk after valid JSON")

-- | `eitherDecode`
instance (FromJSON a) => MimeUnrender JSON a where
  mimeUnrender _ = eitherDecodeLenient

-- | @urlDecodeAsForm@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance (FromForm a) => MimeUnrender FormUrlEncoded a where
  mimeUnrender _ = left TextS.unpack . urlDecodeAsForm

-- | @left show . TextL.decodeUtf8'@
instance MimeUnrender PlainText TextL.Text where
  mimeUnrender _ = left show . TextL.decodeUtf8'

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender PlainText TextS.Text where
  mimeUnrender _ = left show . TextS.decodeUtf8' . BL.toStrict

-- | @Right . BC.unpack@
instance MimeUnrender PlainText String where
  mimeUnrender _ = bimap show TextL.unpack . TextL.decodeUtf8'

-- | @Right . id@
instance MimeUnrender OctetStream BL.ByteString where
  mimeUnrender _ = Right . id

-- | @Right . toStrict@
instance MimeUnrender OctetStream B.ByteString where
  mimeUnrender _ = Right . BL.toStrict


getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader . fromMaybe ct_wildcard . lookup hAccept . requestHeaders
  where
    ct_wildcard :: B.ByteString
    ct_wildcard = "*" <> "/" <> "*"
