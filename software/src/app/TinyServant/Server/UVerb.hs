{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module TinyServant.Server.UVerb where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.SOP (I (..))
import Data.SOP.Constraint (All, And)
import Data.String.Conversions (cs)
import Data.Text qualified as T
import Network.HTTP.Types (HeaderName, Status, hContentType)
import Network.Wai (Request, Response, responseLBS)
import TinyServant.API (ReflectMethod (..))
import TinyServant.API.ContentTypes (AllCTRender, AllMime, getAcceptHeader, handleAcceptH)
import TinyServant.API.ResponseHeaders (GetHeaders (..), Headers (..))
import TinyServant.API.UVerb (HasStatus, Statuses, UVerb, WithStatus (..), statusOf)
import TinyServant.Server (HasServer (..), allowedMethodHead)
import TinyServant.Union (IsMember, Union, Unique, foldMapUnion, inject)

-- | 'return' for 'UVerb' handlers.  Takes a value of any of the members of the open union,
-- and will construct a union value in an 'Applicative' (eg. 'Server').
respond ::
  forall (x :: Type) (xs :: [Type]) (f :: Type -> Type).
  (Applicative f, HasStatus x, IsMember x xs) =>
  x ->
  f (Union xs)
respond = pure . inject . I

class IsServerResource (cts :: [Type]) a where
  resourceResponse :: Request -> Proxy cts -> a -> Maybe (LB.ByteString, LB.ByteString)
  resourceHeaders :: Proxy cts -> a -> [(HeaderName, B.ByteString)]

instance
  {-# OVERLAPPABLE #-}
  (AllCTRender cts a) =>
  IsServerResource cts a
  where
  resourceHeaders _ _ = []
  resourceResponse request p = handleAcceptH p (getAcceptHeader request)

instance
  {-# OVERLAPPING #-}
  (IsServerResource cts a, GetHeaders (Headers h a)) =>
  IsServerResource cts (Headers h a)
  where
  resourceResponse request p res = resourceResponse request p (getResponse res)
  resourceHeaders cts res = getHeaders res ++ resourceHeaders cts (getResponse res)

instance
  {-# OVERLAPPING #-}
  (IsServerResource cts a) =>
  IsServerResource cts (WithStatus n a)
  where
  resourceResponse request p (WithStatus x) = resourceResponse request p x
  resourceHeaders cts (WithStatus x) = resourceHeaders cts x

encodeResource ::
  forall a cts.
  (IsServerResource cts a, HasStatus a) =>
  Request ->
  Proxy cts ->
  a ->
  (Status, Maybe (LB.ByteString, LB.ByteString), [(HeaderName, B.ByteString)])
encodeResource request cts res =
  ( statusOf (Proxy @a),
    resourceResponse request cts res,
    resourceHeaders cts res
  )

type IsServerResourceWithStatus cts = IsServerResource cts `And` HasStatus

instance
  ( ReflectMethod method,
    AllMime contentTypes,
    All (IsServerResourceWithStatus contentTypes) as,
    Unique (Statuses as)
  ) =>
  HasServer (UVerb method contentTypes as :: Type)
  where
  route ::
    Proxy (UVerb method status as) ->
    IO (Union as) ->
    [T.Text] ->
    Request ->
    Maybe (IO Response)
  route _ handler _ request =
    let method = reflectMethod (Proxy @method)
        cts = Proxy @contentTypes
        pickResource :: Union as -> (Status, Maybe (LB.ByteString, LB.ByteString), [(HeaderName, B.ByteString)])
        pickResource = foldMapUnion (Proxy @(IsServerResourceWithStatus contentTypes)) (encodeResource request cts)
     in Just $ do
          union <- handler
          case pickResource union of
            (_, Nothing, _) -> undefined
            (status, Just (contentT, body), headers) ->
              let bdy = if allowedMethodHead method request then "" else body
               in return $ responseLBS status ((hContentType, cs contentT) : headers) bdy
