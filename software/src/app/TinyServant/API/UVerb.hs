{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TinyServant.API.UVerb where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat)
import Network.HTTP.Types (Status)
import TinyServant.API.ContentTypes (NoContent)
import TinyServant.API.Status (KnownStatus (..))

class (KnownStatus (StatusOf a)) => HasStatus (a :: Type) where
  type StatusOf (a :: Type) :: Nat

statusOf :: forall a proxy. (HasStatus a) => proxy a -> Status
statusOf = const (statusVal (Proxy :: Proxy (StatusOf a)))

class HasStatuses (as :: [Type]) where
  type Statuses (as :: [Type]) :: [Nat]
  statuses :: Proxy as -> [Status]

instance HasStatuses ('[] :: [Type]) where
  type Statuses '[] = '[]
  statuses _ = []

instance (HasStatus a, HasStatuses as) => HasStatuses (a ': as) where
  type Statuses (a ': as) = StatusOf a ': Statuses as
  statuses _ = statusOf (Proxy :: Proxy a) : statuses (Proxy :: Proxy as)

-- | If an API can respond with 'NoContent' we assume that this will happen
-- with the status code 204 No Content. If this needs to be overridden,
-- 'WithStatus' can be used.
instance HasStatus NoContent where
  type StatusOf NoContent = 204

newtype WithStatus (k :: Nat) a = WithStatus a deriving (Eq, Show)

instance (KnownStatus n) => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n

-- instance (HasStatus a) => HasStatus (Headers ls a) where
--   type StatusOf (Headers ls a) = StatusOf a

data UVerb (method :: k1) (contentTypes :: [Type]) (as :: [Type])

type Verb method statusCode contentTypes a = UVerb method contentTypes '[WithStatus statusCode a]
