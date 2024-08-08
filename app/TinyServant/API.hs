{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TinyServant.API where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownNat, Nat)
import Network.HTTP.Types (Method, Status, methodGet, methodPost)

class ReflectMethod a where
  reflectMethod :: Proxy a -> Method

data StdMethod
  = GET
  | POST


instance ReflectMethod 'GET where
  reflectMethod _ = methodGet

instance ReflectMethod 'POST where
  reflectMethod _ = methodPost
