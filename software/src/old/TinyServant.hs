{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TinyServant where

import Control.Applicative
import Data.Aeson (ToJSON, encode)
import Data.Either (Either (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.TypeLits
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.HTTP.Types.Status
import Network.Wai (Application, Request (..), Response, responseLBS)
import Text.Read (readMaybe)
import Prelude hiding (Left)
import TinyServant.API (ReflectMethod)

-- * Basic API types

-- We can include content type information here?



-- type Get = Verb 'GET 200

-- type Post = Verb 'POST 200



-- * HasServer Class

-- The route function's purpose is to take an incomping request and
-- dispatch it to the correct handler.




