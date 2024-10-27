{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Handler (HasHandler(..), HandleableMsg(..)) where

import Data.Kind (Constraint, Type)
import Msg (Msg (..))

-- | The handler type class. Instances have a canonical way of being
-- handled.
class HasHandler a where
  handle :: a -> IO ()

-- | This type family allows us to constrain a type level list to
-- guarantee that it's entries all satisfy the HasHandler constraint.
type family AllHaveHandler (xs :: [Type]) :: Constraint where
  AllHaveHandler '[] = ()
  AllHaveHandler (x ': xs) = (HasHandler x, AllHaveHandler xs)


data HandleableMsg = forall msg. (Msg msg, HasHandler msg) => HandleableMsg msg
