{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module : Constraints
Description : Helper code to make it easier to generate constraints.
-}
module Constraints (AllHaveConstraint) where

import Data.Kind (Constraint, Type)

{- | A type family that constrains the type level list to all satisfy the given
constraint.
-}
type family AllHaveConstraint (c :: Type -> Constraint) (xs :: [Type]) :: Constraint where
    AllHaveConstraint _ '[] = ()
    AllHaveConstraint c (x ': xs) = (c x, AllHaveConstraint c xs)
