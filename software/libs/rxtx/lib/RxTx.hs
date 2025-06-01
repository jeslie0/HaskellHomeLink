{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module RxTx (
  RxTx,
  Rx (..),
  Tx (..),
) where

import RxTx.Rx (Rx (..))
import RxTx.Tx (Tx (..))

type RxTx msg chan rxErr txErr = (Rx msg chan rxErr, Tx msg chan txErr)

