{-# LANGUAGE UndecidableInstances #-}

module HIO.Fd (
  Fd,
  IsFd (..),
) where

import HIO.Fd.IsFd (IsFd(..))
import System.Posix (Fd (..))
import Prelude hiding (read)

-- * IsFd

-- We export a typeclass for things which behave like C file
-- descriptors. We require accessing the file descriptor to be a pure
-- function, which rules out Haskell's Handle and Socket types.


