{-# LANGUAGE LambdaCase #-}

module HAsio.Control (bracket, bracket_, bracketOnError, finally) where

import Control.Exception qualified
import Control.Monad.Except (ExceptT (..), runExceptT)
import HAsio.Error.ErrorStack (ErrorStack)

bracket ::
  ExceptT ErrorStack IO a
  -> (a -> ExceptT ErrorStack IO b)
  -> (a -> ExceptT ErrorStack IO c)
  -> ExceptT ErrorStack IO c
bracket aquire close use =
  ExceptT $
    Control.Exception.bracket
      (runExceptT aquire)
      ( \case
          Left errs -> pure $ Left errs
          Right a -> runExceptT $ close a
      )
      ( \case
          Left errs -> pure $ Left errs
          Right a -> runExceptT $ use a
      )

bracket_ ::
  ExceptT ErrorStack IO a
  -> ExceptT ErrorStack IO b
  -> ExceptT ErrorStack IO c
  -> ExceptT ErrorStack IO c
bracket_ aquire close use = do
  ExceptT $
    Control.Exception.bracket_
      (runExceptT aquire)
      ( runExceptT close
      )
      ( runExceptT use
      )

bracketOnError ::
  ExceptT ErrorStack IO a
  -> (a -> ExceptT ErrorStack IO b)
  -> (a -> ExceptT ErrorStack IO c)
  -> ExceptT ErrorStack IO c
bracketOnError aquire close use =
  ExceptT $
    Control.Exception.bracketOnError
      (runExceptT aquire)
      ( \case
          Left errs -> pure $ Left errs
          Right a -> runExceptT $ close a
      )
      ( \case
          Left errs -> pure $ Left errs
          Right a -> runExceptT $ use a
      )

finally ::
  ExceptT ErrorStack IO a
  -> ExceptT ErrorStack IO b
  -> ExceptT ErrorStack IO a
finally run tidyup = do
  ExceptT $ Control.Exception.finally (runExceptT run) (runExceptT tidyup)
