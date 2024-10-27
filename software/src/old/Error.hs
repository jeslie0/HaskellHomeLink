{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Error where

import Control.Monad.Writer qualified as W
import Data.Text qualified as T

data LogLevel
  = Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Informational
  | Debug
  deriving (Eq, Show, Ord)

-- | A class to represent an error.
class Loggable a where
  showLog :: a -> T.Text
  logLevel :: LogLevel

instance Show Log where
  show (Log a) = T.unpack . showLog $ a

-- | An existential type to allow working with errors of different
-- types generically.
data Log = forall a. (Loggable a) => Log a

newtype LogStack = LogStack [Log] deriving (Semigroup, Monoid, Show)

type LogWriter a = W.Writer LogStack (Maybe a)

newtype BasicLog = BLog T.Text

instance Loggable BasicLog where
  showLog (BLog txt) = txt
  logLevel = Informational


-- instance Functor LogWriter where
--   fmap f (LogWriter w) = LogWriter $ fmap (fmap f) w

-- instance Applicative LogWriter where
--   pure a = LogWriter $ (pure . pure) a

--   (LogWriter wmf) <*> (LogWriter wma) = LogWriter $ do
--     (mf, logs) <- W.listen wmf
--     (ma, logs') <- W.listen wma
--     W.writer (liftA2 ($) mf ma, logs <> logs')

-- instance Monad LogWriter where
--   LogWriter wma >>= f = LogWriter $ do
--     (ma, logs) <- W.listen wma
--     case ma of
--       Nothing -> W.writer (Nothing, logs)
--       Just a -> do
--         W.listen

-- -- | Extract a potential result and logs.
-- runWriter :: LogWriter a -> (Maybe a, LogStack)
-- runWriter (LogWriter logs) = W.runWriter logs

-- -- | Extract the current logs.
-- getLogStack :: LogWriter a -> LogStack
-- getLogStack (LogWriter logs) = W.execWriter logs

-- -- | Add a log and result to the log writer
-- writer :: (a, Log) -> LogWriter a
-- writer (a, l) = LogWriter (W.writer (Just a, LogStack [l]))

-- tell :: Log -> LogWriter ()
-- tell l = LogWriter (Just <$> W.tell (LogStack [l]))
