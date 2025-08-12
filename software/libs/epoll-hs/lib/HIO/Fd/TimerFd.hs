module HIO.Fd.TimerFd (
  TimerFd (..),
  TimeSpec (..),
  ITimerSpec (..),
  ClockId (..),
  createTimerFd,
  createTimerFd',
  createTimerFd_,
  setTime,
  setTime',
  setTime_,
  getTime,
  getTime',
  getTime_,
  SetTimerFdFlags (..),
  ClockFlags (..),
) where

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (..))
import Data.Foldable (Foldable (foldl'))
import Foreign (
  Storable (..),
  alloca,
  nullPtr,
  (.|.),
 )
import Foreign.C (CInt)
import Foreign.Marshal.Utils (with)
import HIO.Error.ErrorStack (ErrorStack, pushErrno)
import HIO.Error.Syscalls qualified as ESys
import HIO.Fd.IsFd (IsFd (..))
import HIO.Fd.TimerFd.Internal (
  ITimerSpec (..),
  c_CLOCK_BOOTTIME,
  c_CLOCK_BOOTTIME_ALARM,
  c_CLOCK_MONOTONIC,
  c_CLOCK_REALTIME,
  c_CLOCK_REALTIME_ALARM,
  c_TFD_CLOEXEC,
  c_TFD_NONBLOCK,
  c_TFD_TIMER_ABSTIME,
  c_TFD_TIMER_CANCEL_ON_SET,
  c_timerfd_create,
  c_timerfd_gettime,
  c_timerfd_settime,
 )
import System.Clock (TimeSpec (..))
import System.Posix.Types (Fd (Fd))

newtype TimerFd = TimerFd Fd

instance IsFd TimerFd where
  toFd (TimerFd fd) = fd

  fromFd = TimerFd

data ClockId
  = Realtime
  | Monotonic
  | Bootttime
  | RealtimeAlarm
  | BoottimeAlarm
  deriving (Eq, Show)

clockIdToCInt :: ClockId -> CInt
clockIdToCInt clockId =
  case clockId of
    Realtime -> c_CLOCK_REALTIME
    Monotonic -> c_CLOCK_MONOTONIC
    Bootttime -> c_CLOCK_BOOTTIME
    RealtimeAlarm -> c_CLOCK_REALTIME_ALARM
    BoottimeAlarm -> c_CLOCK_BOOTTIME_ALARM

data ClockFlags = NonBlocking | CloseOnExec deriving (Eq, Show)

clockFlagsToCInt :: ClockFlags -> CInt
clockFlagsToCInt flags =
  case flags of
    NonBlocking -> c_TFD_NONBLOCK
    CloseOnExec -> c_TFD_CLOEXEC

createTimerFd ::
  Foldable f => ClockId -> f ClockFlags -> IO (Either ErrorStack TimerFd)
createTimerFd clockId flags = do
  tfd <-
    c_timerfd_create
      (clockIdToCInt clockId)
      (foldl' (\acc prev -> clockFlagsToCInt prev .|. acc) 0 flags)
  if tfd < 0
    then do
      Left <$> pushErrno ESys.TimerFdCreate
    else do
      pure . Right . TimerFd $ fromIntegral tfd

createTimerFd' ::
  Foldable f => ClockId -> f ClockFlags -> ExceptT ErrorStack IO TimerFd
createTimerFd' clockId =
  ExceptT . createTimerFd clockId

createTimerFd_ ::
  Foldable f => ClockId -> f ClockFlags -> IO TimerFd
createTimerFd_ clockId =
  either throwIO pure <=< createTimerFd clockId

data SetTimerFdFlags
  = AbsoluteTime
  | RelativeTime
  | CancelOnSet

setTimerFdFlagsToCInt :: SetTimerFdFlags -> CInt
setTimerFdFlagsToCInt flags =
  case flags of
    AbsoluteTime -> c_TFD_TIMER_ABSTIME
    RelativeTime -> 0
    CancelOnSet -> c_TFD_TIMER_CANCEL_ON_SET

setTime ::
  Foldable f =>
  TimerFd
  -> f SetTimerFdFlags
  -> ITimerSpec
  -> IO (Either ErrorStack ())
setTime timerFd flags itimerspec = do
  let Fd tfd = toFd timerFd
  with itimerspec $ \itimerPtr -> do
    val <-
      c_timerfd_settime
        tfd
        (foldl' (\acc cur -> setTimerFdFlagsToCInt cur .|. acc) 0 flags)
        itimerPtr
        nullPtr
    if val < 0
      then do
        Left <$> pushErrno ESys.TimerFdSetTime
      else pure $ Right ()

setTime' ::
  Foldable f =>
  TimerFd
  -> f SetTimerFdFlags
  -> ITimerSpec
  -> ExceptT ErrorStack IO ()
setTime' timerfd flags =
  ExceptT . setTime timerfd flags

setTime_ ::
  Foldable f =>
  TimerFd
  -> f SetTimerFdFlags
  -> ITimerSpec
  -> IO ()
setTime_ timerfd flags =
  either throwIO pure <=< setTime timerfd flags

getTime :: TimerFd -> IO (Either ErrorStack ITimerSpec)
getTime timerFd = do
  let Fd tfd = toFd timerFd
  alloca $ \ptr -> do
    val <- c_timerfd_gettime tfd ptr
    if val < 0
      then do
        Left <$> pushErrno ESys.TimerFdGetTime
      else
        Right <$> peek ptr

getTime' :: TimerFd -> ExceptT ErrorStack IO ITimerSpec
getTime' =
  ExceptT . getTime

getTime_ :: TimerFd -> IO ITimerSpec
getTime_ =
  either throwIO pure <=< getTime
