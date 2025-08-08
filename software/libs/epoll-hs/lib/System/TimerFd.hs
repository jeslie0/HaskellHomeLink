module System.TimerFd (
  TimerFd,
  timerFdToFd,
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

import Control.Exception (IOException, throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (..))
import Data.Foldable (Foldable (foldl'))
import Foreign (
  ForeignPtr,
  IntPtr (IntPtr),
  Storable (..),
  alloca,
  intPtrToPtr,
  newForeignPtr,
  nullPtr,
  ptrToIntPtr,
  withForeignPtr,
  (.|.),
 )
import Foreign.C (CInt, errnoToIOError, getErrno)
import Foreign.Marshal.Utils (with)
import System.Posix.Types (Fd (Fd))
import System.TimerFd.Internal (
  ITimerSpec,
  c_CLOCK_BOOTTIME,
  c_CLOCK_BOOTTIME_ALARM,
  c_CLOCK_MONOTONIC,
  c_CLOCK_REALTIME,
  c_CLOCK_REALTIME_ALARM,
  c_TFD_CLOEXEC,
  c_TFD_NONBLOCK,
  c_TFD_TIMER_ABSTIME,
  c_TFD_TIMER_CANCEL_ON_SET,
  c_timerfd_close,
  c_timerfd_create,
  c_timerfd_gettime,
  c_timerfd_settime,
 )

newtype TimerFd = TimerFd (ForeignPtr Fd)

timerFdToFd :: TimerFd -> IO Fd
timerFdToFd (TimerFd frnPtr) =
  withForeignPtr frnPtr $ pure . fromIntegral . ptrToIntPtr

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
  Foldable f => ClockId -> f ClockFlags -> IO (Either IOException TimerFd)
createTimerFd clockId flags = do
  tfd <-
    c_timerfd_create
      (clockIdToCInt clockId)
      (foldl' (\acc prev -> clockFlagsToCInt prev .|. acc) 0 flags)
  if tfd < 0
    then do
      n <- getErrno
      pure . Left $ errnoToIOError "createTimerFd" n Nothing Nothing
    else do
      Right . TimerFd
        <$> newForeignPtr c_timerfd_close (intPtrToPtr . IntPtr . fromIntegral $ tfd)

createTimerFd' ::
  Foldable f => ClockId -> f ClockFlags -> ExceptT IOException IO TimerFd
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
  -> IO (Either IOException ())
setTime timerFd flags itimerspec = do
  Fd tfd <- timerFdToFd timerFd
  with itimerspec $ \itimerPtr -> do
    val <-
      c_timerfd_settime
        tfd
        (foldl' (\acc cur -> setTimerFdFlagsToCInt cur .|. acc) 0 flags)
        itimerPtr
        nullPtr
    if val < 0
      then do
        n <- getErrno
        pure . Left $ errnoToIOError "TimerFd.setTime" n Nothing Nothing
      else pure $ Right ()

setTime' ::
  Foldable f =>
  TimerFd
  -> f SetTimerFdFlags
  -> ITimerSpec
  -> ExceptT IOException IO ()
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

getTime :: TimerFd -> IO (Either IOException ITimerSpec)
getTime timerFd = do
  Fd tfd <- timerFdToFd timerFd
  alloca $ \ptr -> do
    val <- c_timerfd_gettime tfd ptr
    if val < 0
      then do
        n <- getErrno
        pure . Left $ errnoToIOError "TimerFd.getTime" n Nothing Nothing
      else
        Right <$> peek ptr

getTime' :: TimerFd -> ExceptT IOException IO ITimerSpec
getTime' =
  ExceptT . getTime

getTime_ :: TimerFd -> IO ITimerSpec
getTime_ =
  either throwIO pure <=< getTime
