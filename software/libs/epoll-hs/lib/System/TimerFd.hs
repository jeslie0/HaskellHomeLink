module System.TimerFd (TimerFd, ClockId (..), createTimerFd, timerFdToFd, setTime, getTime, SetTimerFdFlags(..), ClockFlags(..)) where

import Data.Foldable (Foldable (foldl'))
import Foreign (
  ForeignPtr,
  IntPtr (IntPtr),
  intPtrToPtr,
  newForeignPtr,
  nullPtr,
  ptrToIntPtr,
  withForeignPtr,
  (.|.), alloca, Storable (..),
 )
import Foreign.C (CInt, getErrno)
import Foreign.C.Error (Errno)
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
  c_timerfd_settime, c_timerfd_gettime,
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
  Foldable f => ClockId -> f ClockFlags -> IO (Either Errno TimerFd)
createTimerFd clockId flags = do
  tfd <-
    c_timerfd_create
      (clockIdToCInt clockId)
      (foldl' (\acc prev -> clockFlagsToCInt prev .|. acc) 0 flags)
  if tfd < 0
    then Left <$> getErrno
    else do
      Right . TimerFd
        <$> newForeignPtr c_timerfd_close (intPtrToPtr . IntPtr . fromIntegral $ tfd)

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
  Foldable f => TimerFd -> f SetTimerFdFlags -> ITimerSpec -> IO (Either Errno ())
setTime timerFd flags itimerspec = do
  Fd tfd <- timerFdToFd timerFd
  with itimerspec $ \itimerPtr -> do
    val <-
      c_timerfd_settime
        tfd
        (foldl' (\acc cur -> setTimerFdFlagsToCInt cur .|. acc) 0 flags)
        itimerPtr
        nullPtr
    if val < 0 then Left <$> getErrno else pure $ Right ()

getTime :: TimerFd -> IO ITimerSpec
getTime timerFd = do
  Fd tfd <- timerFdToFd timerFd
  alloca $ \ptr -> do
    n <- c_timerfd_gettime tfd ptr
    peek ptr
