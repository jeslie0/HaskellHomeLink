{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIO.Error.Syscall (SyscallError (..)) where

import HIO.Error.Error (Error (..))
import HIO.Error.ErrorCategory (ErrorCategory (..))

data SyscallError
  = Read
  | Write
  | Open
  | Close
  | Stat
  | EpollCreate
  | EpollCreate1
  | EpollWait
  | EpollCtl
  | EventFd
  | TimerFdCreate
  | TimerFdSetTime
  | TimerFdGetTime
  deriving (Eq, Show, Ord, Enum)

data SystemCategory

instance ErrorCategory SystemCategory where
  getErrorCategoryName = "system"

instance Error SyscallError where
  type ECat SyscallError = SystemCategory

  getErrorMessage err =
    case err of
      Read -> "read failed"
      Write -> "write failed"
      Open -> "open failed"
      Close -> "close failed"
      Stat -> "stat failed"
      EpollCreate -> "epoll_create failed"
      EpollCreate1 -> "epoll_create1 failed"
      EpollWait -> "epoll_wait failed"
      EpollCtl -> "epoll_ctl failed"
      EventFd -> "eventfd failed"
      TimerFdCreate -> "timerfd_create failed"
      TimerFdSetTime -> "timerfd_settime failed"
      TimerFdGetTime -> "timergd_gettime failed"
