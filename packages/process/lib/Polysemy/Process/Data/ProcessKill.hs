-- |Data type indicating whether to kill a process after exiting its scope.
module Polysemy.Process.Data.ProcessKill where

import Polysemy.Time (NanoSeconds)

-- |Indicate whether to kill a process after exiting the scope in which it was used, if it hasn't terminated.
data ProcessKill =
  -- |Wait for the specified interval, then kill.
  KillAfter NanoSeconds
  |
  -- |Kill immediately.
  KillImmediately
  |
  -- |Wait indefinitely for the process to terminate.
  KillNever
  deriving stock (Eq, Show)
