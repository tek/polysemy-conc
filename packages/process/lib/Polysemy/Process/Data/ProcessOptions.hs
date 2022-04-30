-- |Data type containing options for processes.
module Polysemy.Process.Data.ProcessOptions where

import qualified Polysemy.Process.Data.ProcessKill as ProcessKill
import Polysemy.Process.Data.ProcessKill (ProcessKill)

-- |Controls the behaviour of 'Polysemy.Process.Process' interpreters.
data ProcessOptions =
  ProcessOptions {
    -- |Whether to discard output chunks if the queue is full.
    discard :: Bool,
    -- |Maximum number of chunks allowed to be queued for each of the three standard pipes.
    qsize :: Int,
    -- |What to do if the process hasn't terminated when exiting the scope.
    kill :: ProcessKill
  }
  deriving stock (Eq, Show)

instance Default ProcessOptions where
  def =
    ProcessOptions True 1024 ProcessKill.KillImmediately
