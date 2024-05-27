-- | Pid data type, Internal
module Polysemy.Process.Data.Pid where

-- | A process ID.
newtype Pid =
  Pid { unPid :: Int }
  deriving stock (Eq, Show, Read)
  deriving newtype (Num, Real, Enum, Integral, Ord)
