{-# options_haddock prune #-}

-- |Description: SystemProcessError, Internal
module Polysemy.Process.Data.SystemProcessError where

-- |Error for 'Polysemy.Process.SystemProcess'.
data SystemProcessError =
  -- |The process terminated.
  Terminated Text
  |
  -- |Stdio was requested, but the process was started without pipes.
  NoPipes
  deriving stock (Eq, Show)

-- |Error for the scope of 'Polysemy.Process.SystemProcess'.
data SystemProcessScopeError =
  -- |The process couldn't start.
  StartFailed Text
  deriving stock (Eq, Show)
