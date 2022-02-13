{-# options_haddock prune #-}

-- |Description: SystemProcessError, Internal
module Polysemy.Process.Data.SystemProcessError where

-- |Signal error for 'Polysemy.Process.SystemProcess'.
data SystemProcessError =
  -- |The process terminated.
  Terminated Text
  deriving stock (Eq, Show)
