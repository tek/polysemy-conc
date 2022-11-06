{-# options_haddock prune #-}

-- |The effect 'SyncRead' is equivalent to 'Polysemy.Conc.Sync' without the write actions.
module Polysemy.Conc.Effect.SyncRead where

import Polysemy.Time (TimeUnit)
import Prelude hiding (empty)

-- |An interface to a shared variable ('MVar') that can only be read.
data SyncRead (d :: Type) :: Effect where
  -- |Read the variable, waiting until a value is available.
  Block :: SyncRead d m d
  -- |Read the variable, waiting until a value is available or the timeout has expired.
  Wait :: TimeUnit u => u -> SyncRead d m (Maybe d)
  -- |Read the variable, returning 'Nothing' immmediately if no value was available.
  Try :: SyncRead d m (Maybe d)
  -- |Indicate whether the variable is empty.
  Empty :: SyncRead d m Bool

makeSem ''SyncRead
