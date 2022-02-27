{-# options_haddock prune #-}

-- |Description: API and combinators for 'SyncRead'.
module Polysemy.Conc.SyncRead (
  module Polysemy.Conc.SyncRead,
  module Polysemy.Conc.Effect.SyncRead,
) where

import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)

import qualified Polysemy.Conc.Effect.SyncRead as SyncRead
import Polysemy.Conc.Effect.SyncRead (SyncRead, block, empty, try, wait)

-- |Run an action repeatedly until the 'SyncRead' variable is available.
whileEmpty ::
  ∀ a r .
  Member (SyncRead a) r =>
  Sem r () ->
  Sem r ()
whileEmpty action =
  spin
  where
    spin = do
      action
      whenM (not <$> SyncRead.empty @a) spin

-- |Run an action repeatedly until the 'SyncRead' variable is available, waiting for the specified time between executions.
whileEmptyInterval ::
  ∀ a u t d r .
  TimeUnit u =>
  Members [Time t d, SyncRead a] r =>
  u ->
  Sem r () ->
  Sem r ()
whileEmptyInterval interval action =
  spin
  where
    spin = do
      action
      whenM (not <$> SyncRead.empty @a) (Time.sleep @t @d interval *> spin)
