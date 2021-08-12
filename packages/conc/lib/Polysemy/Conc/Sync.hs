-- |Description: Sync Combinators
module Polysemy.Conc.Sync where

import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync, SyncResources)

-- |Run an action repeatedly until the 'Sync' variable is available.
whileEmpty ::
  ∀ a r .
  Member (Sync a) r =>
  Sem r () ->
  Sem r ()
whileEmpty action =
  spin
  where
    spin = do
      action
      whenM (not <$> Sync.empty @a) spin

-- |Run an action repeatedly until the 'Sync' variable is available, waiting for the specified time between executions.
whileEmptyInterval ::
  ∀ a u t d r .
  TimeUnit u =>
  Members [Time t d, Sync a] r =>
  u ->
  Sem r () ->
  Sem r ()
whileEmptyInterval interval action =
  spin
  where
    spin = do
      action
      whenM (not <$> Sync.empty @a) (Time.sleep @t @d interval *> spin)

-- |Run an action with a locally scoped 'Sync' variable.
withSync ::
  ∀ d res r .
  Member (Scoped (SyncResources res) (Sync d)) r =>
  InterpreterFor (Sync d) r
withSync =
  scoped @(SyncResources res)
