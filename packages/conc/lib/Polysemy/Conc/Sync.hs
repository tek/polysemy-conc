{-# options_haddock prune #-}

-- |Description: Sync Combinators
module Polysemy.Conc.Sync (
  module Polysemy.Conc.Sync,
  module Polysemy.Conc.Effect.Sync,
) where

import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)

import Polysemy.Conc.Effect.Scoped (scoped)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (
  ScopedSync,
  Sync,
  SyncResources,
  block,
  empty,
  putBlock,
  putTry,
  putWait,
  takeBlock,
  takeTry,
  takeWait,
  try,
  wait,
  )

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
-- This avoids a dependency on @'Embed' 'IO'@ in application logic while still allowing the variable to be scoped.
withSync ::
  ∀ d res r .
  Member (ScopedSync res d) r =>
  InterpreterFor (Sync d) r
withSync =
  scoped @(SyncResources res)

-- |Run the action @ma@ with an exclusive lock (mutex).
-- When multiple threads call the action concurrently, only one is allowed to execute it at a time.
-- The value @l@ is used to disambiguate the 'Sync' from other uses of the combinator.
-- You can pass in something like @Proxy @"db-write"@.
--
-- /Note:/ The 'Sync' must be interpreted with an initially full @MVar@, e.g. using 'Polysemy.Conc.interpretSyncAs'.
lock ::
  ∀ l r a .
  Members [Sync l, Resource] r =>
  l ->
  Sem r a ->
  Sem r a
lock l ma =
  finally (takeBlock @l *> ma) (putTry l)

-- |Remove the content of the 'Sync' variable if it is present.
clear ::
  ∀ a r .
  Member (Sync a) r =>
  Sem r ()
clear =
  void (takeTry @a)
