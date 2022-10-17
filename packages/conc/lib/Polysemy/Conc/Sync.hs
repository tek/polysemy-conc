{-# options_haddock prune #-}

-- |Description: Sync Combinators
module Polysemy.Conc.Sync (
  module Polysemy.Conc.Sync,
  module Polysemy.Conc.Effect.Sync,
) where

import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)

import Polysemy.Conc.Effect.Mask (Mask, mask, restore)
import Polysemy.Conc.Effect.Scoped (scoped_)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync

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
--
-- This avoids a dependency on @'Embed' 'IO'@ in application logic while still allowing the variable to be scoped.
withSync ::
  ∀ d r .
  Member (ScopedSync d) r =>
  InterpreterFor (Sync d) r
withSync =
  scoped_

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
{-# inline lock #-}

-- |Remove the content of the 'Sync' variable if it is present.
clear ::
  ∀ a r .
  Member (Sync a) r =>
  Sem r ()
clear =
  void (takeTry @a)
{-# inline clear #-}

-- |Modify a 'Sync' variable with async exceptions masked for the 'Sync' operations, but not the action.
-- Allows a value to be returned.
-- Equivalent to 'Control.Concurrent.MVar.modifyMVar'.
modify ::
  ∀ a b r .
  Members [Sync a, Mask, Resource] r =>
  (a -> Sem r (a, b)) ->
  Sem r b
modify m =
  mask do
    a <- takeBlock
    (a', b) <- onException (restore (raise (m a))) (putBlock a)
    b <$ putBlock a'
{-# inline modify #-}

-- |Modify a 'Sync' variable with async exceptions masked for the 'Sync' operations, but not the action.
-- Does not allow a value to be returned.
-- Equivalent to 'Control.Concurrent.MVar.modifyMVar_'.
modify_ ::
  ∀ a r .
  Members [Sync a, Mask, Resource] r =>
  (a -> Sem r a) ->
  Sem r ()
modify_ m =
  mask do
    a <- takeBlock
    a' <- onException (restore (raise (m a))) (putBlock a)
    putBlock a'
{-# inline modify_ #-}

-- |Modify a 'Sync' variable with async exceptions masked for the entire procedure.
-- Allows a value to be returned.
-- Equivalent to 'Control.Concurrent.MVar.modifyMVarMasked'.
modifyMasked ::
  ∀ a b r .
  Members [Sync a, Mask, Resource] r =>
  (a -> Sem r (a, b)) ->
  Sem r b
modifyMasked m =
  mask do
    a <- takeBlock
    (a', b) <- onException (raise (m a)) (putBlock a)
    b <$ putBlock a'
{-# inline modifyMasked #-}

-- |Modify a 'Sync' variable with async exceptions masked for the entire procedure.
-- Does not allow a value to be returned.
-- Equivalent to 'Control.Concurrent.MVar.modifyMVarMasked_'.
modifyMasked_ ::
  ∀ a r .
  Members [Sync a, Mask, Resource] r =>
  (a -> Sem r a) ->
  Sem r ()
modifyMasked_ m =
  mask do
    a <- takeBlock
    a' <- onException (raise (m a)) (putBlock a)
    putBlock a'
{-# inline modifyMasked_ #-}

-- |Run an action with the current value of the 'Sync' variable with async exceptions masked for the 'Sync' operations,
-- but not the action.
-- Equivalent to 'Control.Concurrent.MVar.withMVar'.
use ::
  ∀ a b r .
  Members [Sync a, Mask, Resource] r =>
  (a -> Sem r b) ->
  Sem r b
use m =
  mask do
    a <- takeBlock
    finally (restore (raise (m a))) (putBlock a)
{-# inline use #-}

-- |Run an action with the current value of the 'Sync' variable with async exceptions masked for the entire procedure.
-- Equivalent to 'Control.Concurrent.MVar.withMVarMasked'.
useMasked ::
  ∀ a b r .
  Members [Sync a, Mask, Resource] r =>
  (a -> Sem r b) ->
  Sem r b
useMasked m =
  mask do
    a <- takeBlock
    finally (raise (m a)) (putBlock a)
{-# inline useMasked #-}
