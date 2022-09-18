{-# options_haddock prune #-}

-- |Lock interpreters, Internal
module Polysemy.Conc.Interpreter.Lock where

import Control.Concurrent (ThreadId, myThreadId)

import Polysemy.Conc.Effect.Lock (Lock (Lock, LockOr))
import Polysemy.Conc.Effect.Mask (Mask, mask, restore)
import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync (putTry, takeBlock, takeTry)
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Interpreter.Sync (interpretSyncAs)

currentThread ::
  Member (Embed IO) r =>
  Sem r ThreadId
currentThread =
  embed myThreadId

-- |Interpret 'Lock' by executing all actions unconditionally.
interpretLockPermissive ::
  InterpreterFor Lock r
interpretLockPermissive =
  interpretH \case
    Lock ma ->
      runTSimple ma
    LockOr _ ma ->
      runTSimple ma
{-# inline interpretLockPermissive #-}

lockOnDifferentThread ::
  ∀ mres f m r a .
  Members [Sync (), Resource, Race, Mask mres, Embed IO] r =>
  ThreadId ->
  m a ->
  (Sem (Lock : r) (f a) -> Sem (Lock : r) (f a)) ->
  Sem (WithTactics Lock f m r) (f a)
lockOnDifferentThread lockThread maI f = do
  thread <- currentThread
  ma <- runT maI
  raise $ interpretLockReentrantEntered @mres thread do
    if thread == lockThread
    then ma
    else f ma
{-# inline lockOnDifferentThread #-}

enter ::
  ∀ mres f m r a .
  Members [Sync (), Resource, Race, Mask mres, Embed IO] r =>
  m a ->
  (Sem (Lock : r) (f a) -> Sem (Lock : r) (f a)) ->
  Sem (WithTactics Lock f m r) (f a)
enter maI f = do
  thread <- currentThread
  ma <- runT maI
  raise $ interpretLockReentrantEntered @mres thread do
    f ma
{-# inline enter #-}

lockWait ::
  ∀ mres r a .
  Members [Sync (), Resource, Mask mres] r =>
  Sem r a ->
  Sem r a
lockWait ma =
  mask @mres do
    Sync.takeBlock @()
    finally (restore (raise ma)) (Sync.putTry ())
{-# inline lockWait #-}

lockAlt ::
  ∀ mres r a .
  Members [Sync (), Resource, Mask mres] r =>
  Sem r a ->
  Sem r a ->
  Sem r a
lockAlt alt ma =
  mask @mres do
    Sync.takeTry >>= \case
      Just () ->
        finally (restore (raise ma)) (Sync.putTry ())
      Nothing ->
        restore (raise alt)
{-# inline lockAlt #-}

-- |Subinterpreter for 'interpretLockReentrant' that checks whether the current thread is equal to the lock-acquiring
-- thread to allow reentry into the lock.
interpretLockReentrantEntered ::
  ∀ mres r .
  Members [Sync (), Resource, Race, Mask mres, Embed IO] r =>
  ThreadId ->
  InterpreterFor Lock r
interpretLockReentrantEntered lockThread =
  interpretH \case
    Lock maI ->
      lockOnDifferentThread @mres lockThread maI (lockWait @mres)
    LockOr altI maI -> do
      alt <- runT altI
      lockOnDifferentThread @mres lockThread maI (lockAlt @mres alt)
{-# inline interpretLockReentrantEntered #-}

-- |Interpret 'Lock' as a reentrant lock, allowing nested calls to 'Polysemy.Conc.lock' unless called from a different
-- thread (as in, @async@ was called in a higher-order action passed to 'Polysemy.Conc.lock'.)
interpretLockReentrant ::
  ∀ mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor Lock r
interpretLockReentrant =
  interpretSyncAs () .
  reinterpretH \case
    Lock maI ->
      enter @mres maI (lockWait @mres)
    LockOr altI maI -> do
      alt <- runT altI
      enter @mres maI (lockAlt @mres alt)
{-# inline interpretLockReentrant #-}
