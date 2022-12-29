-- |Description: Polysemy Effects for Concurrency
module Polysemy.Conc (
  -- * Introduction
  -- $intro

  -- * Queues
  -- $queue
  Queue,
  QueueResult,

  -- ** Interpreters
  interpretQueueTBM,
  interpretQueueTB,
  interpretQueueListReadOnlyAtomic,
  interpretQueueListReadOnlyAtomicWith,
  interpretQueueListReadOnlyState,
  interpretQueueListReadOnlyStateWith,

  -- ** Combinators
  resultToMaybe,
  loop,
  loopOr,

  -- * MVars
  -- $mvar
  Sync,
  SyncRead,
  ScopedSync,

  -- ** Interpreters
  interpretSync,
  interpretSyncAs,
  withSync,
  interpretScopedSync,
  interpretScopedSyncAs,
  syncRead,

  -- * Lock
  Lock,
  lock,
  lockOr,
  lockOrSkip,
  lockOrSkip_,

  -- ** Interpreters
  interpretLockReentrant,
  interpretLockPermissive,

  -- * Semaphores
  Semaphore,

  -- ** Interpreters
  interpretSemaphoreQ,
  interpretSemaphoreT,

  -- * Gate
  Gate,
  Gates,

  -- ** Interpreters
  interpretGates,
  interpretGate,

  -- * Racing
  -- $race
  Race,
  race,
  race_,
  timeout,
  timeout_,
  timeoutAs,
  timeoutAs_,
  timeoutU,
  timeoutMaybe,
  timeoutStop,
  retrying,
  retryingWithError,

  -- ** Interpreters
  interpretRace,

  -- * Signal Handling
  -- $signal
  Interrupt,

  -- ** Interpreters
  interpretInterrupt,
  interpretInterruptOnce,
  interpretInterruptNull,

  -- * Event Channels
  Events,
  Consume,
  publish,
  consume,
  subscribe,
  subscribeGated,
  subscribeAsync,
  subscribeWhile,
  subscribeWhileGated,
  subscribeWhileAsync,
  subscribeLoop,
  subscribeLoopGated,
  subscribeLoopAsync,
  subscribeFind,
  subscribeFirstJust,
  subscribeElem,
  consumeWhile,
  consumeLoop,
  consumeFind,
  consumeFirstJust,
  consumeElem,
  EventConsumer,

  -- ** Interpreters
  interpretEventsChan,

  -- * Exceptions
  Critical,

  -- ** Interpreters
  interpretCritical,
  interpretCriticalNull,

  -- * Masking
  Mask,
  UninterruptibleMask,
  mask,
  uninterruptibleMask,
  restore,
  Restoration,

  -- * Interpreters
  interpretMaskFinal,
  interpretUninterruptibleMaskFinal,
  interpretMaskPure,
  interpretUninterruptibleMaskPure,

  -- * Scoped Effects
  Scoped,
  Scoped_,
  scoped,
  scoped_,
  rescope,

  -- ** Interpreters
  interpretScoped,
  interpretScopedH,
  interpretScopedH',
  interpretScopedAs,
  interpretScopedWith,
  interpretScopedWithH,
  interpretScopedWith_,
  runScoped,
  runScopedAs,
  interpretScopedResumable,
  interpretScopedResumableH,
  interpretScopedResumable_,
  interpretScopedResumableWith,
  interpretScopedResumableWithH,
  interpretScopedResumableWith_,
  interpretResumableScoped,
  interpretResumableScopedH,
  interpretResumableScoped_,
  interpretResumableScopedWith,
  interpretResumableScopedWithH,
  interpretResumableScopedWith_,
  interpretScopedR,
  interpretScopedRH,
  interpretScopedR_,
  interpretScopedRWith,
  interpretScopedRWithH,
  interpretScopedRWith_,

  -- * Monitoring
  Monitor,
  monitor,
  withMonitor,
  restart,
  Restart,
  RestartingMonitor,
  ScopedMonitor,

  -- ** Interpreters
  interpretMonitorRestart,
  interpretMonitorPure,
  monitorClockSkew,
  ClockSkewConfig (ClockSkewConfig),
  clockSkewConfig,

  -- * Other Combinators
  ConcStack,
  runConc,
  interpretAtomic,
  withAsyncBlock,
  withAsync,
  withAsync_,
  scheduleAsync,
  scheduleAsyncIO,
  withAsyncGated,
  withAsyncGated_,
) where

import Prelude hiding (
  Scoped,
  Scoped_,
  interpretScoped,
  interpretScopedAs,
  interpretScopedH,
  interpretScopedH',
  interpretScopedWith,
  interpretScopedWithH,
  interpretScopedWith_,
  rescope,
  runScoped,
  runScopedAs,
  scoped,
  scoped_,
  )

import Polysemy.Conc.Async (
  scheduleAsync,
  scheduleAsyncIO,
  withAsync,
  withAsyncBlock,
  withAsyncGated,
  withAsyncGated_,
  withAsync_,
  )
import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Data.QueueResult (QueueResult)
import Polysemy.Conc.Effect.Critical (Critical)
import Polysemy.Conc.Effect.Events (Consume, Events, consume, publish, subscribe)
import Polysemy.Conc.Effect.Gate (Gate, Gates)
import Polysemy.Conc.Effect.Interrupt (Interrupt)
import Polysemy.Conc.Effect.Lock (Lock, lock, lockOr, lockOrSkip, lockOrSkip_)
import Polysemy.Conc.Effect.Mask (Mask, Restoration, UninterruptibleMask, mask, restore, uninterruptibleMask)
import Polysemy.Conc.Effect.Monitor (Monitor, Restart, RestartingMonitor, ScopedMonitor, monitor, restart, withMonitor)
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race, race, timeout)
import Polysemy.Conc.Effect.Scoped (Scoped, Scoped_, rescope, scoped, scoped_)
import Polysemy.Conc.Effect.Semaphore (Semaphore)
import Polysemy.Conc.Effect.Sync (ScopedSync, Sync)
import Polysemy.Conc.Effect.SyncRead (SyncRead)
import Polysemy.Conc.Events (
  consumeElem,
  consumeFind,
  consumeFirstJust,
  consumeLoop,
  consumeWhile,
  subscribeAsync,
  subscribeElem,
  subscribeFind,
  subscribeFirstJust,
  subscribeGated,
  subscribeLoop,
  subscribeLoopAsync,
  subscribeLoopGated,
  subscribeWhile,
  subscribeWhileAsync,
  subscribeWhileGated,
  )
import Polysemy.Conc.Interpreter.Critical (interpretCritical, interpretCriticalNull)
import Polysemy.Conc.Interpreter.Events (EventConsumer, interpretEventsChan)
import Polysemy.Conc.Interpreter.Gate (interpretGate, interpretGates)
import Polysemy.Conc.Interpreter.Interrupt (interpretInterrupt, interpretInterruptNull, interpretInterruptOnce)
import Polysemy.Conc.Interpreter.Lock (interpretLockPermissive, interpretLockReentrant)
import Polysemy.Conc.Interpreter.Mask (
  interpretMaskFinal,
  interpretMaskPure,
  interpretUninterruptibleMaskFinal,
  interpretUninterruptibleMaskPure,
  )
import Polysemy.Conc.Interpreter.Monitor (interpretMonitorPure, interpretMonitorRestart)
import Polysemy.Conc.Interpreter.Queue.Pure (
  interpretQueueListReadOnlyAtomic,
  interpretQueueListReadOnlyAtomicWith,
  interpretQueueListReadOnlyState,
  interpretQueueListReadOnlyStateWith,
  )
import Polysemy.Conc.Interpreter.Queue.TB (interpretQueueTB)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBM)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Scoped (
  interpretResumableScoped,
  interpretResumableScopedH,
  interpretResumableScopedWith,
  interpretResumableScopedWithH,
  interpretResumableScopedWith_,
  interpretResumableScoped_,
  interpretScoped,
  interpretScopedAs,
  interpretScopedH,
  interpretScopedH',
  interpretScopedR,
  interpretScopedRH,
  interpretScopedRWith,
  interpretScopedRWithH,
  interpretScopedRWith_,
  interpretScopedR_,
  interpretScopedResumable,
  interpretScopedResumableH,
  interpretScopedResumableWith,
  interpretScopedResumableWithH,
  interpretScopedResumableWith_,
  interpretScopedResumable_,
  interpretScopedWith,
  interpretScopedWithH,
  interpretScopedWith_,
  runScoped,
  runScopedAs,
  )
import Polysemy.Conc.Interpreter.Semaphore (interpretSemaphoreQ, interpretSemaphoreT)
import Polysemy.Conc.Interpreter.Stack (ConcStack, runConc)
import Polysemy.Conc.Interpreter.Sync (interpretScopedSync, interpretScopedSyncAs, interpretSync, interpretSyncAs)
import Polysemy.Conc.Interpreter.SyncRead (syncRead)
import Polysemy.Conc.Monitor (ClockSkewConfig (ClockSkewConfig), clockSkewConfig, monitorClockSkew)
import Polysemy.Conc.Queue (loop, loopOr)
import Polysemy.Conc.Queue.Result (resultToMaybe)
import Polysemy.Conc.Race (race_, timeoutAs, timeoutAs_, timeoutMaybe, timeoutStop, timeoutU, timeout_)
import Polysemy.Conc.Retry (retrying, retryingWithError)
import Polysemy.Conc.Sync (withSync)

-- $intro
-- This library provides an assortment of tools for concurrency-related tasks:
--
-- - [STM queues](#queue)
-- - [MVars](#mvar)
-- - [Racing](#race)
-- - [Signal handling](#signal)
-- - [Masking](#mask)

-- $queue
-- #queue#

-- $mvar
-- #mvar#
-- An 'Control.Concurrent.MVar' is abstracted as 'Sync' since it can be used to synchronize threads.

-- $race
-- #race#
-- Racing works like this:
--
-- @
-- prog =
--  Polysemy.Conc.race (httpRequest "hackage.haskell.org") (readFile "\/path\/to\/file") >>= \\case
--    Left _ -> putStrLn "hackage was faster"
--    Right _ -> putStrLn "file was faster"
-- @
--
-- When the first thunk finishes, the other will be killed.

-- $signal
-- #signal#

-- $mask
-- #mask#
-- The two effects 'Mask' and 'UninterruptibleMask' use the 'Polysemy.Scoped' pattern, which allow an effect
-- with resources to be constrained to a region of code.
-- The actual effect is 'Polysemy.Conc.Effect.Mask.RestoreMask', with `mask` and 'uninterruptibleMask' only specializing
-- 'Polysemy.Conc.Effect.scoped' to the appropriate resource type.
--
-- Usage is straightforward:
--
-- @
-- prog :: Member Mask r
-- prog =
--  mask do
--    doMaskedThing
--    restore do
--      doUnmaskedThing
--    doMaskedThing
-- @
--
-- The @resource@ parameter stays polymorphic; it is used to connect the resource in the interpreter to the callsite.
