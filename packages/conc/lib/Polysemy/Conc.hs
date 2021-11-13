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
  ScopedSync,

  -- ** Interpreters
  interpretSync,
  interpretSyncAs,
  withSync,
  lock,
  interpretScopedSync,
  interpretScopedSyncAs,

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

  -- * Event Channels
  Events,
  publish,
  consume,
  subscribe,
  subscribeWhile,
  subscribeLoop,
  EventResource,
  EventChan,
  ChanEvents,
  EventConsumer,
  ChanConsumer,

  -- ** Interpreters
  interpretEventsChan,

  -- * Exceptions
  Critical,

  -- ** Interpreters
  interpretCritical,
  interpretCriticalNull,

  -- * Masking
  Mask,
  UninterruptipleMask,
  mask,
  uninterruptibleMask,
  restore,

  -- * Interpreters
  interpretMaskFinal,
  interpretUninterruptibleMaskFinal,

  -- * Scoped Effects
  Scoped,
  scoped,

  -- ** Interpreters
  runScoped,
  runScopedAs,
  interpretScoped,
  interpretScopedH,
  interpretScopedAs,

  -- * Monitoring
  Monitor,
  monitor,
  withMonitor,
  restart,
  Restart,
  RestartingMonitor,
  MonitorResource (MonitorResource),
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
) where

import Polysemy.Conc.Async (
  scheduleAsync,
  scheduleAsyncIO,
  withAsync,
  withAsyncBlock,
  withAsync_,
  )
import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Data.QueueResult (QueueResult)
import Polysemy.Conc.Effect.Critical (Critical)
import Polysemy.Conc.Effect.Events (EventResource, Events, consume, publish, subscribe)
import Polysemy.Conc.Effect.Interrupt (Interrupt)
import Polysemy.Conc.Effect.Mask (Mask, UninterruptipleMask, mask, restore, uninterruptibleMask)
import Polysemy.Conc.Effect.Monitor (
  Monitor,
  MonitorResource (MonitorResource),
  Restart,
  RestartingMonitor,
  ScopedMonitor,
  monitor,
  restart,
  withMonitor,
  )
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race, race, timeout)
import Polysemy.Conc.Effect.Scoped (Scoped, scoped)
import Polysemy.Conc.Effect.Sync (ScopedSync, Sync)
import Polysemy.Conc.Events (subscribeLoop, subscribeWhile)
import Polysemy.Conc.Interpreter.Critical (interpretCritical, interpretCriticalNull)
import Polysemy.Conc.Interpreter.Events (ChanConsumer, ChanEvents, EventChan, EventConsumer, interpretEventsChan)
import Polysemy.Conc.Interpreter.Interrupt (interpretInterrupt, interpretInterruptOnce)
import Polysemy.Conc.Interpreter.Mask (interpretMaskFinal, interpretUninterruptibleMaskFinal)
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
import Polysemy.Conc.Interpreter.Scoped (interpretScoped, interpretScopedAs, interpretScopedH, runScoped, runScopedAs)
import Polysemy.Conc.Interpreter.Stack (ConcStack, runConc)
import Polysemy.Conc.Interpreter.Sync (interpretScopedSync, interpretScopedSyncAs, interpretSync, interpretSyncAs)
import Polysemy.Conc.Monitor (ClockSkewConfig (ClockSkewConfig), clockSkewConfig, monitorClockSkew)
import Polysemy.Conc.Queue (loop, loopOr)
import Polysemy.Conc.Queue.Result (resultToMaybe)
import Polysemy.Conc.Race (race_, timeoutAs, timeoutAs_, timeoutMaybe, timeoutU, timeout_)
import Polysemy.Conc.Retry (retrying, retryingWithError)
import Polysemy.Conc.Sync (lock, withSync)

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
-- An 'MVar' is abstracted as 'Sync' since it can be used to synchronize threads.

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
-- The two effects 'Mask' and 'UninterruptipleMask' use the 'Polysemy.Conc.Effect.Scoped' pattern, which allow an effect
-- with resources to be constrained to a region of code.
-- The actual effect is 'Polysemy.Conc.Effect.Mask.RestoreMask', with `mask` and 'uninterruptibleMask' only specializing
-- 'Polysemy.Conc.Effect.scoped' to the appropriate resource type.
--
-- Usage is straightforward:
--
-- @
-- prog :: Member (Mask resource) r
-- prog =
--  mask do
--    doMaskedThing
--    restore do
--      doUnmaskedThing
--    doMaskedThing
-- @
--
-- The @resource@ parameter stays polymorphic; it is used to connect the resource in the interpreter to the callsite.
