-- |Description: Polysemy Effects for Concurrency
module Polysemy.Conc (
  -- * Introduction
  -- $intro

  -- * Queues
  -- $queue
  Queue,
  QueueResult,
  resultToMaybe,

  -- ** Interpreters
  interpretQueueTBM,
  interpretQueueTB,
  interpretQueueListReadOnlyAtomic,
  interpretQueueListReadOnlyAtomicWith,
  interpretQueueListReadOnlyState,
  interpretQueueListReadOnlyStateWith,

  -- ** Combinators
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

  -- * Event Channels
  Events,
  publish,
  consume,
  subscribe,
  subscribeWhile,
  subscribeLoop,
  EventToken,
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

  -- * Other Combinators
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
import Polysemy.Conc.Effect.Events (EventToken, Events, consume, publish, subscribe)
import Polysemy.Conc.Effect.Interrupt (Interrupt)
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race, race, timeout)
import Polysemy.Conc.Effect.Sync (ScopedSync, Sync)
import Polysemy.Conc.Events (subscribeLoop, subscribeWhile)
import Polysemy.Conc.Interpreter.Critical (interpretCritical, interpretCriticalNull)
import Polysemy.Conc.Interpreter.Events (ChanConsumer, ChanEvents, EventChan, EventConsumer, interpretEventsChan)
import Polysemy.Conc.Interpreter.Interrupt (interpretInterrupt)
import Polysemy.Conc.Interpreter.Queue.Pure (
  interpretQueueListReadOnlyAtomic,
  interpretQueueListReadOnlyAtomicWith,
  interpretQueueListReadOnlyState,
  interpretQueueListReadOnlyStateWith,
  )
import Polysemy.Conc.Interpreter.Queue.TB (interpretQueueTB)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBM)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretScopedSync, interpretScopedSyncAs, interpretSync, interpretSyncAs)
import Polysemy.Conc.Queue (loop, loopOr)
import Polysemy.Conc.Queue.Result (resultToMaybe)
import Polysemy.Conc.Race (race_, timeoutAs, timeoutAs_, timeoutMaybe, timeoutU, timeout_)
import Polysemy.Conc.Retry (retrying, retryingWithError)
import Polysemy.Conc.Sync (withSync, lock)

-- $intro
-- This library provides an assortment of tools for concurrency-related tasks:
--
-- - [STM queues](#queue)
-- - [MVars](#mvar)
-- - [Racing](#race)
-- - [Signal handling](#signal)

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
