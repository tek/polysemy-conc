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

  -- ** Interpreters
  interpretSync,
  interpretSyncAs,
  withSync,
  interpretScopedSync,
  interpretScopedSyncAs,

  -- * Racing
  -- $race
  Race,
  race,
  race_,
  timeout,
  timeout_,
  timeoutU,
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
) where

import Polysemy.Conc.Async (withAsync, withAsyncBlock, withAsync_)
import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Critical (interpretCritical, interpretCriticalNull)
import Polysemy.Conc.Data.Critical (Critical)
import Polysemy.Conc.Data.Interrupt (Interrupt)
import Polysemy.Conc.Data.Queue (Queue)
import Polysemy.Conc.Data.QueueResult (QueueResult)
import Polysemy.Conc.Data.Race (Race, race, timeout)
import Polysemy.Conc.Effect.Events (EventToken, Events, consume, publish, subscribe)
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Events (subscribeLoop, subscribeWhile)
import Polysemy.Conc.Interpreter.Events (ChanConsumer, EventChan, EventConsumer, interpretEventsChan)
import Polysemy.Conc.Interpreter.Queue.Pure (
  interpretQueueListReadOnlyAtomic,
  interpretQueueListReadOnlyAtomicWith,
  interpretQueueListReadOnlyState,
  interpretQueueListReadOnlyStateWith,
  )
import Polysemy.Conc.Interpreter.Queue.TB (interpretQueueTB)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBM)
import Polysemy.Conc.Interpreter.Sync (interpretScopedSync, interpretScopedSyncAs, interpretSync, interpretSyncAs)
import Polysemy.Conc.Interrupt (interpretInterrupt)
import Polysemy.Conc.Queue (loop, loopOr)
import Polysemy.Conc.Queue.Result (resultToMaybe)
import Polysemy.Conc.Race (interpretRace, race_, timeoutU, timeout_)
import Polysemy.Conc.Retry (retrying, retryingWithError)
import Polysemy.Conc.Sync (withSync)

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
