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

  -- * MVars
  -- $mvar
  Sync,

  -- ** Interpreters
  interpretSync,

  -- * Racing
  -- $race
  Race,
  race,
  race_,
  timeout,
  timeout_,
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

  -- ** Interpreters
  interpretEventsChan,

  -- * Combinators
  interpretAtomic,
  withAsync,
) where

import Polysemy.Conc.Async (withAsync)
import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Data.Interrupt (Interrupt)
import Polysemy.Conc.Data.Queue (Queue)
import Polysemy.Conc.Data.QueueResult (QueueResult)
import Polysemy.Conc.Data.Race (Race, race, timeout)
import Polysemy.Conc.Effect.Events (Events, consume, publish)
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Interpreter.Events (interpretEventsChan)
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import Polysemy.Conc.Interrupt (interpretInterrupt)
import Polysemy.Conc.Queue (
  interpretQueueListReadOnlyAtomic,
  interpretQueueListReadOnlyAtomicWith,
  interpretQueueListReadOnlyState,
  interpretQueueListReadOnlyStateWith,
  )
import Polysemy.Conc.Queue.Result (resultToMaybe)
import Polysemy.Conc.Queue.TB (interpretQueueTB)
import Polysemy.Conc.Queue.TBM (interpretQueueTBM)
import Polysemy.Conc.Race (interpretRace, race_, timeout_)
import Polysemy.Conc.Retry (retrying, retryingWithError)

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
