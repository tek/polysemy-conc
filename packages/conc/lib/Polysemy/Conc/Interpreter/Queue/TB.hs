-- |Description: Queue Interpreters for 'TBQueue'
module Polysemy.Conc.Interpreter.Queue.TB where

import Control.Concurrent.STM (
  TBQueue,
  atomically,
  isFullTBQueue,
  newTBQueueIO,
  peekTBQueue,
  readTBQueue,
  tryPeekTBQueue,
  tryReadTBQueue,
  writeTBQueue,
  )

import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Queue.Result (naResult)
import Polysemy.Conc.Queue.Timeout (withTimeout)

-- |Interpret 'Queue' with a 'TBQueue'.
--
-- This variant expects an allocated queue as an argument.
interpretQueueTBWith ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  TBQueue d ->
  InterpreterFor (Queue d) r
interpretQueueTBWith queue =
  interpret \case
    Queue.Read ->
      embed (atomically (QueueResult.Success <$> readTBQueue queue))
    Queue.TryRead ->
      embed (atomically (naResult <$> tryReadTBQueue queue))
    Queue.ReadTimeout timeout ->
      withTimeout timeout (Just <$> readTBQueue queue)
    Queue.Peek ->
      embed (atomically (QueueResult.Success <$> peekTBQueue queue))
    Queue.TryPeek ->
      embed (atomically (naResult <$> tryPeekTBQueue queue))
    Queue.Write d ->
      embed (atomically (writeTBQueue queue d))
    Queue.TryWrite d ->
      embed $ atomically do
        ifM (isFullTBQueue queue) (pure QueueResult.NotAvailable) (QueueResult.Success <$> writeTBQueue queue d)
    Queue.WriteTimeout timeout d ->
      withTimeout timeout (Just <$> writeTBQueue queue d)
    Queue.Closed ->
      pure False
    Queue.Close ->
      unit
{-# inline interpretQueueTBWith #-}

-- |Interpret 'Queue' with a 'TBQueue'.
interpretQueueTB ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  -- |Buffer size
  Natural ->
  InterpreterFor (Queue d) r
interpretQueueTB maxQueued sem = do
  queue <- embed (newTBQueueIO @d maxQueued)
  interpretQueueTBWith queue sem
{-# inline interpretQueueTB #-}
