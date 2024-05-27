{-# options_haddock prune #-}

-- | Description: Queue Interpreters for 'TBMQueue'
module Polysemy.Conc.Interpreter.Queue.TBM where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (
  TBMQueue,
  closeTBMQueue,
  isClosedTBMQueue,
  newTBMQueueIO,
  peekTBMQueue,
  readTBMQueue,
  tryPeekTBMQueue,
  tryReadTBMQueue,
  tryWriteTBMQueue,
  writeTBMQueue,
  )

import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Queue.Result (closedBoolResult, closedNaResult, closedResult)
import Polysemy.Conc.Queue.Timeout (withTimeout)

-- | Interpret 'Queue' with a 'TBMQueue'.
--
-- This variant expects an allocated queue as an argument.
interpretQueueTBMWith ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  TBMQueue d ->
  InterpreterFor (Queue d) r
interpretQueueTBMWith queue =
  interpret \case
    Queue.Read ->
      embed (atomically (closedResult <$> readTBMQueue queue))
    Queue.TryRead ->
      embed (atomically (closedNaResult <$> tryReadTBMQueue queue))
    Queue.ReadTimeout timeout ->
      withTimeout timeout (readTBMQueue queue)
    Queue.Peek ->
      embed (atomically (closedResult <$> peekTBMQueue queue))
    Queue.TryPeek ->
      embed (atomically (closedNaResult <$> tryPeekTBMQueue queue))
    Queue.Write d ->
      embed (atomically (writeTBMQueue queue d))
    Queue.TryWrite d ->
      embed (atomically (closedBoolResult <$> tryWriteTBMQueue queue d))
    Queue.WriteTimeout timeout d ->
      withTimeout timeout do
        ifM (isClosedTBMQueue queue) (pure Nothing) (Just <$> writeTBMQueue queue d)
    Queue.Closed ->
      embed (atomically (isClosedTBMQueue queue))
    Queue.Close ->
      embed (atomically (closeTBMQueue queue))
{-# inline interpretQueueTBMWith #-}

withTBMQueue ::
  ∀ d r a .
  Members [Resource, Embed IO] r =>
  Int ->
  (TBMQueue d -> Sem r a) ->
  Sem r a
withTBMQueue maxQueued =
  bracket (embed (newTBMQueueIO maxQueued)) (embed . atomically . closeTBMQueue)

-- | Interpret 'Queue' with a 'TBMQueue'.
interpretQueueTBM ::
  ∀ d r .
  Members [Resource, Race, Embed IO] r =>
  -- | Buffer size
  Int ->
  InterpreterFor (Queue d) r
interpretQueueTBM maxQueued sem = do
  withTBMQueue maxQueued \ queue ->
    interpretQueueTBMWith queue sem
{-# inline interpretQueueTBM #-}
