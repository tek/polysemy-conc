-- |Description: Queue interpreters for 'TBMQueue'
module Polysemy.Conc.Queue.TBM where

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

import qualified Polysemy.Conc.Data.Queue as Queue
import Polysemy.Conc.Data.Queue (Queue)
import Polysemy.Conc.Data.Race (Race)
import Polysemy.Conc.Queue.Result (closedBoolResult, closedNaResult, closedResult)
import Polysemy.Conc.Queue.Timeout (withTimeout)
import Polysemy.Resource (bracket, Resource)

-- |Interpret 'Queue' with a 'TBMQueue'.
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
      atomically (closedResult <$> readTBMQueue queue)
    Queue.TryRead ->
      atomically (closedNaResult <$> tryReadTBMQueue queue)
    Queue.ReadTimeout timeout ->
      withTimeout timeout (readTBMQueue queue)
    Queue.Peek ->
      atomically (closedResult <$> peekTBMQueue queue)
    Queue.TryPeek ->
      atomically (closedNaResult <$> tryPeekTBMQueue queue)
    Queue.Write d ->
      atomically (writeTBMQueue queue d)
    Queue.TryWrite d ->
      atomically (closedBoolResult <$> tryWriteTBMQueue queue d)
    Queue.WriteTimeout timeout d ->
      withTimeout timeout do
        ifM (isClosedTBMQueue queue) (pure Nothing) (Just <$> writeTBMQueue queue d)
    Queue.Closed ->
      atomically (isClosedTBMQueue queue)
    Queue.Close ->
      atomically (closeTBMQueue queue)
{-# inline interpretQueueTBMWith #-}

-- |Interpret 'Queue' with a 'TBMQueue'.
interpretQueueTBM ::
  ∀ d r .
  Members [Resource, Race, Embed IO] r =>
  -- |Buffer size
  Int ->
  InterpreterFor (Queue d) r
interpretQueueTBM maxQueued sem = do
  bracket (embed (newTBMQueueIO @d maxQueued)) (atomically . closeTBMQueue) \ queue ->
    interpretQueueTBMWith queue sem
{-# inline interpretQueueTBM #-}
