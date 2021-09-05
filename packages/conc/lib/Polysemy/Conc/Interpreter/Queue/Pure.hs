-- |Description: Pure Queue Interpreters
module Polysemy.Conc.Interpreter.Queue.Pure where

import Polysemy.AtomicState (atomicState')
import Polysemy.State (State, evalState, get, gets, put)

import Polysemy.Conc.AtomicState (interpretAtomic)
import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (Queue)
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import Polysemy.Conc.Data.QueueResult (QueueResult)

-- |Reinterpret 'Queue' as 'AtomicState' with a list that cannot be written to.
-- Useful for testing.
interpretQueueListReadOnlyAtomicWith ::
  ∀ d r .
  Member (AtomicState [d]) r =>
  InterpreterFor (Queue d) r
interpretQueueListReadOnlyAtomicWith =
  interpret \case
    Queue.Read ->
      read
    Queue.TryRead ->
      read
    Queue.ReadTimeout _ ->
      read
    Queue.Peek ->
      peek
    Queue.TryPeek ->
      peek
    Queue.Write _ ->
      pass
    Queue.TryWrite _ ->
      pure QueueResult.NotAvailable
    Queue.WriteTimeout _ _ ->
      pure QueueResult.NotAvailable
    Queue.Closed ->
      atomicGets @[d] null
    Queue.Close ->
      atomicPut @[d] []
  where
    read :: Sem r (QueueResult d)
    read =
      atomicState' @[d] \case
        [] -> ([], QueueResult.Closed)
        h : t -> (t, QueueResult.Success h)
    peek :: Sem r (QueueResult d)
    peek =
      atomicGets @[d] \case
        [] -> QueueResult.Closed
        h : _ -> QueueResult.Success h
{-# inline interpretQueueListReadOnlyAtomicWith #-}

-- |Variant of 'interpretQueueListReadOnlyAtomicWith' that interprets the 'AtomicState'.
interpretQueueListReadOnlyAtomic ::
  ∀ d r .
  Member (Embed IO) r =>
  [d] ->
  InterpreterFor (Queue d) r
interpretQueueListReadOnlyAtomic ds sem =
  interpretAtomic ds (interpretQueueListReadOnlyAtomicWith (raiseUnder sem))
{-# inline interpretQueueListReadOnlyAtomic #-}

-- |Reinterpret 'Queue' as 'State' with a list that cannot be written to.
-- Useful for testing.
interpretQueueListReadOnlyStateWith ::
  ∀ d r .
  Member (State [d]) r =>
  InterpreterFor (Queue d) r
interpretQueueListReadOnlyStateWith =
  interpret \case
    Queue.Read ->
      read
    Queue.TryRead ->
      read
    Queue.ReadTimeout _ ->
      read
    Queue.Peek ->
      peek
    Queue.TryPeek ->
      peek
    Queue.Write _ ->
      pass
    Queue.TryWrite _ ->
      pure QueueResult.NotAvailable
    Queue.WriteTimeout _ _ ->
      pure QueueResult.NotAvailable
    Queue.Closed ->
      gets @[d] null
    Queue.Close ->
      put @[d] []
  where
    read :: Sem r (QueueResult d)
    read =
      get @[d] >>= \case
        [] -> pure QueueResult.Closed
        h : t -> QueueResult.Success h <$ put t
    peek :: Sem r (QueueResult d)
    peek =
      gets @[d] \case
        [] -> QueueResult.Closed
        h : _ -> QueueResult.Success h
{-# inline interpretQueueListReadOnlyStateWith #-}

-- |Variant of 'interpretQueueListReadOnlyAtomicWith' that interprets the 'State'.
interpretQueueListReadOnlyState ::
  ∀ d r .
  Member (Embed IO) r =>
  [d] ->
  InterpreterFor (Queue d) r
interpretQueueListReadOnlyState ds sem = do
  evalState ds (interpretQueueListReadOnlyStateWith (raiseUnder sem))
{-# inline interpretQueueListReadOnlyState #-}
