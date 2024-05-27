{-# options_haddock prune #-}

-- | Description: Events/Consume Interpreters, Internal
module Polysemy.Conc.Interpreter.Events where

import Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan, dupChan, newChan, readChan, tryWriteChan)

import Polysemy.Conc.Async (withAsync_)
import qualified Polysemy.Conc.Effect.Events as Events
import Polysemy.Conc.Effect.Events (Consume, Events)
import Polysemy.Conc.Effect.Race (Race)

-- | Convenience alias for the consumer effect.
type EventConsumer e =
  Scoped_ (Consume e)

-- | Interpret 'Consume' by reading from an 'OutChan'.
-- Used internally by 'interpretEventsChan', not safe to use directly.
interpretConsumeChan ::
  ∀ e r .
  Member (Embed IO) r =>
  OutChan e ->
  InterpreterFor (Consume e) r
interpretConsumeChan chan =
  interpret \case
    Events.Consume ->
      embed (readChan chan)

-- | Interpret 'Events' by writing to an 'InChan'.
-- Used internally by 'interpretEventsChan', not safe to use directly.
-- When the channel queue is full, this silently discards events.
interpretEventsInChan ::
  ∀ e r .
  Member (Embed IO) r =>
  InChan e ->
  InterpreterFor (Events e) r
interpretEventsInChan inChan =
  interpret \case
    Events.Publish e ->
      void (embed (tryWriteChan inChan e))

-- | Interpret 'Events' and 'Consume' together by connecting them to the two ends of an unagi channel.
-- 'Consume' is only interpreted in a 'Polysemy.Conc.Scoped' manner, ensuring that a new duplicate of the channel is
-- created so that all consumers see all events (from the moment they are connected).
--
-- This should be used in conjunction with 'Polysemy.Conc.subscribe':
--
-- @
-- interpretEventsChan do
--   async $ subscribe do
--     putStrLn =<< consume
--   publish "hello"
-- @
--
-- Whenever 'Polysemy.Conc.subscribe' creates a new scope, this interpreter calls 'dupChan' and passes the
-- duplicate to 'interpretConsumeChan'.
interpretEventsChan ::
  ∀ e r .
  Members [Resource, Race, Async, Embed IO] r =>
  InterpretersFor [Events e, EventConsumer e] r
interpretEventsChan sem = do
  (inChan, outChan) <- embed (newChan @e 64)
  withAsync_ (forever (embed (readChan outChan))) do
    runScopedAs (const (embed (dupChan inChan))) interpretConsumeChan (interpretEventsInChan inChan sem)
