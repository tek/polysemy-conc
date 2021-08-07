{-# options_haddock prune #-}
-- |Description: Events/Consume Interpreters, Internal
module Polysemy.Conc.Interpreter.Events where

import Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan, dupChan, newChan, readChan, tryWriteChan)
import Polysemy (InterpretersFor)

import qualified Polysemy.Conc.Effect.Events as Events
import Polysemy.Conc.Effect.Events (EventToken (EventToken), Events, Consume)
import Polysemy.Conc.Effect.Scoped (Scoped, runScopedAs)

-- |Interpret 'Consume' by reading from an 'OutChan'.
-- Used internally by 'interpretEventsChan', not safe to use directly.
interpretConsumeChan ::
  ∀ e r .
  Member (Embed IO) r =>
  EventToken (OutChan e) ->
  InterpreterFor (Consume e) r
interpretConsumeChan (EventToken chan) =
  interpret \case
    Events.Consume ->
      embed (readChan chan)

-- |Interpret 'Events' by writing to an 'InChan'.
-- Used internally by 'interpretEventsChan', not safe to use directly.
-- When the channel queue is full, this silently discards events.
interpretEventsInChan ::
  ∀ e r .
  Member (Embed IO) r =>
  InChan e ->
  InterpreterFor (Events (OutChan e) e) r
interpretEventsInChan inChan =
  interpret \case
    Events.Publish e ->
      void (embed (tryWriteChan inChan e))

-- |Interpret 'Events' and 'Consume' together by connecting them to the two ends of an unagi channel.
-- 'Consume' is only interpreted in a 'Scoped' manner, ensuring that a new duplicate of the channel is created so that
-- all consumers see all events (from the moment they are connected).
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
  Member (Embed IO) r =>
  InterpretersFor [Events (OutChan e) e, Scoped (EventToken (OutChan e)) (Consume e)] r
interpretEventsChan sem = do
  (inChan, _) <- embed (newChan @e 64)
  runScopedAs (EventToken <$> embed (dupChan inChan)) interpretConsumeChan (interpretEventsInChan inChan sem)