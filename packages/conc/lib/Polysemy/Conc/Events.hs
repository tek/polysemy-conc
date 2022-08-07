-- |Description: Events Combinators
module Polysemy.Conc.Events where

import Polysemy.Conc.Async (withAsync_)
import qualified Polysemy.Conc.Effect.Events as Events
import Polysemy.Conc.Effect.Events (Consume)
import Polysemy.Conc.Effect.Gate (Gate, gate, signal, withGate)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Events (EventConsumer)

-- |Pull repeatedly from 'Polysemy.Conc.Consume', passing the event to the supplied callback.
-- Stop when the action returns @False@.
consumeWhile ::
  Member (Consume e) r =>
  (e -> Sem r Bool) ->
  Sem r ()
consumeWhile action =
  spin
  where
    spin =
      whenM (action =<< Events.consume) spin

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
-- Stop when the action returns @False@.
subscribeWhile ::
  ∀ e token r .
  Member (EventConsumer token e) r =>
  (e -> Sem r Bool) ->
  Sem r ()
subscribeWhile action =
  Events.subscribe @e @token (consumeWhile (raise . action))

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
-- Stop when the action returns @False@.
--
-- Signals the caller that the channel was successfully subscribed to using the 'Gate' effect.
subscribeWhileGated ::
  ∀ e token r .
  Members [EventConsumer token e, Gate] r =>
  (e -> Sem r Bool) ->
  Sem r ()
subscribeWhileGated action =
  Events.subscribe @e @token do
    signal
    consumeWhile (raise . action)

-- |Start a new thread that pulls repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied
-- callback and stops when the action returns @False@.
subscribeWhileAsync ::
  ∀ e token gres r a .
  Members [EventConsumer token e, Scoped gres Gate, Resource, Race, Async] r =>
  (e -> Sem r Bool) ->
  Sem r a ->
  Sem r a
subscribeWhileAsync action ma =
  withGate @gres $ withAsync_ (subscribeWhileGated @_ @token (raise . action)) do
    gate
    raise ma

-- |Pull repeatedly from 'Polysemy.Conc.Consume', passing the event to the supplied callback.
consumeLoop ::
  Member (Consume e) r =>
  (e -> Sem r ()) ->
  Sem r ()
consumeLoop action =
  forever (action =<< Events.consume)

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
subscribeLoop ::
  ∀ e token r .
  Member (EventConsumer token e) r =>
  (e -> Sem r ()) ->
  Sem r ()
subscribeLoop action =
  Events.subscribe @e @token (consumeLoop (raise . action))

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
--
-- Signals the caller that the channel was successfully subscribed to using the 'Gate' effect.
subscribeLoopGated ::
  ∀ e token r .
  Members [EventConsumer token e, Gate] r =>
  (e -> Sem r ()) ->
  Sem r ()
subscribeLoopGated action =
  Events.subscribe @e @token do
    signal
    consumeLoop (raise . action)

-- |Start a new thread that pulls repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied
-- callback.
subscribeLoopAsync ::
  ∀ e token gres r a .
  Members [EventConsumer token e, Scoped gres Gate, Resource, Race, Async] r =>
  (e -> Sem r ()) ->
  Sem r a ->
  Sem r a
subscribeLoopAsync action ma =
  withGate @gres $ withAsync_ (subscribeLoopGated @_ @token (raise . action)) do
    gate
    raise ma

-- |Block until the specified value has been returned by 'Polysemy.Conc.Consume'.
consumeFind ::
  ∀ e r .
  Eq e =>
  Member (Consume e) r =>
  e ->
  Sem r ()
consumeFind target =
  consumeWhile @e (pure . (target /=))

-- |Block until the specified value has been published to the 'Polysemy.Conc.Events' channel.
subscribeFind ::
  ∀ e token r .
  Eq e =>
  Member (EventConsumer token e) r =>
  e ->
  Sem r ()
subscribeFind target =
  subscribeWhile @e @token (pure . (target /=))
