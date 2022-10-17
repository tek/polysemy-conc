-- |Description: Events Combinators
module Polysemy.Conc.Events where

import Polysemy.Conc.Async (withAsync_)
import qualified Polysemy.Conc.Effect.Events as Events
import Polysemy.Conc.Effect.Events (Consume)
import Polysemy.Conc.Effect.Gate (Gate, Gates, gate, signal, withGate)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped_)
import Polysemy.Conc.Interpreter.Events (EventConsumer)

-- |Create a new scope for 'Polysemy.Conc.Events', causing the nested program to get its own copy of the event stream.
--
-- Calls 'signal' before running the argument to ensure that 'Events.subscribe' has finished creating a channel, for use
-- with asynchronous execution.
subscribeGated ::
  ∀ e r .
  Members [EventConsumer e, Gate] r =>
  InterpreterFor (Consume e) r
subscribeGated action =
  Events.subscribe @e do
    signal
    action

-- |Create a new scope for 'Polysemy.Conc.Events', causing the nested program to get its own copy of the event stream.
--
-- Executes in a new thread, ensuring that the main thread blocks until 'Events.subscribe' has finished creating a
-- channel.
subscribeAsync ::
  ∀ e r a .
  Members [EventConsumer e, Scoped_ Gate, Resource, Race, Async] r =>
  Sem (Consume e : r) () ->
  Sem r a ->
  Sem r a
subscribeAsync consumer ma =
  withGate $ withAsync_ (subscribeGated @_ (raiseUnder @Gate consumer)) do
    gate
    raise @Gate ma

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
  ∀ e r .
  Member (EventConsumer e) r =>
  (e -> Sem r Bool) ->
  Sem r ()
subscribeWhile action =
  Events.subscribe @e (consumeWhile (raise . action))

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
-- Stop when the action returns @False@.
--
-- Signals the caller that the channel was successfully subscribed to using the 'Gate' effect.
subscribeWhileGated ::
  ∀ e r .
  Members [EventConsumer e, Gate] r =>
  (e -> Sem r Bool) ->
  Sem r ()
subscribeWhileGated action =
  subscribeGated @e do
    consumeWhile (raise . action)

-- |Start a new thread that pulls repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied
-- callback and stops when the action returns @False@.
subscribeWhileAsync ::
  ∀ e r a .
  Members [EventConsumer e, Gates, Resource, Race, Async] r =>
  (e -> Sem (Consume e : r) Bool) ->
  Sem r a ->
  Sem r a
subscribeWhileAsync action =
  subscribeAsync @e (consumeWhile action)

-- |Pull repeatedly from 'Polysemy.Conc.Consume', passing the event to the supplied callback.
consumeLoop ::
  Member (Consume e) r =>
  (e -> Sem r ()) ->
  Sem r ()
consumeLoop action =
  forever (action =<< Events.consume)

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
subscribeLoop ::
  ∀ e r .
  Member (EventConsumer e) r =>
  (e -> Sem r ()) ->
  Sem r ()
subscribeLoop action =
  Events.subscribe @e (consumeLoop (raise . action))

-- |Pull repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied callback.
--
-- Signals the caller that the channel was successfully subscribed to using the 'Gate' effect.
subscribeLoopGated ::
  ∀ e r .
  Members [EventConsumer e, Gate] r =>
  (e -> Sem r ()) ->
  Sem r ()
subscribeLoopGated action =
  subscribeGated @e do
    consumeLoop (raise . action)

-- |Start a new thread that pulls repeatedly from the 'Polysemy.Conc.Events' channel, passing the event to the supplied
-- callback.
subscribeLoopAsync ::
  ∀ e r a .
  Members [EventConsumer e, Gates, Resource, Race, Async] r =>
  (e -> Sem (Consume e : r) ()) ->
  Sem r a ->
  Sem r a
subscribeLoopAsync action =
  subscribeAsync @e (consumeLoop action)

-- |Block until a value matching the predicate has been returned by 'Polysemy.Conc.Consume'.
consumeFind ::
  ∀ e r .
  Member (Consume e) r =>
  (e -> Sem r Bool) ->
  Sem r e
consumeFind f =
  spin
  where
    spin = do
      e <- Events.consume
      ifM (f e) (pure e) spin

-- |Block until a value matching the predicate has been published to the 'Polysemy.Conc.Events' channel.
subscribeFind ::
  ∀ e r .
  Member (EventConsumer e) r =>
  (e -> Sem (Consume e : r) Bool) ->
  Sem r e
subscribeFind f =
  Events.subscribe @e (consumeFind f)

-- |Return the first value returned by 'Polysemy.Conc.Consume' for which the function produces 'Just'.
consumeFirstJust ::
  ∀ e a r .
  Member (Consume e) r =>
  (e -> Sem r (Maybe a)) ->
  Sem r a
consumeFirstJust f =
  spin
  where
    spin = do
      e <- Events.consume
      maybe spin pure =<< f e

-- |Return the first value published to the 'Polysemy.Conc.Events' channel for which the function produces 'Just'.
subscribeFirstJust ::
  ∀ e a r .
  Member (EventConsumer e) r =>
  (e -> Sem (Consume e : r) (Maybe a)) ->
  Sem r a
subscribeFirstJust f =
  Events.subscribe @e (consumeFirstJust f)

-- |Block until the specified value has been returned by 'Polysemy.Conc.Consume'.
consumeElem ::
  ∀ e r .
  Eq e =>
  Member (Consume e) r =>
  e ->
  Sem r ()
consumeElem target =
  void (consumeFind (pure . (target ==)))

-- |Block until the specified value has been published to the 'Polysemy.Conc.Events' channel.
subscribeElem ::
  ∀ e r .
  Eq e =>
  Member (EventConsumer e) r =>
  e ->
  Sem r ()
subscribeElem target =
  Events.subscribe @e (consumeElem target)
