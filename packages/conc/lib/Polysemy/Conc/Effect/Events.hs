{-# options_haddock prune #-}
-- |Description: Events/Consume Effects, Internal
module Polysemy.Conc.Effect.Events where

import Polysemy (makeSem_)
import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

-- |Marker for the 'Scoped' resource for 'Events'.
newtype EventResource resource =
  EventResource { unEventToken :: resource }
  deriving (Eq, Show, Generic)

-- |An event publisher that can be consumed from multiple threads.
data Events (resource :: Type) (e :: Type) :: Effect where
  Publish :: e -> Events resource e m ()

makeSem_ ''Events

-- |Publish one event.
publish ::
  ∀ e resource r .
  Member (Events resource e) r =>
  e ->
  Sem r ()

-- |Consume events emitted by 'Events'.
data Consume (e :: Type) :: Effect where
  Consume :: Consume e m e

makeSem_ ''Consume

-- |Consume one event emitted by 'Events'.
consume ::
  ∀ e r .
  Member (Consume e) r =>
  Sem r e

-- |Create a new scope for 'Events', causing the nested program to get its own copy of the event stream.
-- To be used with 'Polysemy.Conc.interpretEventsChan'.
subscribe ::
  ∀ e resource r .
  Member (Scoped (EventResource resource) (Consume e)) r =>
  InterpreterFor (Consume e) r
subscribe =
  scoped @(EventResource resource)
