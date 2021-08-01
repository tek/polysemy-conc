{-# options_haddock prune #-}
-- |Description: Events/Consume Effects, Internal
module Polysemy.Conc.Effect.Events where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

-- |Consume events emitted by 'Events'.
data Consume (e :: Type) :: Effect where
  Consume :: Consume e m e

makeSem ''Consume

-- |Marker for the 'Scoped' token for 'Events'.
newtype EventToken token =
  EventToken { unEventToken :: token }
  deriving (Eq, Show, Generic)

-- |An event publisher that can be consumed from multiple threads.
data Events (token :: Type) (e :: Type) :: Effect where
  Publish :: e -> Events token e m ()

makeSem ''Events

-- |Create a new scope for 'Events', causing the nested program to get its own copy of the event stream.
-- To be used with 'Polysemy.Conc.interpretEventsChan'.
subscribe ::
  ∀ e token r .
  Member (Scoped (EventToken token) (Consume e)) r =>
  InterpreterFor (Consume e) r
subscribe =
  scoped @(EventToken token)
