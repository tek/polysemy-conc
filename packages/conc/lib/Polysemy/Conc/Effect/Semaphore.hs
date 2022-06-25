-- |Semaphore effect, Internal.
module Polysemy.Conc.Effect.Semaphore where

-- |This effect abstracts over the concept of a quantity semaphore, a concurrency primitive that contains a number of
-- slots that can be acquired and released.
data Semaphore :: Effect where
  -- |Wait until a slot is available, then acquire it.
  Wait :: Semaphore m ()
  -- |Release a slot.
  Signal :: Semaphore m ()

makeSem_ ''Semaphore

-- |Wait until a slot is available, then acquire it.
wait ::
  ∀ r .
  Member Semaphore r =>
  Sem r ()

-- |Release a slot.
signal ::
  ∀ r .
  Member Semaphore r =>
  Sem r ()
