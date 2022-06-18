module Polysemy.Conc.Effect.Semaphore where

data Semaphore :: Effect where
  Wait :: Semaphore m ()
  Signal :: Semaphore m ()

makeSem ''Semaphore
