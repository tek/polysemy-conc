{-# options_haddock prune #-}

-- |Description: Race effect
module Polysemy.Conc.Effect.Race where

import Polysemy.Time (TimeUnit)

-- |Abstract the concept of running two programs concurrently, aborting the other when one terminates.
-- 'Timeout' is a simpler variant, where one thread just sleeps for a given interval.
data Race :: Effect where
  -- |Run both programs concurrently, returning the result of the faster one.
  Race :: m a -> m b -> Race m (Either a b)
  -- |Run the fallback action if the given program doesn't finish within the specified interval.
  Timeout :: TimeUnit u => m a -> u -> m b -> Race m (Either a b)

makeSem_ ''Race

-- |Run both programs concurrently, returning the result of the faster one.
race ::
  ∀ a b r .
  Member Race r =>
  Sem r a ->
  Sem r b ->
  Sem r (Either a b)

-- |Run the fallback action if the given program doesn't finish within the specified interval.
timeout ::
  ∀ a b u r .
  TimeUnit u =>
  Member Race r =>
  Sem r a ->
  u ->
  Sem r b ->
  Sem r (Either a b)
