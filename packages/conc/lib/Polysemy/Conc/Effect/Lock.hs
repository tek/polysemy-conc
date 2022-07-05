-- |Lock effect, Internal
module Polysemy.Conc.Effect.Lock where

-- |An exclusive lock or mutex, protecting a region from concurrent access.
data Lock :: Effect where
  -- |Run an action if the lock is available, block otherwise.
  Lock :: m a -> Lock m a
  -- |Run the second action if the lock is available, or the first action otherwise.
  LockOr :: m a -> m a -> Lock m a

makeSem_ ''Lock

-- |Run an action if the lock is available, block otherwise.
lock ::
  ∀ r a .
  Member Lock r =>
  Sem r a ->
  Sem r a

-- |Run an action if the lock is available, block otherwise.
lockOr ::
  ∀ r a .
  Member Lock r =>
  Sem r a ->
  Sem r a ->
  Sem r a

-- |Run an action if the lock is available, skip and return 'Nothing' otherwise.
lockOrSkip ::
  ∀ r a .
  Member Lock r =>
  Sem r a ->
  Sem r (Maybe a)
lockOrSkip ma =
  lockOr (pure Nothing) (Just <$> ma)
{-# inline lockOrSkip #-}

-- |Run an action if the lock is available, skip otherwise.
-- Return @()@.
lockOrSkip_ ::
  ∀ r a .
  Member Lock r =>
  Sem r a ->
  Sem r ()
lockOrSkip_ ma =
  lockOr unit (void ma)
{-# inline lockOrSkip_ #-}
