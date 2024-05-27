{-# options_haddock prune #-}

-- | Description: Critical effect
module Polysemy.Conc.Effect.Critical where

import Prelude hiding (catch)

-- | An effect that catches exceptions.
--
-- Provides the exact functionality of `Polysemy.Error.fromExceptionSem`, but pushes the dependency on @Final IO@ to the
-- interpreter, and makes it optional.
data Critical :: Effect where
  -- | Catch all exceptions of type @e@ in this computation.
  Catch :: Exception e => m a -> Critical m (Either e a)

makeSem ''Critical

-- | Catch exceptions of type @e@ and return a fallback value.
catchAs ::
  ∀ e a r .
  Exception e =>
  Member Critical r =>
  a ->
  Sem r a ->
  Sem r a
catchAs a =
  fmap (fromRight a) . catch @_ @e

-- | Convenience overload for 'SomeException'.
run ::
  Member Critical r =>
  Sem r a ->
  Sem r (Either SomeException a)
run =
  catch

-- | Convenience overload for 'SomeException'.
runAs ::
  Member Critical r =>
  a ->
  Sem r a ->
  Sem r a
runAs a =
  fmap (fromRight a) . run
