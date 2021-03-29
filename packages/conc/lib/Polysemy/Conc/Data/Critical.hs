{-# options_haddock prune #-}
-- |Description: Critical effect
module Polysemy.Conc.Data.Critical where

-- |An effect that catches exceptions.
--
-- The point of this is to catch exceptions that haven't been anticipated, as a failsafe.
-- Used internally in this library.
data Critical :: Effect where
  -- |Catch all exceptions of type @e@ in this computation.
  Catch :: Exception e => m a -> Critical m (Either e a)

makeSem ''Critical

-- |Catch exceptions of type @e@ and return a fallback value.
catchAs ::
  ∀ e a r .
  Exception e =>
  Member Critical r =>
  a ->
  Sem r a ->
  Sem r a
catchAs a =
  fmap (fromRight a) . catch @_ @e

-- |Convenience overload for 'SomeException'.
run ::
  Member Critical r =>
  Sem r a ->
  Sem r (Either SomeException a)
run =
  catch

-- |Convenience overload for 'SomeException'.
runAs ::
  Member Critical r =>
  a ->
  Sem r a ->
  Sem r a
runAs a =
  fmap (fromRight a) . run
