{-# options_haddock prune #-}

-- |Description: Mask Effect, Internal
module Polysemy.Conc.Effect.Mask where

-- |Part of an effect abstracting 'Control.Exception.mask'.
data RestoreMask :: Effect where
  Restore :: m a -> RestoreMask m a

makeSem_ ''RestoreMask

-- |Restore the previous masking state.
-- Can only be called inside of an action passed to 'mask' or 'uninterruptibleMask'.
restore ::
  ∀ r a .
  Member RestoreMask r =>
  Sem r a ->
  Sem r a

-- |Resource type for the scoped 'Mask' effect, wrapping the @restore@ callback passed in by 'Base.mask'.
newtype Restoration =
  Restoration { unRestoration :: ∀ a . IO a -> IO a }

-- |The scoped masking effect.
type Mask =
  Scoped_ RestoreMask

-- |The scoped uninterruptible masking effect.
type UninterruptibleMask =
  Scoped_ RestoreMask

-- |Mark a region as masked.
-- Uses the 'Scoped_' pattern.
mask ::
  Member Mask r =>
  InterpreterFor RestoreMask r
mask =
  scoped_

-- |Mark a region as uninterruptibly masked.
-- Uses the 'Scoped_' pattern.
uninterruptibleMask ::
  Member UninterruptibleMask r =>
  InterpreterFor RestoreMask r
uninterruptibleMask =
  scoped_
