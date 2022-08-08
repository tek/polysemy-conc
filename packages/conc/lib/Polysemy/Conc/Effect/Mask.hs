{-# options_haddock prune #-}

-- |Description: Mask Effect, Internal
module Polysemy.Conc.Effect.Mask where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

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
type Mask resource =
  Scoped resource RestoreMask

-- |Convenience alias for the default implementation of 'Mask'.
type MaskIO =
  Mask Restoration

-- |The scoped uninterruptible masking effect.
type UninterruptibleMask resource =
  Scoped resource RestoreMask

-- |Convenience alias for the default implementation of 'UninterruptibleMask'.
type UninterruptibleMaskIO =
  Mask Restoration

-- |Mark a region as masked.
-- Uses the 'Scoped' pattern.
mask ::
  ∀ resource r .
  Member (Mask resource) r =>
  InterpreterFor RestoreMask r
mask =
  scoped @resource

-- |Mark a region as uninterruptibly masked.
-- Uses the 'Scoped' pattern.
uninterruptibleMask ::
  ∀ resource r .
  Member (UninterruptibleMask resource) r =>
  InterpreterFor RestoreMask r
uninterruptibleMask =
  scoped @resource
