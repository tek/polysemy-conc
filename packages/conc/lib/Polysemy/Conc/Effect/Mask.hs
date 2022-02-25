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

newtype MaskResource resource =
  MaskResource { unMaskResource :: resource }

newtype UninterruptibleMaskResource resource =
  UninterruptibleMaskResource { unUninterruptibleMaskResource :: resource }

-- |The scoped masking effect.
type Mask resource =
  Scoped (MaskResource resource) RestoreMask

-- |The scoped uninterruptible masking effect.
type UninterruptibleMask resource =
  Scoped (UninterruptibleMaskResource resource) RestoreMask

-- |Mark a region as masked.
-- Uses the 'Scoped' pattern.
mask ::
  ∀ resource r .
  Member (Mask resource) r =>
  InterpreterFor RestoreMask r
mask =
  scoped @(MaskResource resource)

-- |Mark a region as uninterruptibly masked.
-- Uses the 'Scoped' pattern.
uninterruptibleMask ::
  ∀ resource r .
  Member (UninterruptibleMask resource) r =>
  InterpreterFor RestoreMask r
uninterruptibleMask =
  scoped @(UninterruptibleMaskResource resource)
