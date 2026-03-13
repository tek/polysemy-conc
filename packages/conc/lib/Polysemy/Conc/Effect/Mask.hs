{-# options_haddock prune #-}

-- | Description: Mask Effect, Internal
module Polysemy.Conc.Effect.Mask where

-- | Part of an effect abstracting 'Control.Exception.mask'.
data RestoreMask :: Effect where
  Restore :: m a -> RestoreMask m a

makeSem_ ''RestoreMask

-- | Restore the previous masking state.
-- Can only be called inside of an action passed to 'mask' or 'uninterruptibleMask'.
restore ::
  ∀ r a .
  Member RestoreMask r =>
  Sem r a ->
  Sem r a

-- | Resource type for the scoped 'Mask' effect, wrapping the @restore@ callback passed in by 'Base.mask'.
newtype Restoration =
  Restoration { unRestoration :: ∀ a . IO a -> IO a }

-- | Whether 'mask' or 'uninterruptibleMask' was requested.
data MaskMode =
  Interruptible
  |
  Uninterruptible
  deriving stock (Eq, Show)

-- | The scoped masking effect, parameterized by 'MaskMode'.
type Mask =
  Scoped MaskMode RestoreMask

-- | Deprecated synonym for 'Mask'.
type UninterruptibleMask = Mask
{-# deprecated UninterruptibleMask "Use Mask instead, which now handles both variants via MaskMode" #-}

-- | Mark a region as masked.
-- Uses the 'Scoped' pattern with 'Interruptible'.
mask ::
  Member Mask r =>
  InterpreterFor RestoreMask r
mask =
  scoped Interruptible

-- | Mark a region as uninterruptibly masked.
-- Uses the 'Scoped' pattern with 'Uninterruptible'.
uninterruptibleMask ::
  Member Mask r =>
  InterpreterFor RestoreMask r
uninterruptibleMask =
  scoped Uninterruptible
