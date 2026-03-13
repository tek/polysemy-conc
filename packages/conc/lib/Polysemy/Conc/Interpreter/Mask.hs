{-# options_haddock prune #-}

-- | Description: Mask Interpreters, Internal
module Polysemy.Conc.Interpreter.Mask where

import qualified Control.Exception as Base
import Polysemy.Final (runS, withStrategicToFinal, withWeavingToFinal)

import Polysemy.Conc.Effect.Mask (
  Mask,
  MaskMode (Interruptible, Uninterruptible),
  Restoration (Restoration),
  RestoreMask (Restore),
  )

mask ::
  Member (Final IO) r =>
  (Restoration -> Sem r a) ->
  Sem r a
mask f =
  withWeavingToFinal @IO \ s lower _ ->
    Base.mask \ restore -> lower (f (Restoration restore) <$ s)

uninterruptibleMask ::
  Member (Final IO) r =>
  (Restoration -> Sem r a) ->
  Sem r a
uninterruptibleMask f =
  withWeavingToFinal @IO \ s lower _ ->
    Base.uninterruptibleMask \ restore -> lower (f (Restoration restore) <$ s)

interpretRestoreMask ::
  ∀ r .
  Member (Final IO) r =>
  Restoration ->
  InterpreterFor RestoreMask r
interpretRestoreMask (Restoration restore) =
  interpretH \case
    Restore ma ->
      withStrategicToFinal (restore <$> runS (runTSimple ma))

maskForMode ::
  Member (Final IO) r =>
  MaskMode ->
  (Restoration -> Sem r a) ->
  Sem r a
maskForMode = \case
  Interruptible -> mask
  Uninterruptible -> uninterruptibleMask

-- | Interpret 'Mask' by sequencing the action without masking.
interpretMaskPure :: InterpreterFor Mask r
interpretMaskPure =
  interpretScopedH (const ($ ())) \ () -> \case
    Restore ma -> runTSimple ma

-- | Interpret 'Mask' in 'IO', dispatching on 'MaskMode' to select 'Base.mask' or 'Base.uninterruptibleMask'.
interpretMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor Mask r
interpretMaskFinal =
  runScoped maskForMode interpretRestoreMask

-- | Interpret 'Mask' by sequencing the action without masking.
--
-- @since 0.14.1.0
{-# deprecated interpretUninterruptibleMaskPure "Use interpretMaskPure, which now handles both variants" #-}
interpretUninterruptibleMaskPure :: InterpreterFor Mask r
interpretUninterruptibleMaskPure =
  interpretMaskPure

-- | Interpret 'Mask' in 'IO'.
--
-- @since 0.14.1.0
{-# deprecated interpretUninterruptibleMaskFinal "Use interpretMaskFinal, which now handles both variants" #-}
interpretUninterruptibleMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor Mask r
interpretUninterruptibleMaskFinal =
  interpretMaskFinal
