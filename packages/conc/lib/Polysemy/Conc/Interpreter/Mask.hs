{-# options_haddock prune #-}

-- |Description: Mask Interpreters, Internal
module Polysemy.Conc.Interpreter.Mask where

import qualified Control.Exception as Base
import Polysemy.Final (runS, withStrategicToFinal, withWeavingToFinal)

import Polysemy.Conc.Effect.Mask (
  Mask,
  Restoration (Restoration),
  RestoreMask (Restore),
  UninterruptibleMask,
  )
import Polysemy.Conc.Interpreter.Scoped (interpretScopedH, runScoped)

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

-- |Interpret 'Mask' by sequencing the action without masking.
interpretMaskPure :: InterpreterFor Mask r
interpretMaskPure =
  interpretScopedH (const ($ ())) \ () -> \case
    Restore ma -> runTSimple ma

-- |Interpret 'Mask' in 'IO'.
interpretMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor Mask r
interpretMaskFinal =
  runScoped (const mask) \ r -> interpretRestoreMask r

-- |Interpret 'UninterruptibleMask' by sequencing the action without masking.
interpretUninterruptibleMaskPure :: InterpreterFor UninterruptibleMask r
interpretUninterruptibleMaskPure =
  interpretScopedH (const ($ ())) \ () -> \case
    Restore ma -> runTSimple ma

-- |Interpret 'UninterruptibleMask' in 'IO'.
interpretUninterruptibleMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor UninterruptibleMask r
interpretUninterruptibleMaskFinal =
  runScoped (const uninterruptibleMask) \ r -> interpretRestoreMask r
