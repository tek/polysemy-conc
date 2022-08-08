{-# options_haddock prune #-}

-- |Description: Mask Interpreters, Internal
module Polysemy.Conc.Interpreter.Mask where

import qualified Control.Exception as Base
import Polysemy.Final (runS, withStrategicToFinal, withWeavingToFinal)

import Polysemy.Conc.Effect.Mask (
  Mask,
  RestoreMask (Restore),
  UninterruptibleMask,
  UninterruptibleMaskResource (UninterruptibleMaskResource), Restoration (Restoration),
  )
import Polysemy.Conc.Interpreter.Scoped (runScoped)

mask ::
  Member (Final IO) r =>
  (Restoration -> Sem r a) ->
  Sem r a
mask f =
  withWeavingToFinal @IO \ s lower _ ->
    Base.mask \ restore -> lower (f (Restoration restore) <$ s)

uninterruptibleMask ::
  Member (Final IO) r =>
  (UninterruptibleMaskResource Restoration -> Sem r a) ->
  Sem r a
uninterruptibleMask f =
  withWeavingToFinal @IO \ s lower _ ->
    Base.uninterruptibleMask \ restore -> lower (f (UninterruptibleMaskResource (Restoration restore)) <$ s)

interpretRestoreMask ::
  ∀ r .
  Member (Final IO) r =>
  Restoration ->
  InterpreterFor RestoreMask r
interpretRestoreMask (Restoration restore) =
  interpretH \case
    Restore ma -> do
      let
        restoreSem m =
          withStrategicToFinal (restore <$> runS m)
      restoreSem (runTSimple ma)

-- |Interpret 'Mask' in 'IO'.
interpretMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor (Mask Restoration) r
interpretMaskFinal =
  runScoped mask \ r -> interpretRestoreMask r

-- |Interpret 'UninterruptibleMask' in 'IO'.
interpretUninterruptibleMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor (UninterruptibleMask Restoration) r
interpretUninterruptibleMaskFinal =
  runScoped uninterruptibleMask \ (UninterruptibleMaskResource r) -> interpretRestoreMask r
