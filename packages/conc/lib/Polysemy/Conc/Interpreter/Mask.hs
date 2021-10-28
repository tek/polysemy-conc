{-# options_haddock prune #-}
-- |Description: Mask Interpreters, Internal
module Polysemy.Conc.Interpreter.Mask where

import qualified Control.Exception as Base
import Polysemy (interpretH, runTSimple)
import Polysemy.Final (runS, withStrategicToFinal, withWeavingToFinal)

import Polysemy.Conc.Effect.Mask (
  Mask,
  MaskResource (MaskResource),
  RestoreMask (Restore),
  UninterruptipleMask,
  UninterruptipleMaskResource (UninterruptipleMaskResource),
  )
import Polysemy.Conc.Interpreter.Scoped (runScoped)

newtype Restoration =
  Restoration { unRestoration :: ∀ a . IO a -> IO a }

mask ::
  Member (Final IO) r =>
  (MaskResource Restoration -> Sem r a) ->
  Sem r a
mask f =
  withWeavingToFinal @IO \ s lower _ ->
    Base.mask \ restore -> (lower (f (MaskResource (Restoration restore)) <$ s))

uninterruptibleMask ::
  Member (Final IO) r =>
  (UninterruptipleMaskResource Restoration -> Sem r a) ->
  Sem r a
uninterruptibleMask f =
  withWeavingToFinal @IO \ s lower _ ->
    Base.uninterruptibleMask \ restore -> (lower (f (UninterruptipleMaskResource (Restoration restore)) <$ s))

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
          withStrategicToFinal do
            (restore <$> runS m)
      restoreSem (runTSimple ma)

-- |Interpret 'Mask' in 'IO'.
interpretMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor (Mask Restoration) r
interpretMaskFinal =
  runScoped mask \ (MaskResource r) -> interpretRestoreMask r

-- |Interpret 'UninterruptipleMask' in 'IO'.
interpretUninterruptibleMaskFinal ::
  Member (Final IO) r =>
  InterpreterFor (UninterruptipleMask Restoration) r
interpretUninterruptibleMaskFinal =
  runScoped uninterruptibleMask \ (UninterruptipleMaskResource r) -> interpretRestoreMask r
