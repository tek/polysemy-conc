{-# options_haddock prune #-}

-- |Description: Pty Interpreters, Internal
module Polysemy.Process.Interpreter.Pty where

import Polysemy.Conc.Effect.Scoped (Scoped_)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedResumable)
import Polysemy.Resume (Stop, stopEitherWith, stopNote, type (!!))
import System.Posix (closeFd, fdToHandle, openPseudoTerminal)
import System.Posix.Pty (closePty, createPty, ptyDimensions, resizePty)

import Polysemy.Process.Data.PtyError (PtyError (PtyError))
import Polysemy.Process.Data.PtyResources (PtyResources (PtyResources, handle, primary, pty, secondary))
import Polysemy.Process.Effect.Pty (Cols (Cols), Pty (Handle, Resize, Size), Rows (Rows))

tryStop ::
  Members [Stop PtyError, Embed IO] r =>
  IO a ->
  Sem r a
tryStop =
  stopEitherWith PtyError <=< tryIOError

acquirePty ::
  Member (Embed IO) r =>
  Sem (Stop PtyError : r) PtyResources
acquirePty = do
  (primary, secondary) <- tryStop openPseudoTerminal
  pty <- stopNote (PtyError "no pty returned") =<< tryStop (createPty secondary)
  handle <- tryStop (fdToHandle secondary)
  pure PtyResources {..}

releasePty ::
  Member (Embed IO) r =>
  PtyResources ->
  Sem r ()
releasePty PtyResources {primary, pty} = do
  tryAny_ (closePty pty)
  tryAny_ (closeFd primary)

withPty ::
  Members [Resource, Embed IO] r =>
  (PtyResources -> Sem (Stop PtyError : r) a) ->
  Sem (Stop PtyError : r) a
withPty =
  bracket acquirePty releasePty

-- |Interpret Pty as a 'System.Posix.Pty'.
interpretPty ::
  Members [Resource, Embed IO] r =>
  InterpreterFor (Scoped_ PtyResources Pty !! PtyError) r
interpretPty =
  interpretScopedResumable (const withPty) \ PtyResources {..} -> \case
    Handle ->
      pure handle
    Resize rows cols -> do
      tryStop (resizePty pty (fromIntegral rows, fromIntegral cols))
    Size ->
      bimap Rows Cols <$> tryStop (ptyDimensions pty)
