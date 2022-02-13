{-# options_haddock prune #-}

-- |Description: Pty Interpreters, Internal
module Polysemy.Process.Interpreter.Pty where

import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedResumable)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Resume (Stop, type (!!), stopNote)
import System.Posix.Pty (resizePty, createPty, closePty)

import Polysemy.Process.Data.PtyError (PtyError (CreationFailed))
import Polysemy.Process.Data.PtyResources (PtyResources (PtyResources, primary, secondary, handle, pty))
import Polysemy.Process.Effect.Pty (Pty (Handle, Resize))
import System.Posix (openPseudoTerminal, fdToHandle, closeFd)

tryStop ::
  Members [Stop PtyError, Embed IO] r =>
  IO a ->
  Sem r a
tryStop =
  stopNote CreationFailed <=< tryMaybe

acquirePty ::
  Member (Embed IO) r =>
  Sem (Stop PtyError : r) PtyResources
acquirePty = do
  (primary, secondary) <- tryStop openPseudoTerminal
  pty <- stopNote CreationFailed =<< tryStop (createPty secondary)
  handle <- tryStop (fdToHandle secondary)
  pure PtyResources {..}

releasePty ::
  Member (Embed IO) r =>
  PtyResources ->
  Sem r ()
releasePty PtyResources {primary, pty} = do
  embed (closePty pty)
  embed (closeFd primary)

withPty ::
  Members [Resource, Embed IO] r =>
  (PtyResources -> Sem (Stop PtyError : r) a) ->
  Sem (Stop PtyError : r) a
withPty =
  bracket acquirePty releasePty

-- |Interpret Pty as a 'System.Posix.Pty'.
interpretPty ::
  Members [Resource, Embed IO] r =>
  InterpreterFor (Scoped PtyResources Pty !! PtyError) r
interpretPty =
  interpretScopedResumable withPty \ PtyResources {..} -> \case
    Handle ->
      pure handle
    Resize rows cols ->
      embed (resizePty pty (fromIntegral rows, fromIntegral cols))
