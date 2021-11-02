module Polysemy.Process.Interpreter.Process where

import Polysemy.Async (Async)
import Polysemy.Conc (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Scoped (runScoped)
import Polysemy.Error (fromException, runError)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Resume (type (!!))
import Prelude hiding (fromException)
import qualified System.Process.Typed as System
import System.Process.Typed (
  ProcessConfig,
  startProcess,
  stopProcess,
  )

import Polysemy.Process.Effect.Process (Process)

withProcess ::
  Members [Resource, Embed IO] r =>
  ProcessConfig stdin stdout stderr ->
  (System.Process stdin stdout stderr -> Sem r a) ->
  Sem r a
withProcess config =
  bracket (embed @IO (startProcess config)) (runError @SomeException . fromException @SomeException . stopProcess)

-- |Interpret 'Process' with a system process resource.
interpretProcessNative ::
  ∀ resource i o e err stdin stdout stderr r .
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessConfig stdin stdout stderr ->
  (∀ x. System.Process stdin stdout stderr -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor (Process i o e !! err) r) ->
  InterpreterFor (Scoped resource (Process i o e !! err)) r
interpretProcessNative config fromProcess =
  runScoped \ f ->
    withProcess config \ prc ->
      fromProcess prc f
