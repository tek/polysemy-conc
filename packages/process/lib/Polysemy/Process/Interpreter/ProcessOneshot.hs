-- |Description: Process Interpreters, Internal
module Polysemy.Process.Interpreter.ProcessOneshot where

import Polysemy.Conc.Effect.PScoped (PScoped)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.PScoped (interpretResumablePScopedWith_)
import Polysemy.Resume (type (!!))

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessOptions (ProcessOptions)
import Polysemy.Process.Data.SystemProcessError (SystemProcessError)
import Polysemy.Process.Effect.Process (Process)
import Polysemy.Process.Effect.SystemProcess (SystemProcess)
import Polysemy.Process.Interpreter.Process (ScopeEffects, handleProcessWithQueues, pscope)
import Polysemy.Process.Interpreter.ProcessIO (ProcessIO)
import Polysemy.Process.Interpreter.SystemProcess (PipesProcess, SysProcConf, interpretSystemProcessNative)

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- deferring decoding of stdout and stderr to the interpreters of two 'ProcessOutput' effects.
-- Unlike 'Polysemy.Process.interpretProcess', this variant sends errors inside the scope to the individual 'Process'
-- actions.
-- This variant is for parameterized scopes, meaning that a value of arbitrary type may be passed to
-- 'Polysemy.Process.withProcessOneshotParam' which is then passed to the supplied function to produce a 'SysProcConf'
-- for the native process.
interpretProcessOneshot ::
  ∀ resource err param proc i o r .
  Members (ProcessIO i o) r =>
  Member (PScoped proc resource (SystemProcess !! err)) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  (param -> Sem r proc) ->
  InterpreterFor (PScoped param () (Process i o !! ProcessError)) r
interpretProcessOneshot options proc =
  interpretResumablePScopedWith_ @(ScopeEffects i o err) (\ p sem -> pscope @resource options proc p sem) handleProcessWithQueues

-- |Variant of 'interpretProcessOneshot' that takes a static 'SysProcConf'.
interpretProcessOneshot_ ::
  ∀ proc resource err i o r .
  Members (ProcessIO i o) r =>
  Member (PScoped proc resource (SystemProcess !! err)) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  proc ->
  InterpreterFor (Scoped () (Process i o !! ProcessError)) r
interpretProcessOneshot_ options proc =
  interpretProcessOneshot @resource @err options (const (pure proc))

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess'.
-- This variant is for parameterized scopes, meaning that a value of arbitrary type may be passed to
-- 'Polysemy.Process.withProcessOneshotParam' which is then passed to the supplied function to produce a 'SysProcConf'
-- for the native process.
interpretProcessOneshotNative ::
  ∀ param i o r .
  Members (ProcessIO i o) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  (param -> Sem r SysProcConf) ->
  InterpreterFor (PScoped param () (Process i o !! ProcessError)) r
interpretProcessOneshotNative options proc =
  interpretSystemProcessNative pure .
  interpretProcessOneshot @PipesProcess @SystemProcessError options (raise . proc) .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess'.
-- This variant takes a static 'SysProcConf'.
interpretProcessOneshotNative_ ::
  ∀ i o r .
  Members (ProcessIO i o) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  SysProcConf ->
  InterpreterFor (Scoped () (Process i o !! ProcessError)) r
interpretProcessOneshotNative_ options proc =
  interpretSystemProcessNative pure .
  interpretProcessOneshot @PipesProcess @SystemProcessError options (const (pure proc)) .
  raiseUnder
