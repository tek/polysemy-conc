-- |Description: Process Interpreters, Internal
module Polysemy.Process.Interpreter.ProcessOneshot where

import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped_)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedRWith_)
import Polysemy.Resume (Stop, type (!!))

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessOptions (ProcessOptions)
import Polysemy.Process.Data.SystemProcessError (SystemProcessError, SystemProcessScopeError)
import Polysemy.Process.Effect.Process (Process)
import Polysemy.Process.Effect.SystemProcess (SystemProcess)
import Polysemy.Process.Interpreter.Process (ScopeEffects, handleProcessWithQueues, pscope, terminated)
import Polysemy.Process.Interpreter.ProcessIO (ProcessIO)
import Polysemy.Process.Interpreter.SystemProcess (PipesProcess, SysProcConf, interpretSystemProcessNative)

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three
-- 'Control.Concurrent.STM.TBMQueue.TBMQueue's, deferring decoding of stdout and stderr to the interpreters of two
-- 'Polysemy.Process.ProcessOutput' effects.
-- Unlike 'Polysemy.Process.interpretProcess', this variant sends errors inside the scope to the individual 'Process'
-- actions.
-- This variant is for parameterized scopes, meaning that a value of arbitrary type may be passed to
-- 'Polysemy.Process.withProcessOneshotParam' which is then passed to the supplied function to produce a 'SysProcConf'
-- for the native process.
interpretProcessOneshot ::
  ∀ resource param proc i o r .
  Members (ProcessIO i o) r =>
  Member (Scoped proc resource (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  (param -> Sem (Stop SystemProcessScopeError : r) proc) ->
  InterpreterFor (Scoped param () (Process i o !! ProcessError) !! SystemProcessScopeError) r
interpretProcessOneshot options proc =
  interpretScopedRWith_ @(ScopeEffects i o SystemProcessError)
  (\ p -> pscope @resource @SystemProcessScopeError options proc p)
  (handleProcessWithQueues terminated)

-- |Variant of 'interpretProcessOneshot' that takes a static 'SysProcConf'.
interpretProcessOneshot_ ::
  ∀ proc resource i o r .
  Members (ProcessIO i o) r =>
  Member (Scoped proc resource (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  proc ->
  InterpreterFor (Scoped_ () (Process i o !! ProcessError) !! SystemProcessScopeError) r
interpretProcessOneshot_ options proc =
  interpretProcessOneshot @resource options (const (pure proc))

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
  InterpreterFor (Scoped param () (Process i o !! ProcessError) !! SystemProcessScopeError) r
interpretProcessOneshotNative options proc =
  interpretSystemProcessNative pure .
  interpretProcessOneshot @PipesProcess options (insertAt @0 . proc) .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess'.
-- This variant takes a static 'SysProcConf'.
interpretProcessOneshotNative_ ::
  ∀ i o r .
  Members (ProcessIO i o) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  SysProcConf ->
  InterpreterFor (Scoped_ () (Process i o !! ProcessError) !! SystemProcessScopeError) r
interpretProcessOneshotNative_ options proc =
  interpretSystemProcessNative pure .
  interpretProcessOneshot @PipesProcess options (const (pure proc)) .
  raiseUnder
