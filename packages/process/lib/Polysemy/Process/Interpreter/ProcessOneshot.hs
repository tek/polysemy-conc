-- |Description: Process Interpreters, Internal
module Polysemy.Process.Interpreter.ProcessOneshot where

import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Scoped (interpretResumableScopedWith_)
import Polysemy.Resume (type (!!))
import Prelude hiding (fromException)

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessOptions (ProcessOptions)
import Polysemy.Process.Effect.Process (Process)
import Polysemy.Process.Effect.ProcessInput (ProcessInput)
import Polysemy.Process.Effect.ProcessOutput (OutputPipe (Stderr, Stdout), ProcessOutput)
import Polysemy.Process.Effect.SystemProcess (SystemProcess)
import Polysemy.Process.Interpreter.Process (ScopeEffects, handleProcessWithQueues, scope)
import Polysemy.Process.Interpreter.ProcessInput (interpretProcessInputId, interpretProcessInputText)
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputIgnore,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- deferring decoding of stdout and stderr to the interpreters of two 'ProcessOutput' effects.
-- Unlike 'Polysemy.Process.interpretProcess', this variant sends errors inside the scope to the individual 'Process'
-- actions.
interpretProcessOneshot ::
  ∀ resource err i o r .
  Member (Scoped resource (SystemProcess !! err)) r =>
  Members [ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, ProcessInput i, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process i o !! ProcessError)) r
interpretProcessOneshot options =
  interpretResumableScopedWith_ @(ScopeEffects i o err) (scope @i @o @resource options) handleProcessWithQueues

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing 'ByteString's.
-- Silently discards stderr.
interpretProcessOneshotByteString ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process ByteString ByteString !! ProcessError)) r
interpretProcessOneshotByteString options =
  interpretProcessOutputIgnore @'Stderr @ByteString .
  interpretProcessOutputId @'Stdout .
  interpretProcessInputId .
  interpretProcessOneshot @resource @err options .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing chunks of lines of 'ByteString's.
-- Silently discards stderr.
interpretProcessOneshotByteStringLines ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process ByteString ByteString !! ProcessError)) r
interpretProcessOneshotByteStringLines options =
  interpretProcessOutputIgnore @'Stderr @ByteString .
  interpretProcessOutputLines @'Stdout .
  interpretProcessInputId .
  interpretProcessOneshot @resource @err options .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing 'Text's.
-- Silently discards stderr.
interpretProcessOneshotText ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process Text Text !! ProcessError)) r
interpretProcessOneshotText options =
  interpretProcessOutputIgnore @'Stderr @Text .
  interpretProcessOutputText @'Stdout .
  interpretProcessInputText .
  interpretProcessOneshot @resource @err options .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing chunks of lines of 'Text's.
-- Silently discards stderr.
interpretProcessOneshotTextLines ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process Text Text !! ProcessError)) r
interpretProcessOneshotTextLines options =
  interpretProcessOutputIgnore @'Stderr @Text .
  interpretProcessOutputTextLines @'Stdout .
  interpretProcessInputText .
  interpretProcessOneshot @resource @err options .
  raiseUnder3
