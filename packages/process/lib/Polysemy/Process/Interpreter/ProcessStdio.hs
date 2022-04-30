{-# options_haddock prune #-}

-- |Description: Process Interpreters for stdio, Internal
module Polysemy.Process.Interpreter.ProcessStdio where

import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Resume (type (!!))
import System.Process.Typed (ProcessConfig)

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessOptions (ProcessOptions)
import Polysemy.Process.Data.SystemProcessError (SystemProcessError)
import Polysemy.Process.Effect.Process (Process)
import Polysemy.Process.Interpreter.Process (
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,
  )
import Polysemy.Process.Interpreter.SystemProcess (PipesProcess, interpretSystemProcessNative)

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing unaccumulated chunks of 'ByteString'.
interpretProcessByteStringNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  ProcessOptions ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString ByteString ByteString) !! ProcessError) r
interpretProcessByteStringNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessByteString @PipesProcess @SystemProcessError options .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing lines of 'ByteString'.
interpretProcessByteStringLinesNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString ByteString ByteString) !! ProcessError) r
interpretProcessByteStringLinesNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessByteStringLines @PipesProcess @SystemProcessError options .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing unaccumulated chunks of 'Text'.
interpretProcessTextNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString Text Text) !! ProcessError) r
interpretProcessTextNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessText @PipesProcess @SystemProcessError options .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing lines of 'Text'.
interpretProcessTextLinesNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString Text Text) !! ProcessError) r
interpretProcessTextLinesNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessTextLines @PipesProcess @SystemProcessError options .
  raiseUnder
