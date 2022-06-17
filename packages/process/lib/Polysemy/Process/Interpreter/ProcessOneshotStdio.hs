{-# options_haddock prune #-}

-- |Description: Process Interpreters for stdio, Internal
module Polysemy.Process.Interpreter.ProcessOneshotStdio where

import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Resume (type (!!))
import System.Process.Typed (ProcessConfig)

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessOptions (ProcessOptions)
import Polysemy.Process.Data.SystemProcessError (SystemProcessError)
import Polysemy.Process.Effect.Process (Process)
import Polysemy.Process.Interpreter.ProcessOneshot (
  interpretProcessOneshotByteString,
  interpretProcessOneshotByteStringLines,
  interpretProcessOneshotText,
  interpretProcessOneshotTextLines,
  )
import Polysemy.Process.Interpreter.SystemProcess (PipesProcess, interpretSystemProcessNative)

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing unaccumulated chunks of 'ByteString'.
-- Silently discards stderr.
interpretProcessOneshotByteStringNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString ByteString !! ProcessError)) r
interpretProcessOneshotByteStringNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessOneshotByteString @PipesProcess @SystemProcessError options .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing lines of 'ByteString'.
-- Silently discards stderr.
interpretProcessOneshotByteStringLinesNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString ByteString !! ProcessError)) r
interpretProcessOneshotByteStringLinesNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessOneshotByteStringLines @PipesProcess @SystemProcessError options .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing unaccumulated chunks of 'Text'.
-- Silently discards stderr.
interpretProcessOneshotTextNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process Text Text !! ProcessError)) r
interpretProcessOneshotTextNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessOneshotText @PipesProcess @SystemProcessError options .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing lines of 'Text'.
-- Silently discards stderr.
interpretProcessOneshotTextLinesNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  -- |Basic config. The pipes will be changed to 'System.IO.Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process Text Text !! ProcessError)) r
interpretProcessOneshotTextLinesNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessOneshotTextLines @PipesProcess @SystemProcessError options .
  raiseUnder
