{-# options_haddock prune #-}

-- |Description: Process Interpreters for stdio, Internal
module Polysemy.Process.Interpreter.ProcessStdio where

import Polysemy.Async (Async)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Resource (Resource)
import Polysemy.Resume (type (!!))
import System.Process.Typed (ProcessConfig)

import Polysemy.Process.Data.ProcessError (ProcessError)
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
  Bool ->
  -- |Maximum number of chunks allowed to be queued for each of the three standard pipes.
  Int ->
  -- |Basic config. The pipes will be changed to 'Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString ByteString ByteString) !! ProcessError) r
interpretProcessByteStringNative discard qSize conf =
  interpretSystemProcessNative conf .
  interpretProcessByteString @PipesProcess @SystemProcessError discard qSize .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing lines of 'ByteString'.
interpretProcessByteStringLinesNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued for each of the three standard pipes.
  Int ->
  -- |Basic config. The pipes will be changed to 'Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString ByteString ByteString) !! ProcessError) r
interpretProcessByteStringLinesNative discard qSize conf =
  interpretSystemProcessNative conf .
  interpretProcessByteStringLines @PipesProcess @SystemProcessError discard qSize .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing unaccumulated chunks of 'Text'.
interpretProcessTextNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued for each of the three standard pipes.
  Int ->
  -- |Basic config. The pipes will be changed to 'Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString Text Text) !! ProcessError) r
interpretProcessTextNative discard qSize conf =
  interpretSystemProcessNative conf .
  interpretProcessText @PipesProcess @SystemProcessError discard qSize .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess', producing lines of 'Text'.
interpretProcessTextLinesNative ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued for each of the three standard pipes.
  Int ->
  -- |Basic config. The pipes will be changed to 'Handle' by the interpreter.
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process ByteString Text Text) !! ProcessError) r
interpretProcessTextLinesNative discard qSize conf =
  interpretSystemProcessNative conf .
  interpretProcessTextLines @PipesProcess @SystemProcessError discard qSize .
  raiseUnder
