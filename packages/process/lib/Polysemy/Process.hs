-- |Description: Polysemy Effects for System Processes
module Polysemy.Process (
  -- * Introduction
  -- $intro

  -- * Effects
  -- ** Process
  Process,
  recv,
  send,
  withProcess,
  withProcess_,
  withProcessOneshot,
  withProcessOneshot_,
  ProcessOptions (ProcessOptions),
  ProcessKill (..),
  ProcessError (..),

  -- ** ProcessOutput
  ProcessOutput,
  OutputPipe (Stdout, Stderr),
  ProcessOutputParseResult (..),

  -- ** ProcessInput
  ProcessInput,

  -- ** SystemProcess
  SystemProcess,
  withSystemProcess,
  withSystemProcess_,

  -- ** Pty
  Pty,
  withPty,

  -- * Interpreters
  -- ** Process
  interpretProcess,
  interpretProcessOneshot,
  interpretProcessNative,
  interpretProcessOneshotNative,
  interpretProcess_,
  interpretProcessOneshot_,
  interpretProcessNative_,
  interpretProcessOneshotNative_,
  interpretInputOutputProcess,
  interpretInputHandleBuffered,
  interpretInputHandle,
  interpretOutputHandleBuffered,
  interpretOutputHandle,
  interpretProcessIO,
  interpretProcessHandles,
  interpretProcessCurrent,

  -- ** ProcessIO
  ProcessIO,
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,

  -- ** ProcessOutput
  interpretProcessOutputIgnore,
  interpretProcessOutputId,
  interpretProcessOutputLeft,
  interpretProcessOutputRight,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  interpretProcessOutputIncremental,

  -- ** ProcessInput
  interpretProcessInputId,
  interpretProcessInputText,

  -- ** SystemProcess
  SysProcConf,
  PipesProcess,
  SystemProcessError,
  interpretSystemProcessNative,
  interpretSystemProcessNative_,
  interpretSystemProcessWithProcess,
  handleSystemProcessWithProcess,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessWithProcessOpaque,
  interpretSystemProcessNativeOpaqueSingle,
  interpretSystemProcessNativeOpaque,

  -- ** Pty
  interpretPty,

  -- * Tools
  resolveExecutable,
) where

import Prelude hiding (send)

import Polysemy.Process.Data.ProcessError (ProcessError (..))
import Polysemy.Process.Data.ProcessKill (ProcessKill (..))
import Polysemy.Process.Data.ProcessOptions (ProcessOptions (ProcessOptions))
import Polysemy.Process.Data.ProcessOutputParseResult (ProcessOutputParseResult (..))
import Polysemy.Process.Data.SystemProcessError (SystemProcessError)
import Polysemy.Process.Effect.Process (Process (..), recv, send, withProcess, withProcessOneshot, withProcessOneshot_, withProcess_)
import Polysemy.Process.Effect.ProcessInput (ProcessInput)
import Polysemy.Process.Effect.ProcessOutput (OutputPipe (Stderr, Stdout), ProcessOutput)
import Polysemy.Process.Effect.Pty (Pty, withPty)
import Polysemy.Process.Effect.SystemProcess (SystemProcess, withSystemProcess, withSystemProcess_)
import Polysemy.Process.Executable (resolveExecutable)
import Polysemy.Process.Interpreter.Process (
  interpretInputHandle,
  interpretInputHandleBuffered,
  interpretInputOutputProcess,
  interpretOutputHandle,
  interpretOutputHandleBuffered,
  interpretProcess,
  interpretProcessCurrent,
  interpretProcessHandles,
  interpretProcessIO,
  interpretProcessNative,
  interpretProcessNative_,
  interpretProcess_,
  )
import Polysemy.Process.Interpreter.ProcessIO (
  ProcessIO,
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,
  )
import Polysemy.Process.Interpreter.ProcessInput (interpretProcessInputId, interpretProcessInputText)
import Polysemy.Process.Interpreter.ProcessOneshot (
  interpretProcessOneshot,
  interpretProcessOneshotNative,
  interpretProcessOneshotNative_,
  interpretProcessOneshot_,
  )
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputIgnore,
  interpretProcessOutputIncremental,
  interpretProcessOutputLeft,
  interpretProcessOutputLines,
  interpretProcessOutputRight,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )
import Polysemy.Process.Interpreter.Pty (interpretPty)
import Polysemy.Process.Interpreter.SystemProcess (
  PipesProcess,
  SysProcConf,
  handleSystemProcessWithProcess,
  interpretSystemProcessNative,
  interpretSystemProcessNativeOpaque,
  interpretSystemProcessNativeOpaqueSingle,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessNative_,
  interpretSystemProcessWithProcess,
  interpretSystemProcessWithProcessOpaque,
  )

-- $intro
-- This library provides an abstraction of a system process in the effect 'Process', whose constructors represent the
-- three standard file descriptors.
--
-- An intermediate effect, 'SystemProcess', is more concretely tied to the functionality of the "System.Process"
-- library.
-- See "Polysemy.Process.SystemProcess" for its constructors.
--
-- The utility effect 'ProcessOutput' takes care of decoding the process output, getting called by the 'Process'
-- interpreters whenever a chunk was read, while accumulating chunks until they were decoded successfully.
-- See "Polysemy.Process.ProcessOutput" for its constructors.
--
-- The effect 'Pty' abstracts pseudo terminals.
-- See "Polysemy.Process.Pty" for its constructors.
