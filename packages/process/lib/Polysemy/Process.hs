-- |Description: Polysemy Effects for System Processes
module Polysemy.Process (
  -- * Introduction
  -- $intro

  -- * Effects
  -- ** Process
  Process (..),
  recv,
  recvError,
  send,
  withProcess,

  -- ** ProcessOutput
  ProcessOutput,

  -- ** SystemProcess
  SystemProcess,
  withSystemProcess,

  -- ** Pty
  Pty,
  withPty,

  -- * Interpreters
  -- ** Process
  interpretProcessByteStringNative,
  interpretProcessByteStringLinesNative,
  interpretProcessTextNative,
  interpretProcessTextLinesNative,
  interpretProcess,
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,

  -- ** ProcessOutput
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,

  -- ** SystemProcess
  interpretSystemProcessWithProcess,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessNative,

  -- ** Pty
  interpretPty,

  -- * Tools
  resolveExecutable,
) where

import Polysemy.Process.Effect.Process (Process (..), recv, recvError, send, withProcess)
import Polysemy.Process.Effect.ProcessOutput (ProcessOutput)
import Polysemy.Process.Effect.Pty (Pty, withPty)
import Polysemy.Process.Effect.SystemProcess (
  SystemProcess, withSystemProcess
  )
import Polysemy.Process.Executable (resolveExecutable)
import Polysemy.Process.Interpreter.Process (
  interpretProcess,
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,
  )
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )
import Polysemy.Process.Interpreter.ProcessStdio (
  interpretProcessByteStringLinesNative,
  interpretProcessByteStringNative,
  interpretProcessTextLinesNative,
  interpretProcessTextNative,
  )
import Polysemy.Process.Interpreter.Pty (interpretPty)
import Polysemy.Process.Interpreter.SystemProcess (
  interpretSystemProcessNative,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessWithProcess,
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
