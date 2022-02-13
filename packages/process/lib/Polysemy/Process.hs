-- |Description: Polysemy Effects for System Processes
module Polysemy.Process (
  -- * Introduction
  -- $intro

  -- * Effects
  -- ** Process
  Process,
  recv,
  recvError,
  send,
  withProcess,

  -- ** SystemProcess
  SystemProcess,
  readStdout,
  readStderr,
  writeStdin,
  pid,
  signal,
  wait,
  withSystemProcess,
  interrupt,

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
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,

  -- ** SystemProcess
  interpretSystemProcessWithProcess,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessNative,

  -- * Tools
  resolveExecutable,
) where

import Polysemy.Process.Effect.Process (Process, recv, recvError, send, withProcess)
import Polysemy.Process.Effect.SystemProcess (
  SystemProcess,
  interrupt,
  pid,
  readStderr,
  readStdout,
  signal,
  wait,
  withSystemProcess,
  writeStdin,
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
import Polysemy.Process.Interpreter.SystemProcess (
  interpretSystemProcessNative,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessWithProcess,
  )

-- $intro
-- This library provides an abstraction of a system process in the effect 'Process', whose constructors represent the
-- three standard file descriptors.
--
-- The values produced by the constructors are chunks of the process' output when using the default interpreter.
