-- |Description: Polysemy Effects for System Processes
module Polysemy.Process (
  -- * Introduction
  -- $intro

  -- * Effect
  Process,
  recv,
  recvError,
  send,
  withProcess,

  -- * Interpreters
  interpretProcessNative,
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,

  -- * Tools
  resolveExecutable,
) where

import Polysemy.Process.Effect.Process (Process, recv, recvError, send, withProcess)
import Polysemy.Process.Executable (resolveExecutable)
import Polysemy.Process.Interpreter.Process (interpretProcessNative)
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )
import Polysemy.Process.Interpreter.ProcessStd (
  interpretProcessByteString,
  interpretProcessByteStringLines,
  interpretProcessText,
  interpretProcessTextLines,
  )

-- $intro
-- This library provides an abstraction of a system process in the effect 'Process', whose constructors represent the
-- three standard file descriptors.
--
-- The values produced by the constructors are chunks of the process' output when using the default interpreter.
