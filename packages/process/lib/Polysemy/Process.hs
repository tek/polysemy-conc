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
  interpretProcessIOE,
) where

import Polysemy.Process.Effect.Process (Process, recv, recvError, send, withProcess)
import Polysemy.Process.Interpreter.Process (interpretProcessNative)
import Polysemy.Process.Interpreter.ProcessIOE (interpretProcessIOE)

-- $intro
-- This library provides an abstraction of a system process in the effect 'Process', whose constructors represent the
-- three standard file descriptors.
--
-- The values produced by the constructors are chunks of the process' output when using the default interpreter.
