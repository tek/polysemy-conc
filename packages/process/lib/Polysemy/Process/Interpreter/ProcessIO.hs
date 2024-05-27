-- | Interpreters for 'ProcessOutput' and 'ProcessInput', Internal
module Polysemy.Process.Interpreter.ProcessIO where

import Polysemy.Process.Effect.ProcessInput (ProcessInput)
import Polysemy.Process.Effect.ProcessOutput (OutputPipe (Stderr, Stdout), ProcessOutput)
import Polysemy.Process.Interpreter.ProcessInput (interpretProcessInputId, interpretProcessInputText)
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputIgnore,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )

-- | The effects used by 'Polysemy.Process.Process' to send and receive chunks of bytes to and from a process.
type ProcessIO i o =
  [
    ProcessInput i,
    ProcessOutput 'Stdout o,
    ProcessOutput 'Stderr o
  ]

-- | Interpret 'ProcessIO' with plain 'ByteString's without chunking.
-- Silently discards stderr.
interpretProcessByteString ::
  InterpretersFor (ProcessIO ByteString ByteString) r
interpretProcessByteString =
  interpretProcessOutputIgnore @'Stderr .
  interpretProcessOutputId @'Stdout .
  interpretProcessInputId

-- | Interpret 'ProcessIO' with 'ByteString's chunked as lines.
-- Silently discards stderr.
interpretProcessByteStringLines ::
  InterpretersFor (ProcessIO ByteString ByteString) r
interpretProcessByteStringLines =
  interpretProcessOutputIgnore @'Stderr .
  interpretProcessOutputLines @'Stdout .
  interpretProcessInputId

-- | Interpret 'ProcessIO' with plain 'Text's without chunking.
-- Silently discards stderr.
interpretProcessText ::
  InterpretersFor (ProcessIO Text Text) r
interpretProcessText =
  interpretProcessOutputIgnore @'Stderr .
  interpretProcessOutputText @'Stdout .
  interpretProcessInputText

-- | Interpret 'ProcessIO' with 'Text's chunked as lines.
-- Silently discards stderr.
interpretProcessTextLines ::
  InterpretersFor (ProcessIO Text Text) r
interpretProcessTextLines =
  interpretProcessOutputIgnore @'Stderr .
  interpretProcessOutputTextLines @'Stdout .
  interpretProcessInputText
