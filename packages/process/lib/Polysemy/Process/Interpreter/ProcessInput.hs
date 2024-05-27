{-# options_haddock prune #-}

-- | Description: ProcessInput Interpreters, Internal
module Polysemy.Process.Interpreter.ProcessInput where

import Polysemy.Process.Effect.ProcessInput (ProcessInput (Encode))

-- | Interpret 'ProcessInput' by passing 'ByteString' through.
interpretProcessInputId :: InterpreterFor (ProcessInput ByteString) r
interpretProcessInputId =
  interpret \case
    Encode value ->
      pure value
{-# inline interpretProcessInputId #-}

-- | Interpret 'ProcessInput' by UTF-8-encoding 'Text'.
interpretProcessInputText :: InterpreterFor (ProcessInput Text) r
interpretProcessInputText =
  interpret \case
    Encode value ->
      pure (encodeUtf8 value)
{-# inline interpretProcessInputText #-}
