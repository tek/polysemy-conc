{-# options_haddock prune #-}

-- |Description: ProcessOutput Interpreters, Internal
module Polysemy.Process.Interpreter.ProcessOutput where

import qualified Data.ByteString as ByteString

import Polysemy.Process.Effect.ProcessOutput (ProcessOutput (Chunk))

-- |Interpret 'ProcessOutput' by discarding any output.
interpretProcessOutputIgnore ::
  ∀ p a r .
  InterpreterFor (ProcessOutput p a) r
interpretProcessOutputIgnore =
  interpret \case
    Chunk _ _ ->
      pure ([], "")
{-# inline interpretProcessOutputIgnore #-}

-- |Interpret 'ProcessOutput' by immediately emitting raw 'ByteString's without accumulation.
interpretProcessOutputId ::
  ∀ p r .
  InterpreterFor (ProcessOutput p ByteString) r
interpretProcessOutputId =
  interpret \case
    Chunk buffer new ->
      pure ([buffer <> new], "")
{-# inline interpretProcessOutputId #-}

splitLines :: ByteString -> ByteString -> ([ByteString], ByteString)
splitLines buffer new =
  second fold (foldr' folder ([], Nothing) parts)
  where
    parts =
      ByteString.split 10 (buffer <> new)
    folder a (z, Nothing) =
      (z, Just a)
    folder a (z, Just r) =
      (a : z, Just r)

-- |Interpret 'ProcessOutput' by emitting individual 'ByteString' lines of output.
interpretProcessOutputLines ::
  ∀ p r .
  InterpreterFor (ProcessOutput p ByteString) r
interpretProcessOutputLines =
  interpret \case
    Chunk buffer new ->
      pure (splitLines buffer new)
{-# inline interpretProcessOutputLines #-}

-- |Interpret 'ProcessOutput' by immediately emitting 'Text' without accumulation.
interpretProcessOutputText ::
  ∀ p r .
  InterpreterFor (ProcessOutput p Text) r
interpretProcessOutputText =
  interpret \case
    Chunk buffer new ->
      pure ([decodeUtf8 (buffer <> new)], "")
{-# inline interpretProcessOutputText #-}

-- |Interpret 'ProcessOutput' by emitting individual 'Text' lines of output.
interpretProcessOutputTextLines ::
  ∀ p r .
  InterpreterFor (ProcessOutput p Text) r
interpretProcessOutputTextLines =
  interpret \case
    Chunk buffer new ->
      pure (first (fmap decodeUtf8) (splitLines buffer new))
{-# inline interpretProcessOutputTextLines #-}
