module Polysemy.Process.Interpreter.ProcessOutput where

import Polysemy.Process.Effect.ProcessOutput (ProcessOutput (Chunk))
import qualified Data.ByteString as ByteString
import Data.Foldable (foldr')

interpretProcessOutputId :: InterpreterFor (ProcessOutput ByteString) r
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

interpretProcessOutputLines :: InterpreterFor (ProcessOutput ByteString) r
interpretProcessOutputLines =
  interpret \case
    Chunk buffer new ->
      pure (splitLines buffer new)
{-# inline interpretProcessOutputLines #-}

interpretProcessOutputText :: InterpreterFor (ProcessOutput Text) r
interpretProcessOutputText =
  interpret \case
    Chunk buffer new ->
      pure ([decodeUtf8 (buffer <> new)], "")
{-# inline interpretProcessOutputText #-}

interpretProcessOutputTextLines :: InterpreterFor (ProcessOutput Text) r
interpretProcessOutputTextLines =
  interpret \case
    Chunk buffer new ->
      pure (first (fmap decodeUtf8) (splitLines buffer new))
{-# inline interpretProcessOutputTextLines #-}
