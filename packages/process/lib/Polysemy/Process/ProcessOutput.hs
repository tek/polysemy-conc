{-# options_haddock prune #-}

-- |The utility effect 'ProcessOutput' takes care of decoding process output, getting called by the
-- 'Polysemy.Process.Process' interpreters whenever a chunk was read, while accumulating chunks until they were decoded
-- successfully.
module Polysemy.Process.ProcessOutput (
  module Polysemy.Process.Effect.ProcessOutput,
  module Polysemy.Process.Interpreter.ProcessOutput,
) where

import Polysemy.Process.Effect.ProcessOutput (ProcessOutput (Chunk), chunk)
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )
