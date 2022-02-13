{-# options_haddock prune #-}

-- |Description: ProcessOutput effect, Internal.
module Polysemy.Process.Effect.ProcessOutput where

-- |This effect is used by the effect 'Polysemy.Process.Process' to accumulate and decode chunks of 'ByteString's, for
-- example using a parser.
-- The interpreter may be stateful or stateless, since the constructor 'Chunk' is expected to be called with both the
-- accumulated unprocessed output as well as the new chunk.
data ProcessOutput a :: Effect where
  -- |Add a chunk of output to the accumulator, returning any number of successfully parsed values and the leftover
  -- output.
  Chunk ::
    -- |The accumulation of the previous leftovers.
    ByteString ->
    -- |The new chunk read from the process.
    ByteString ->
    ProcessOutput a m ([a], ByteString)

makeSem ''ProcessOutput
