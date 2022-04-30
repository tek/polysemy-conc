{-# options_haddock prune #-}

-- |Description: ProcessOutput effect, Internal.
module Polysemy.Process.Effect.ProcessOutput where

-- |Kind tag for selecting the 'ProcessOutput' handler for stdout/stderr.
data OutputPipe =
  -- |Tag for stdout.
  Stdout
  |
  -- |Tag for stderr.
  Stderr
  deriving stock (Eq, Show)

-- |This effect is used by the effect 'Polysemy.Process.Process' to accumulate and decode chunks of 'ByteString's, for
-- example using a parser.
-- The interpreter may be stateful or stateless, since the constructor 'Chunk' is expected to be called with both the
-- accumulated unprocessed output as well as the new chunk.
data ProcessOutput (p :: OutputPipe) a :: Effect where
  -- |Add a chunk of output to the accumulator, returning any number of successfully parsed values and the leftover
  -- output.
  Chunk ::
    -- |The accumulation of the previous leftovers.
    ByteString ->
    -- |The new chunk read from the process.
    ByteString ->
    ProcessOutput p a m ([a], ByteString)

makeSem ''ProcessOutput
