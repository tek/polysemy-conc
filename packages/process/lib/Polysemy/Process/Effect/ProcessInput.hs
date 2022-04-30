{-# options_haddock prune #-}

-- |Description: ProcessInput effect, Internal.
module Polysemy.Process.Effect.ProcessInput where

-- |This effect is used by the effect 'Polysemy.Process.Process' to encode values for process input.
-- example using a parser.
data ProcessInput a :: Effect where
  -- |Encode a value for enqueueing it to a process' stdin.
  Encode ::
    -- |The value to encode.
    a ->
    ProcessInput a m ByteString

makeSem ''ProcessInput
