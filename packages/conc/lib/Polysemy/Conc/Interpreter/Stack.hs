{-# options_haddock prune #-}

-- | Description: Convenience Interpreters for all Conc Effects, Internal
module Polysemy.Conc.Interpreter.Stack where

import Polysemy.Conc.Effect.Gate (Gates)
import Polysemy.Conc.Effect.Mask (Mask, UninterruptibleMask)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Interpreter.Gate (interpretGates)
import Polysemy.Conc.Interpreter.Mask (interpretMaskFinal, interpretUninterruptibleMaskFinal)
import Polysemy.Conc.Interpreter.Race (interpretRace)

-- | A default basic stack with 'Final' for _polysemy-conc_.
type ConcStack =
  [
    UninterruptibleMask,
    Mask,
    Gates,
    Race,
    Async,
    Resource,
    Embed IO,
    Final IO
  ]

-- | Interprets 'UninterruptibleMask', 'Mask' and 'Race' in terms of @'Final' 'IO'@ and runs the entire rest of the
-- stack.
runConc ::
  Sem ConcStack a ->
  IO a
runConc =
  runFinal .
  embedToFinal @IO .
  resourceToIOFinal .
  asyncToIOFinal .
  interpretRace .
  interpretGates .
  interpretMaskFinal .
  interpretUninterruptibleMaskFinal
