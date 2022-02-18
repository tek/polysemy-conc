{-# options_haddock prune #-}

-- |The effect 'Pty' abstracts pseudo terminals.
module Polysemy.Process.Pty (
  module Polysemy.Process.Effect.Pty,
  module Polysemy.Process.Interpreter.Pty,
) where

import Polysemy.Process.Effect.Pty (Pty (..), handle, resize, size, withPty)
import Polysemy.Process.Interpreter.Pty (interpretPty)
