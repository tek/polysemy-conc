-- |Gate interpreters, Internal
module Polysemy.Conc.Interpreter.Gate where

import Polysemy.Conc.Effect.Gate (Gate (Gate, Signal))
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedAs)

-- |Interpret 'Gate' with an 'MVar'.
interpretGate ::
  ∀ r .
  Member (Embed IO) r =>
  InterpreterFor Gate r
interpretGate sem = do
  mv <- embed newEmptyMVar
  int mv sem
  where
    int :: MVar () -> InterpreterFor Gate r
    int mv =
      interpret \case
        Signal ->
          void (embed (tryPutMVar mv ()))
        Gate ->
          embed (readMVar mv)

-- |Interpret @'Scoped' ('MVar' ()) 'Gate'@.
interpretGates ::
  Member (Embed IO) r =>
  InterpreterFor (Scoped (MVar ()) Gate) r
interpretGates =
  interpretScopedAs (embed newEmptyMVar) \ mv -> \case
    Signal ->
      void (embed (tryPutMVar mv ()))
    Gate ->
      embed (readMVar mv)
