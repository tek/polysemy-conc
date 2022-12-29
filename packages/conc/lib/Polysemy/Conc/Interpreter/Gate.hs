-- |Gate interpreters, Internal
module Polysemy.Conc.Interpreter.Gate where

import Polysemy.Conc.Effect.Gate (Gate (Gate, Signal))

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

-- |Interpret @'Scoped_' 'Gate'@ with an @'MVar' ()@.
interpretGates ::
  Member (Embed IO) r =>
  InterpreterFor (Scoped_ Gate) r
interpretGates =
  interpretScopedAs @(MVar ()) (const (embed newEmptyMVar)) \ mv -> \case
    Signal ->
      void (embed (tryPutMVar mv ()))
    Gate ->
      embed (readMVar mv)
