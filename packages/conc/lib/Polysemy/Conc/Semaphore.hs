module Polysemy.Conc.Semaphore (
  Semaphore,
  interpretSemaphoreQ,
  interpretSemaphoreT,
  signal,
  wait,
) where

import Polysemy.Conc.Effect.Semaphore (Semaphore, signal, wait)
import Polysemy.Conc.Interpreter.Semaphore (interpretSemaphoreQ, interpretSemaphoreT)
