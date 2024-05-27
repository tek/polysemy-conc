-- | Semaphore interpreters, Internal.
module Polysemy.Conc.Interpreter.Semaphore where

import Control.Concurrent (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)

import qualified Polysemy.Conc.Effect.Semaphore as Semaphore
import Polysemy.Conc.Effect.Semaphore (Semaphore)

-- | Interpret 'Semaphore' using the supplied 'QSem'.
interpretSemaphoreQWith ::
  Member (Embed IO) r =>
  QSem ->
  InterpreterFor Semaphore r
interpretSemaphoreQWith qsem =
  interpret \case
    Semaphore.Wait ->
      embed (waitQSem qsem)
    Semaphore.Signal ->
      embed (signalQSem qsem)
{-# inline interpretSemaphoreQWith #-}

-- | Interpret 'Semaphore' as a 'QSem'.
interpretSemaphoreQ ::
  Member (Embed IO) r =>
  Int ->
  InterpreterFor Semaphore r
interpretSemaphoreQ n sem = do
  qsem <- embed (newQSem n)
  interpretSemaphoreQWith qsem sem
{-# inline interpretSemaphoreQ #-}

-- | Interpret 'Semaphore' using the supplied 'TSem'.
interpretSemaphoreTWith ::
  Member (Embed IO) r =>
  TSem ->
  InterpreterFor Semaphore r
interpretSemaphoreTWith qsem =
  interpret \case
    Semaphore.Wait ->
      embed (atomically (waitTSem qsem))
    Semaphore.Signal ->
      embed (atomically (signalTSem qsem))
{-# inline interpretSemaphoreTWith #-}

-- | Interpret 'Semaphore' as a 'TSem'.
interpretSemaphoreT ::
  Member (Embed IO) r =>
  Integer ->
  InterpreterFor Semaphore r
interpretSemaphoreT n sem = do
  qsem <- embed (atomically (newTSem n))
  interpretSemaphoreTWith qsem sem
{-# inline interpretSemaphoreT #-}
