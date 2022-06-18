module Polysemy.Conc.Interpreter.Semaphore where

import Control.Concurrent (QSem, newQSem, signalQSem, waitQSem)

import qualified Polysemy.Conc.Effect.Semaphore as Semaphore
import Polysemy.Conc.Effect.Semaphore (Semaphore)
import Control.Concurrent.STM.TSem (TSem, waitTSem, signalTSem, newTSem)
import Control.Concurrent.STM (atomically)

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

interpretSemaphoreQ ::
  Member (Embed IO) r =>
  Int ->
  InterpreterFor Semaphore r
interpretSemaphoreQ n sem = do
  qsem <- embed (newQSem n)
  interpretSemaphoreQWith qsem sem
{-# inline interpretSemaphoreQ #-}

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

interpretSemaphoreT ::
  Member (Embed IO) r =>
  Integer ->
  InterpreterFor Semaphore r
interpretSemaphoreT n sem = do
  qsem <- embed (atomically (newTSem n))
  interpretSemaphoreTWith qsem sem
{-# inline interpretSemaphoreT #-}
