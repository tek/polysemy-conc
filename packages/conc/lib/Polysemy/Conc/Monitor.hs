module Polysemy.Conc.Monitor where

import Polysemy.Time (TimeUnit, convert, Time)

import Polysemy.Conc.Effect.Monitor (MonitorCheck (MonitorCheck))
import qualified Polysemy.Time as Time
import Torsor (minus, difference, Torsor)

-- |Check for 'Polysemy.Conc.Effect.Monitor' that checks every @interval@ whether the difference between the current
-- time and the time at the last check is larger than @interval@ + @tolerance@.
-- Can be used to detect that the operating system suspended and resumed.
monitorClockSkew ::
  ∀ t d diff u1 u2 r .
  Ord diff =>
  Torsor t diff =>
  TimeUnit u1 =>
  TimeUnit u2 =>
  TimeUnit diff =>
  Members [AtomicState (Maybe t), Time t d, Embed IO] r =>
  u1 ->
  u2 ->
  MonitorCheck r
monitorClockSkew interval (convert -> tolerance) =
  MonitorCheck (convert interval) \ signal -> do
    atomicGet >>= \case
      Just prev -> do
        now <- Time.now @t @d
        when (minus (difference now prev) tolerance > convert interval) (void (embed @IO (tryPutMVar signal ())))
        atomicPut (Just now)
      Nothing -> do
        now <- Time.now @t @d
        atomicPut (Just now)
