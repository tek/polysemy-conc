-- |Description: Action Retrying
module Polysemy.Conc.Retry where

import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)

import Polysemy.Conc.Data.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import qualified Polysemy.Conc.Race as Race

-- |Run an action repeatedly until it returns 'Right' or the timout has been exceeded.
retrying ::
  ∀ e w u t d r a .
  TimeUnit w =>
  TimeUnit u =>
  Members [Race, Time t d] r =>
  -- |The timeout after which the attempt is abandoned.
  w ->
  -- |The waiting interval between two tries.
  u ->
  Sem r (Either e a) ->
  Sem r (Maybe a)
retrying timeout interval action =
  Race.timeout_ Nothing timeout (Just <$> spin)
  where
    spin =
      action >>= \case
        Right a ->
          pure a
        Left _ -> do
          Time.sleep @t @d interval
          spin

-- |Run an action repeatedly until it returns 'Right' or the timout has been exceeded.
--
-- If the action failed at least once, the last error will be returned in case of timeout.
retryingWithError ::
  ∀ e w u t d r a .
  TimeUnit w =>
  TimeUnit u =>
  Members [Race, Time t d, Embed IO] r =>
  -- |The timeout after which the attempt is abandoned.
  w ->
  -- |The waiting interval between two tries.
  u ->
  Sem r (Either e a) ->
  Sem r (Maybe (Either e a))
retryingWithError timeout interval action =
  interpretSync @e do
    Race.timeout_ Nothing timeout (Just <$> spin) >>= \case
      Just a -> pure (Just (Right a))
      Nothing -> fmap Left <$> Sync.takeTry
  where
    spin =
      raise action >>= \case
        Right a ->
          pure a
        Left e -> do
          void (Sync.takeTry @e)
          Sync.putTry e
          Time.sleep @t @d interval
          spin
