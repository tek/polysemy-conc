-- |Description: Sync Combinators
module Polysemy.Conc.Sync where

import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)

import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)

whileEmpty ::
  ∀ a r .
  Member (Sync a) r =>
  Sem r () ->
  Sem r ()
whileEmpty action =
  spin
  where
    spin = do
      action
      whenM (isJust <$> Sync.try @a) spin

whileEmptyInterval ::
  ∀ a u t d r .
  TimeUnit u =>
  Members [Time t d, Sync a] r =>
  u ->
  Sem r () ->
  Sem r ()
whileEmptyInterval interval action =
  spin
  where
    spin = do
      action
      whenM (isJust <$> Sync.try @a) (Time.sleep @t @d interval *> spin)
