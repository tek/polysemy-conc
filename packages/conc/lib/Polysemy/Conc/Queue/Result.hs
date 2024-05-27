{-# options_haddock hide #-}
module Polysemy.Conc.Queue.Result where

import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import Polysemy.Conc.Data.QueueResult (QueueResult)

closedResult ::
  Maybe d ->
  QueueResult d
closedResult = \case
  Nothing -> QueueResult.Closed
  Just d -> QueueResult.Success d
{-# inline closedResult #-}

naResult ::
  Maybe d ->
  QueueResult d
naResult = \case
  Nothing -> QueueResult.NotAvailable
  Just d -> QueueResult.Success d
{-# inline naResult #-}

closedNaResult ::
  Maybe (Maybe d) ->
  QueueResult d
closedNaResult = \case
  Nothing -> QueueResult.Closed
  Just Nothing -> QueueResult.NotAvailable
  Just (Just d) -> QueueResult.Success d
{-# inline closedNaResult #-}

closedBoolResult ::
  Maybe Bool ->
  QueueResult ()
closedBoolResult = \case
  Nothing -> QueueResult.Closed
  Just False -> QueueResult.NotAvailable
  Just True -> QueueResult.Success ()
{-# inline closedBoolResult #-}

-- | Turn a 'QueueResult.Success' into 'Just'.
resultToMaybe :: QueueResult d -> Maybe d
resultToMaybe = \case
  QueueResult.Success d -> Just d
  QueueResult.NotAvailable -> Nothing
  QueueResult.Closed -> Nothing
{-# inline resultToMaybe #-}
