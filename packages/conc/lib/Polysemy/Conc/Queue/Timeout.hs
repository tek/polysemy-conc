-- |Description: Timeout Helper
module Polysemy.Conc.Queue.Timeout where

import Control.Concurrent.STM (STM, atomically)
import Polysemy.Time (TimeUnit)

import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import Polysemy.Conc.Data.QueueResult (QueueResult)
import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Race as Race

-- |Run an 'STM' action atomically with a time limit
withTimeout ::
  TimeUnit t =>
  Members [Race, Embed IO] r =>
  t ->
  STM (Maybe d) ->
  Sem r (QueueResult d)
withTimeout timeout readQ =
  Race.timeoutAs_ QueueResult.NotAvailable timeout reader'
  where
    reader' =
      maybe QueueResult.Closed QueueResult.Success <$> embed (atomically readQ)
