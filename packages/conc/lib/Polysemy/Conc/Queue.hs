-- |Description: Queue Combinators
module Polysemy.Conc.Queue where

import qualified Polysemy.Conc.Data.Queue as Queue
import Polysemy.Conc.Data.Queue (Queue)
import qualified Polysemy.Conc.Data.QueueResult as QueueResult

-- |Read from a 'Queue' repeatedly until it is closed.
--
-- When an element is received, call @action@ and recurse if it returns 'True'.
-- When no element is available, evaluate @na@ and recurse if it returns 'True'.
loopOr ::
  Member (Queue d) r =>
  Sem r Bool ->
  (d -> Sem r Bool) ->
  Sem r ()
loopOr na action =
  spin
  where
    spin =
      Queue.read >>= \case
        QueueResult.Success d -> whenM (action d) spin
        QueueResult.NotAvailable -> whenM na spin
        QueueResult.Closed -> pass

-- |Read from a 'Queue' repeatedly until it is closed.
--
-- When an element is received, call @action@ and recurse.
loop ::
  Member (Queue d) r =>
  (d -> Sem r ()) ->
  Sem r ()
loop action =
  loopOr (pure True) \ d -> True <$ action d
