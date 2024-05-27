{-# options_haddock prune #-}
-- | Description: Queue Combinators
module Polysemy.Conc.Queue (
  module Polysemy.Conc.Queue,
  module Polysemy.Conc.Effect.Queue,
  module Polysemy.Conc.Data.QueueResult,
) where

import Polysemy.Conc.Data.QueueResult
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (
  Queue,
  close,
  closed,
  peek,
  read,
  readTimeout,
  tryPeek,
  tryRead,
  tryWrite,
  write,
  writeTimeout,
  )
import Polysemy.Conc.Queue.Result (resultToMaybe)

-- | Read from a 'Queue' repeatedly until it is closed.
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
        QueueResult.Closed -> unit

-- | Read from a 'Queue' repeatedly until it is closed.
--
-- When an element is received, call @action@ and recurse.
loop ::
  Member (Queue d) r =>
  (d -> Sem r ()) ->
  Sem r ()
loop action =
  loopOr (pure True) \ d -> True <$ action d

-- | Read from a 'Queue' and convert the result to 'Maybe', returning 'Nothing' if the queue has been closed, and
-- blocking until an element is available.
readMaybe ::
  Member (Queue d) r =>
  Sem r (Maybe d)
readMaybe =
  resultToMaybe <$> read

-- | Read from a 'Queue' and convert the result to 'Maybe', returning 'Nothing' if there is no element available or the
-- queue has been closed.
tryReadMaybe ::
  Member (Queue d) r =>
  Sem r (Maybe d)
tryReadMaybe =
  resultToMaybe <$> tryRead
