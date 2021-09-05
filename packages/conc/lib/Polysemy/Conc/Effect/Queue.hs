{-# options_haddock prune #-}
-- |Description: Queue effect
module Polysemy.Conc.Effect.Queue where

import Polysemy.Time (TimeUnit)
import Polysemy.Conc.Data.QueueResult (QueueResult)

-- |Abstracts queues like 'Control.Concurrent.STM.TBQueue'.
--
-- For documentation on the constructors, see the module "Polysemy.Conc.Data.Queue".
--
-- @
-- import Polysemy.Conc (Queue, QueueResult)
-- import Polysemy.Conc.Effect.Queue as Queue
--
-- prog :: Member (Queue Int) r => Sem r (QueueResult Int)
-- prog = do
--   Queue.write 5
--   Queue.write 10
--   Queue.read >>= \\case
--     QueueResult.Success i -> fmap (i +) \<$> Queue.read
--     r -> pure r
-- @
data Queue d :: Effect where
  -- |Read an element from the queue, blocking until one is available.
  Read :: Queue d m (QueueResult d)
  -- |Read an element from the queue, immediately returning if none is available.
  TryRead :: Queue d m (QueueResult d)
  -- |Read an element from the queue, blocking until one is available or the timeout expires.
  ReadTimeout :: TimeUnit t => t -> Queue d m (QueueResult d)
  -- |Read an element, leaving it in the queue, blocking until one is available.
  Peek :: Queue d m (QueueResult d)
  -- |Read an element, leaving it in the queue, immediately returning if none is available.
  TryPeek :: Queue d m (QueueResult d)
  -- |Write an element to the queue, blocking until a slot is available.
  Write :: d -> Queue d m ()
  -- |Write an element to the queue, immediately returning if no slot is available.
  TryWrite :: d -> Queue d m (QueueResult ())
  -- |Write an element to the queue, blocking until a slot is available or the timeout expires.
  WriteTimeout :: TimeUnit t => t -> d -> Queue d m (QueueResult ())
  -- |Indicate whether the queue is closed.
  Closed :: Queue d m Bool
  -- |Close the queue.
  Close :: Queue d m ()

makeSem ''Queue
