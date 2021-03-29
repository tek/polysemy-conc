-- |Description: Queue result ADT
module Polysemy.Conc.Data.QueueResult where

-- |Encodes failure reasons for queues.
--
-- For documentation on the constructors, see the module "Polysemy.Conc.Data.QueueResult".
--
-- @
-- import qualified Polysemy.Conc.Data.QueueResult as QueueResult
-- @
data QueueResult d =
  -- |The operation was successful.
  Success d
  |
  -- |The queue is either full and cannot be added to, or empty and cannot be read from.
  NotAvailable
  |
  -- |The queue was closed by the user.
  Closed
  deriving (Eq, Show, Ord, Functor, Generic)

instance Semigroup d => Semigroup (QueueResult d) where
  Success d1 <> Success d2 = Success (d1 <> d2)
  Closed <> _ = Closed
  _ <> Closed = Closed
  NotAvailable <> _ = NotAvailable
  _ <> NotAvailable = NotAvailable

instance Monoid d => Monoid (QueueResult d) where
  mempty = Success mempty
