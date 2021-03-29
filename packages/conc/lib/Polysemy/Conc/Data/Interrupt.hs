{-# options_haddock prune #-}
-- |Description: Interrupt effect
module Polysemy.Conc.Data.Interrupt where

-- |The interrupt handler effect allows three kinds of interaction for interrupt signals:
--
-- - Execute a callback when a signal is received
-- - Block a thread until a signal is received
-- - Kill a thread when a signal is received
--
-- For documentation on the constructors, see the module "Polysemy.Conc.Data.Interrupt".
--
-- @
-- import qualified Polysemy.Conc.Data.Interrupt as Interrupt
--
-- prog = do
--   Interrupt.register "task 1" (putStrLn "interrupted")
--   Interrupt.killOnQuit $ forever do
--    doSomeWork
-- @
data Interrupt :: Effect where
  -- |Add a computation to be executed on interrupt, using the first argument as a key.
  Register :: Text -> IO () -> Interrupt m ()
  -- |Remove the previously added handler with the given key.
  Unregister :: Text -> Interrupt m ()
  -- |Manually trigger the interrupt.
  Quit :: Interrupt m ()
  -- |Block until an interrupt is triggered.
  WaitQuit :: Interrupt m ()
  -- |Indicate whether an interrupt was triggered.
  Interrupted :: Interrupt m Bool
  -- |Execute a computation, waiting for it to finish, killing its thread on interrupt.
  KillOnQuit :: Text -> m a -> Interrupt m (Maybe a)

makeSem ''Interrupt

-- |Variant of 'killOnQuit' that returns @()@.
killOnQuit_ ::
  Member Interrupt r =>
  Text ->
  Sem r a ->
  Sem r ()
killOnQuit_ desc ma =
  void (killOnQuit desc ma)
