{-# options_haddock prune #-}
-- |Description: Race interpreters
module Polysemy.Conc.Race where

import qualified Control.Concurrent.Async as Async
import Polysemy.Final (getInitialStateS, interpretFinal, runS)
import qualified Polysemy.Time as Time
import Polysemy.Time (MicroSeconds(MicroSeconds), TimeUnit)
import qualified System.Timeout as System

import qualified Polysemy.Conc.Data.Race as Race
import Polysemy.Conc.Data.Race (Race)

biseqEither ::
  Functor f =>
  Either (f a) (f b) ->
  f (Either a b)
biseqEither =
  either (fmap Left) (fmap Right)
{-# INLINE biseqEither #-}

-- |Interpret 'Race' in terms of 'Async.race' and 'System.timeout'.
-- Since this has to pass higher-order thunks as 'IO' arguments, it is interpreted in terms of 'Final IO'.
interpretRace ::
  Member (Final IO) r =>
  InterpreterFor Race r
interpretRace =
  interpretFinal @IO \case
    Race.Race left right ->
      fmap (fmap biseqEither) . Async.race <$> runS left <*> runS right
    Race.Timeout err (Time.convert -> MicroSeconds timeout) mb -> do
      mbT <- runS mb
      s <- getInitialStateS
      pure (maybe (Left err <$ s) (fmap Right) <$> System.timeout (fromIntegral timeout) mbT)
{-# INLINE interpretRace #-}

-- |Specialization of 'Race.race' for the case where both thunks return the same type, obviating the need for 'Either'.
race_ ::
  Member Race r =>
  Sem r a ->
  Sem r a ->
  Sem r a
race_ ma mb =
  unify <$> Race.race ma mb
{-# INLINE race_ #-}

-- |Specialization of 'Race.timeout' for the case where the thunk return the same type as the fallback, obviating the
-- need for 'Either'.
timeout_ ::
  TimeUnit u =>
  Member Race r =>
  a ->
  u ->
  Sem r a ->
  Sem r a
timeout_ err interval mb =
  unify <$> Race.timeout err interval mb
{-# INLINE timeout_ #-}
