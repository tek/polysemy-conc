-- |Description: Race Combinators
module Polysemy.Conc.Race where

import Polysemy.Time (TimeUnit)

import qualified Polysemy.Conc.Effect.Race as Race
import Polysemy.Conc.Effect.Race (Race)

-- |Specialization of 'Race.race' for the case where both thunks return the same type, obviating the need for 'Either'.
race_ ::
  Member Race r =>
  Sem r a ->
  Sem r a ->
  Sem r a
race_ ml mr =
  unify <$> Race.race ml mr
{-# inline race_ #-}

-- |Specialization of 'Race.timeout' for the case where the thunk return the same type as the fallback, obviating the
-- need for 'Either'.
timeout_ ::
  TimeUnit u =>
  Member Race r =>
  Sem r a ->
  u ->
  Sem r a ->
  Sem r a
timeout_ err interval ma =
  unify <$> Race.timeout err interval ma
{-# inline timeout_ #-}

-- |Version of `Race.timeout` that takes a pure fallback value.
timeoutAs ::
  TimeUnit u =>
  Member Race r =>
  a ->
  u ->
  Sem r b ->
  Sem r (Either a b)
timeoutAs err =
  Race.timeout (pure err)
{-# inline timeoutAs #-}

-- |Specialization of 'timeoutAs' for the case where the thunk return the same type as the fallback, obviating the
-- need for 'Either'.
timeoutAs_ ::
  TimeUnit u =>
  Member Race r =>
  a ->
  u ->
  Sem r a ->
  Sem r a
timeoutAs_ err =
  timeout_ (pure err)
{-# inline timeoutAs_ #-}

-- |Specialization of 'Race.timeout' for unit actions.
timeoutU ::
  TimeUnit u =>
  Member Race r =>
  u ->
  Sem r () ->
  Sem r ()
timeoutU =
  timeout_ pass
{-# inline timeoutU #-}

-- |Variant of 'timeout' that returns 'Maybe'.
timeoutMaybe ::
  TimeUnit u =>
  Member Race r =>
  u ->
  Sem r a ->
  Sem r (Maybe a)
timeoutMaybe u ma =
  timeoutAs_ Nothing u (Just <$> ma)
{-# inline timeoutMaybe #-}

