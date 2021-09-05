{-# options_haddock prune #-}
-- |Description: Race Interpreters
module Polysemy.Conc.Interpreter.Race where

import qualified Control.Concurrent.Async as Async
import Polysemy.Final (interpretFinal, runS)
import qualified Polysemy.Time as Time
import Polysemy.Time (MicroSeconds (MicroSeconds))
import qualified System.Timeout as System

import qualified Polysemy.Conc.Effect.Race as Race
import Polysemy.Conc.Effect.Race (Race)

biseqEither ::
  Functor f =>
  Either (f a) (f b) ->
  f (Either a b)
biseqEither =
  either (fmap Left) (fmap Right)
{-# inline biseqEither #-}

-- |Interpret 'Race' in terms of 'Async.race' and 'System.timeout'.
-- Since this has to pass higher-order thunks as 'IO' arguments, it is interpreted in terms of 'Final IO'.
interpretRace ::
  Member (Final IO) r =>
  InterpreterFor Race r
interpretRace =
  interpretFinal @IO \case
    Race.Race left right ->
      fmap (fmap biseqEither) . Async.race <$> runS left <*> runS right
    Race.Timeout ma (Time.convert -> MicroSeconds timeout) mb -> do
      maT <- runS ma
      mbT <- runS mb
      pure (maybe (fmap Left <$> maT) (pure . fmap Right) =<< System.timeout (fromIntegral timeout) mbT)
{-# inline interpretRace #-}
