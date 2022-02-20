{-# options_haddock hide #-}
{-# language NoImplicitPrelude #-}

module Polysemy.Conc.Prelude (
  module Data.Kind,
  module Polysemy.Conc.Prelude,
  module GHC.Err,
  module Polysemy,
  module Polysemy.AtomicState,
  module Relude,
) where

import Control.Exception (try)
import Data.Kind (Type)
import GHC.Err (undefined)
import Polysemy (
  Effect,
  EffectRow,
  Embed,
  Final,
  InterpreterFor,
  Member,
  Members,
  Sem,
  WithTactics,
  embed,
  embedToFinal,
  interpret,
  interpretH,
  makeSem,
  pureT,
  raise,
  raiseUnder,
  raiseUnder2,
  raiseUnder3,
  reinterpret,
  runFinal,
  )
import Polysemy.AtomicState (AtomicState, atomicGet, atomicGets, atomicModify', atomicPut)
import Polysemy.Internal.Kind (Append)
import Relude hiding (
  Reader,
  State,
  Sum,
  Type,
  ask,
  asks,
  evalState,
  filterM,
  get,
  gets,
  hoistEither,
  modify,
  modify',
  put,
  readFile,
  runReader,
  runState,
  state,
  trace,
  traceShow,
  undefined,
  )

unify :: Either a a -> a
unify =
  either id id
{-# inline unify #-}

tryAny ::
  Member (Embed IO) r =>
  IO a ->
  Sem r (Either Text a)
tryAny =
  embed @IO . fmap (first show) . try @SomeException
{-# inline tryAny #-}

tryMaybe ::
  Member (Embed IO) r =>
  IO a ->
  Sem r (Maybe a)
tryMaybe =
  embed @IO . fmap rightToMaybe . try @SomeException
{-# inline tryMaybe #-}

ignoreException ::
  Member (Embed IO) r =>
  IO () ->
  Sem r ()
ignoreException =
  void . embed @IO . try @SomeException
{-# inline ignoreException #-}

type a ++ b =
  Append a b

leftM ::
  Applicative m =>
  m b ->
  Either a b ->
  m b
leftM f =
  either (const f) pure
{-# inline leftM #-}
