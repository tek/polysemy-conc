-- |Description: Critical interpreters
module Polysemy.Conc.Interpreter.Critical where

import qualified Control.Exception as Exception
import Polysemy (runT)
import Polysemy.Final (getInitialStateS, interpretFinal, runS)

import Polysemy.Conc.Effect.Critical (Critical (..))

-- |Interpret 'Critical' in terms of 'Final' 'IO'.
interpretCritical ::
  Member (Final IO) r =>
  InterpreterFor Critical r
interpretCritical =
  interpretFinal @IO \case
    Catch ma -> do
      s <- getInitialStateS
      o <- runS ma
      pure (run o s)
      where
        run ma' s =
          Exception.catch (fmap Right <$> ma') \ se -> pure (Left se <$ s)
{-# inline interpretCritical #-}

-- |Interpret 'Critical' by doing nothing.
interpretCriticalNull ::
  InterpreterFor Critical r
interpretCriticalNull =
  interpretH \case
    Catch ma ->
      fmap (fmap Right) . raise . interpretCriticalNull =<< runT ma
{-# inline interpretCriticalNull #-}
