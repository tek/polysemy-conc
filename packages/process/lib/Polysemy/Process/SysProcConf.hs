-- |Constructors for 'SysProcConf', Internal
module Polysemy.Process.SysProcConf where

import Path (Abs, File, Path, Rel, toFilePath)
import Path.IO (findExecutable)

import Polysemy.Process.Interpreter.SystemProcess (SysProcConf)
import System.Process.Typed (shell, proc)

-- |Create a 'SysProcConf' from an executable path and a list of arguments.
processConfig :: Path Abs File -> [Text] -> SysProcConf
processConfig exe args =
  proc (toFilePath exe) (toString <$> args)
{-# inline processConfig #-}

-- |Create a 'SysProcConf' from an shell command line.
shellConfig :: Text -> SysProcConf
shellConfig cmd =
  shell (toString cmd)
{-# inline shellConfig #-}

-- |Create a 'SysProcConf' by looking up an executable in the path, and using the supplied arguments.
which ::
  Member (Embed IO) r =>
  Path Rel File ->
  [Text] ->
  Sem r (Maybe SysProcConf)
which exeName args =
  fmap (flip processConfig args) . join <$> tryIOErrorMaybe (findExecutable exeName)
{-# inline which #-}
