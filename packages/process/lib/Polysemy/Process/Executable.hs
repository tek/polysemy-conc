module Polysemy.Process.Executable where

import Exon (exon)
import Path (Abs, File, Path, Rel, toFilePath)
import qualified Path.IO as Path
import Path.IO (getPermissions, executable)

checkExecutable ::
  Member (Embed IO) r =>
  Text ->
  Path Abs File ->
  Sem r (Either Text (Path Abs File))
checkExecutable name path =
  tryAny (getPermissions path) <&> \case
    Right (executable -> True) ->
      Right path
    Right _ ->
      Left (message "executable")
    Left _ ->
      Left (message "a readable file")
  where
    message what =
      [exon|specified path for `#{name}` is not #{what}: #{pathText}|]
    pathText =
      toText (toFilePath path)

-- |Find a file in @$PATH@, verifying that it is executable by this process.
resolveExecutable ::
  Member (Embed IO) r =>
  -- |Executable name, for @$PATH@ lookup and error messages
  Path Rel File ->
  -- |Explicit override to be checked for adequate permissions
  Maybe (Path Abs File) ->
  Sem r (Either Text (Path Abs File))
resolveExecutable exe = \case
  Just path ->
    checkExecutable name path
  Nothing ->
    tryAny (Path.findExecutable exe) <&> \case
      Right (Just path) ->
        Right path
      _ ->
        Left [exon|could not find executable `#{name}` in `$PATH`.|]
  where
    name =
      toText (toFilePath exe)
