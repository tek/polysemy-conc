{-# options_haddock prune #-}

-- | Description: Executable helpers, Internal
module Polysemy.Process.Executable where

import Path (Abs, File, Path, Rel, toFilePath)
import qualified Path.IO as Path
import Path.IO (executable, getPermissions)

checkExecutable ::
  Member (Embed IO) r =>
  Text ->
  Path Abs File ->
  Sem r (Either Text (Path Abs File))
checkExecutable name path =
  tryIOError (getPermissions path) <&> \case
    Right (executable -> True) ->
      Right path
    Right _ ->
      Left (message "executable")
    Left _ ->
      Left (message "a readable file")
  where
    message what =
      "specified path for `" <> name <> "` is not " <> what <> ": " <> pathText
    pathText =
      toText (toFilePath path)

-- | Find a file in @$PATH@, verifying that it is executable by this process.
resolveExecutable ::
  Member (Embed IO) r =>
  -- | Executable name, for @$PATH@ lookup and error messages
  Path Rel File ->
  -- | Explicit override to be checked for adequate permissions
  Maybe (Path Abs File) ->
  Sem r (Either Text (Path Abs File))
resolveExecutable exe = \case
  Just path ->
    checkExecutable name path
  Nothing ->
    tryIOError (Path.findExecutable exe) <&> \case
      Right (Just path) ->
        Right path
      _ ->
        Left ("could not find executable `" <> name <> "` in `$PATH`.")
  where
    name =
      toText (toFilePath exe)
