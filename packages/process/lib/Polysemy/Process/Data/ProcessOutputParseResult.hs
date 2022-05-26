-- |A data type encoding the result of an incremental parser for process output.
module Polysemy.Process.Data.ProcessOutputParseResult where

import Text.Show (showParen, showString, showsPrec)

-- |An incremental parse result, potentially a partial result containing a continuation function.
data ProcessOutputParseResult a =
  Done { value :: a, leftover :: ByteString }
  |
  Partial { continue :: ByteString -> ProcessOutputParseResult a }
  |
  Fail { error :: Text }

instance Show a => Show (ProcessOutputParseResult a) where
  showsPrec d = \case
    Done {..} ->
      showParen (d > 10) (showString "Done { value = " <> showsPrec 11 value <> showString ", leftover = " <> showsPrec 11 leftover <> showString " }")
    Partial _ ->
      showString "Partial"
    Fail e ->
      showParen (d > 10) (showString "Fail { error = " <> showString (show e) <> showString " }")
