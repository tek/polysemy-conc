module Polysemy.Process.Data.ProcessOutputParseResult where

data ProcessOutputParseResult a =
  Done { value :: a, leftover :: ByteString }
  |
  Partial { continue :: ByteString -> ProcessOutputParseResult a }
  |
  Fail { error :: Text }
