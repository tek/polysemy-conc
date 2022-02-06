module Polysemy.Process.Effect.ProcessOutput where

data ProcessOutput a :: Effect where
  Chunk :: ByteString -> ByteString -> ProcessOutput a m ([a], ByteString)

makeSem ''ProcessOutput
