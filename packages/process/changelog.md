# Unreleased

* Allow `Process` to emit custom chunks constructed by an interpreter of `ProcessOutput` instead of `ByteString`s
  containing whatever the `Handle` produced.
* Rename stdio interpreters.

# 0.5.0.0

* Add the effect `Process`, wrapping `System.Process.Typed` using `Scoped`.
