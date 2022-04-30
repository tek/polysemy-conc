# 0.8.0.0
* Add `ProcessOptions`, replacing the primitive parameters of `Process` interpreters.
* Add an option for `Process` that determines whether to kill the process after exiting the scope.

# 0.6.0.0

* Allow `Process` to emit custom chunks constructed by an interpreter of `ProcessOutput` instead of `ByteString`s
  containing whatever the `Handle` produced.
* Rename stdio interpreters.
* Add helper `resolveExecutable` that looks up names in `$PATH` and ensures the files are executable.
* Add effect `Pty` for interacting with pseudo terminals.
* Add low-level process abstraction effect, `SystemProcess`.

# 0.5.0.0

* Add the effect `Process`, wrapping `System.Process.Typed` using `Scoped`.
