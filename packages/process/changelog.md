# Unreleased

* Add `oneshot` variants of `Process` interpreters that send `Stop` to the individual actions inside the scope, modeling
  a process that is expected to terminate.
* Refactor interpreters to use `PScoped` as the basis and specialize unparameterized ones.
  Don't provide loads of specializations for I/O types, rather offer interpreters that handle `ProcessOutput` and
  `ProcessInput` only.
* Add constructors for `SysProcConf` that use `Path` and `Text`.

# 0.9.0.0

* Remove stderr from the `Process` abstraction so that stdout and stderr must be unified after parsing (potentially as
  `Either` to emulate the previous behaviour).
* Make default `ProcessOutput` interpreters discard all stderr output.
* Add re-interpreter from `Input`/`Output` to `Process`.
* Add `interpretProcessOutputIncremental`, a stateful interpreter using a supplied parser that may produce partial
  results.
* Add `interpretProcessCurrent` et al, treating the program's process's stdio as the (mirrored) stdio of an external
  process.
* Add `interpretProcessOutputLeft` and `interpretProcessOutputRight`, lifting results of another interpreter into
  `Either`.

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
