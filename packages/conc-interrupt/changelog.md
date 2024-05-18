# 0.12.0.0

## Breaking

* Move `Scoped` to Polysemy and `polysemy-resume`, keeping them here with a deprecation pragma.

# 0.11.0.0

## Breaking

* Tweak the implementation of `Scoped` interpreters to allow changing higher-order interpreters.
  Slightly changes the signature of some of them.

# 0.10.0.0

## Breaking

* Add a parameter to `Scoped` that allows arbitrary data to be passed to the scoped interpreter via `scoped`.
* Remove the `resource` parameter from `Scoped`.

## Other

* Add `Queue.tryReadMaybe`, a variant of `readMaybe` that does not wait.
* Add variants of the combined `Scoped` interpreters in which the `Resumable` is inside the scope.
* Add `Semaphore`, abstracting `QSem` or `TSem`.
* Add `timeoutStop`, a variant of `timeout` that calls `Polysemy.Resume.stop`.
* Add `Lock`, a mutex effect.
* Add `withAsyncGated`, a variant of `withAsync` that requires the async action to signal the sync action to start.
* Add `Gate`, a synchronization point effect.
* Add variants of `subscribeWhile` and `subscribeLoop` that use `Gate` for synchronization and run in a new thread.
* Add pure interpreters for `Mask`.
* Add more variants of `subscribeLoop` that return matching elements.
* Add `Gates` to `ConcStack`.
* Add `rescope`, a reinterpreter for `Scoped` that may extend the scope parameters.

# 0.9.0.0

* Add the missing `MVar` combinators for `Sync` that bracket an action with `mask`.
* Export `Restoration`.
* Add `interpretProcessOutputLeft/Right`.

# 0.6.1.0

* Add `SyncRead`, a read-only variant of `Sync`.
* Change `withAsync` to use `finally` instead of `bracket`, since the latter causes `AsyncCancelled` to be masked,
  preventing the action from being cancelled unless it runs an interruptible action.

# 0.6.0.0

* Add `Resumable` support for `Scoped`.
* Add `Scoped` interpreters that allow the allocator to interpret additional effects used by the handler.

# 0.5.0.0

* Add `mask` effects.
* Add `Monitor`, an effect that repeatedly checks a condition and restarts a region when it is met.
* Add interpreter combinators for `Scoped`.
* Add a runner for the default `Conc` stack.

# 0.4.0.0

* Add `lock`, a combinator for protecting a region with a mutex.
* Add `scheduleAsync`, a combinator for running an action async that allows the handle to be used before the thread
  starts
* Change the default signal handler for `Interrupt` to `CatchInfo`, catching repeated signals.

# 0.3.0.0

* Change `Race.timeout` to take a `Sem` for the fallback instead of a pure value.
* Export all `Queue` constructors from `Polysemy.Conc.Queue`.
* Export all `Sync` constructors from `Polysemy.Conc.Sync`.
* Move all interpreters to `Polysemy.Conc.Interpreter`.

# 0.2.0.0
* Add `read*` constructors for `Sync`
* Add `subscribeWhile`, a combinator that consumes events until a condition is met
* Add looping combinators for `Queue`
* Add `retry`, a combinator that runs an action repeatedly until it returns `Right` or a timeout is hit
* Add `Scoped`, an effect for local resource scoping
* Add `withAsync`, a bracketing combinator that runs an async action while the main action runs
* Add `interpretAtomic`, a convenience interpreter for `AtomicState` that runs `runAtomicStateTVar`
