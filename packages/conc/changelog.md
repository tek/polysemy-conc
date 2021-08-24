# Unreleased

# 0.2.0.0
* Add `read*` constructors for `Sync`
* Add `subscribeWhile`, a combinator that consumes events until a condition is met
* Add looping combinators for `Queue`
* Add `retry`, a combinator that runs an action repeatedly until it returns `Right` or a timeout is hit
* Add `Scoped`, an effect for local resource scoping
* Add `withAsync`, a bracketing combinator that runs an async action while the main action runs
* Add `interpretAtomic`, a convenience interpreter for `AtomicState` that runs `runAtomicStateTVar`
