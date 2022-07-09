{-# options_haddock prune #-}

-- |Description: Scoped Interpreters, Internal
module Polysemy.Conc.Interpreter.Scoped where

import Polysemy.Internal.Index (InsertAtIndex)
import Polysemy.Resume (Stop, type (!!))

import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.PScoped (
  interpretPScoped,
  interpretPScopedAs,
  interpretPScopedH,
  interpretPScopedResumable,
  interpretPScopedResumableH,
  interpretPScopedResumableWith,
  interpretPScopedResumableWithH,
  interpretPScopedResumableWith_,
  interpretPScopedResumable_,
  interpretPScopedWith,
  interpretPScopedWithH,
  interpretPScopedWith_,
  interpretResumablePScoped,
  interpretResumablePScopedH,
  interpretResumablePScopedWith,
  interpretResumablePScopedWithH,
  interpretResumablePScopedWith_,
  interpretResumablePScoped_,
  runPScoped,
  runPScopedAs,
  )

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- /Note/: This function will be called for each action in the program, so if the interpreter allocates any resources,
-- they will be scoped to a single action. Move them to @withResource@ instead.
runScoped ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScoped withResource =
  runPScoped (const withResource)

-- |Variant of 'runScoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ resource effect r .
  Sem r resource ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScopedAs resource =
  runPScopedAs (const resource)

-- |Variant of 'runScoped' that takes a higher-order handler instead of an interpreter.
interpretScopedH ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedH withResource =
  interpretPScopedH (const withResource)

-- |Variant of 'runScoped' that takes a handler instead of an interpreter.
interpretScoped ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped resource effect) r
interpretScoped withResource =
  interpretPScoped (const withResource)

-- |Variant of 'interpretScoped' in which the resource allocator is a plain action.
interpretScopedAs ::
  ∀ resource effect r .
  Sem r resource ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedAs resource =
  interpretPScopedAs (const resource)

-- |Combined higher-order interpreter for 'Scoped' and 'Polysemy.Resumable.Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumableH withResource =
  interpretPScopedResumableH (const withResource)

-- |Combined interpreter for 'Scoped' and 'Polysemy.Resumable.Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumable withResource =
  interpretPScopedResumable (const withResource)

-- |Combined interpreter for 'Scoped' and 'Polysemy.Resumable.Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ resource effect err r .
  Sem (Stop err : r) resource ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumable_ resource =
  interpretPScopedResumable_ (const resource)

-- |Higher-order interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the
-- resource allocator.
interpretScopedWithH ::
  ∀ extra resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect] r1 r (Scoped resource effect : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedWithH withResource =
  interpretPScopedWithH @extra (const withResource)

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
interpretScopedWith ::
  ∀ extra resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect] r1 r (Scoped resource effect : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedWith withResource =
  interpretPScopedWith @extra (const withResource)

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedWith_ ::
  ∀ extra effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped () effect] r1 r (Scoped () effect : r1) extra =>
  (∀ x . Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped () effect) r
interpretScopedWith_ withResource =
  interpretPScopedWith_ @extra (const withResource)

-- |Combined higher-order interpreter for 'Scoped' and 'Polysemy.Resumable.Resumable' that allows the handler to use
-- additional effects that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect !! err] r1 r (Scoped resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumableWithH withResource =
  interpretPScopedResumableWithH @extra (const withResource)

-- |Combined interpreter for 'Scoped' and 'Polysemy.Resumable.Resumable' that allows the handler to use additional
-- effects that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWith ::
  ∀ extra resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect !! err] r1 r (Scoped resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumableWith withResource =
  interpretPScopedResumableWith @extra (const withResource)

-- |Combined interpreter for 'Scoped' and 'Polysemy.Resumable.Resumable' that allows the handler to use additional
-- effects that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[Scoped () effect !! err] r1 r (Scoped () effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped () effect !! err) r
interpretScopedResumableWith_ extra =
  interpretPScopedResumableWith_ @extra (const extra)

-- |Combined higher-order interpreter for 'Polysemy.Resumable.Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScopedH withResource =
  interpretResumablePScopedH (const withResource)

-- |Combined interpreter for 'Polysemy.Resumable.Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScoped withResource =
  interpretResumablePScoped (const withResource)

-- |Combined interpreter for 'Polysemy.Resumable.Resumable' and 'Scoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumableScoped_ ::
  ∀ resource effect err r .
  Sem r resource ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScoped_ resource =
  interpretResumablePScoped_ (const resource)

-- |Combined higher-order interpreter for 'Polysemy.Resumable.Resumable' and 'Scoped' that allows the handler to use
-- additional effects that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWithH ::
  ∀ extra resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped resource (effect !! err)] r1 r (Scoped resource (effect !! err) : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r1) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScopedWithH withResource =
  interpretResumablePScopedWithH @extra (const withResource)

-- |Combined interpreter for 'Polysemy.Resumable.Resumable' and 'Scoped' that allows the handler to use additional
-- effects that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWith ::
  ∀ extra resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped resource (effect !! err)] r1 r (Scoped resource (effect !! err) : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScopedWith withResource =
  interpretResumablePScopedWith @extra (const withResource)

-- |Combined interpreter for 'Polysemy.Resumable.Resumable' and 'Scoped' that allows the handler to use additional
-- effects that are interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumableScopedWith_ ::
  ∀ extra effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped () (effect !! err)] r1 r (Scoped () (effect !! err) : r1) extra =>
  (∀ x . Sem r1 x -> Sem r x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped () (effect !! err)) r
interpretResumableScopedWith_ extra =
  interpretResumablePScopedWith_ @extra (const extra)
