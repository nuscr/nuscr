# Guard Extraction for Predicate-Based Branching

## Problem

When using multiparty session types to monitor wire protocols (e.g. MAVLink), branching in `choice` is determined by message field values, not by distinct message types. The same wire message (e.g. `COMMAND_ACK`) appears in multiple branches, distinguished only by payload contents (e.g. `result == 0` vs `result != 0`).

Standard session types require unique labels per choice branch. This forces artificial label proliferation (`AckSuccess`, `AckFailure`) and pushes the mapping between wire messages and labels into manual glue code outside the specification.

A secondary consequence: session routing (demultiplexing incoming messages to the correct monitor instance) depends on knowing which wire message types a session expects. When labels are abstract identifiers unrelated to wire message names, routing logic must also be hand-written.

## Proposed Solution

Reuse the existing refinement type syntax to express **guard predicates** — local constraints on payload fields that determine branch selection — without any parser or syntax changes.

### Guard vs Refinement

A refinement type `x: int{x > prev_count}` constrains a payload field using variables from previous messages or recursion variables. A guard `result: int{result == 0}` constrains a field using only variables bound in the current message's payload.

Both use the same syntactic form `var: type{expr}`. The distinction is semantic: a guard's free variables are a subset of the current message's payload-bound variables.

### Extraction Algorithm

Given a refinement expression `e` on a payload field:

1. Flatten top-level `And` nodes into a list of conjuncts
2. Compute `free_var(conjunct)` for each conjunct
3. Partition: conjuncts where all free variables are payload-bound become the **guard**; the rest remain as the **refinement**
4. If the guard portion is non-empty, conjoin its parts into `message.guard`
5. If the refinement portion is empty, degrade `PTRefined(v, t, _)` to `PValue(Some v, t)`

This naturally handles mixed expressions: `result == 0 && result > prev_count` splits into guard `result == 0` and refinement `result > prev_count`. Expressions under `Or` that mix local and non-local variables are never classified as guards because the free variables of the `Or` subtree include non-local variables.

### Relaxed Choice Determinism

With guard extraction enabled, the label-uniqueness check on choice branches is relaxed: branches may share a label if they carry guards. Optionally, guard disjointness can be verified statically via SMT (infrastructure for this already exists in nuscr's refinement type checking).

Non-exhaustive guards are permitted — a message matching no guard constitutes a protocol violation, which is the desired semantics for monitoring.

### Implications for Monitoring and Routing

- **Branch selection**: The generated monitor evaluates guards on the received payload to select the matching transition, rather than dispatching on label alone.
- **Session routing**: Labels correspond to wire message types. At any protocol state, the set of expected labels (with their guards) tells a router which active session should receive an incoming message. When multiple sessions expect the same label, guards disambiguate; when guards are insufficient, the message is broadcast to all matching sessions.

### Example

```
(*# GuardExtraction, RefinementTypes #*)

global protocol Command(role GCS, role AP) {
  COMMAND_LONG(command: int) from GCS to AP;
  choice at AP {
    COMMAND_ACK(result: int{result == 0}) from AP to GCS;
  } or {
    COMMAND_ACK(result: int{result != 0}) from AP to GCS;
  }
}
```

Both branches use the label `COMMAND_ACK` (the wire message type). The predicate on `result` is extracted as a guard because its only free variable (`result`) is bound in the current message's payload.
