# Story BUG-003: TEA-Rust Goto Condition Evaluates Before Lua State Update

| Field | Value |
|-------|-------|
| **Story ID** | BUG-003 |
| **Epic** | EPIC-CONFORMANCE-001 |
| **Status** | Draft |
| **Priority** | High |
| **Type** | Bug (External Dependency) |
| **Discovered** | 2026-01-18 |
| **Reporter** | Manual Testing |
| **Upstream** | TEA (The Edge Agent) - tea-rust implementation |

---

## Bug Summary

In tea-rust, goto conditions that reference state variables set by the current Lua node are evaluated BEFORE the Lua node updates the state. This causes incorrect branching where the goto condition uses stale state values.

## Reproduction

### Agent YAML (Minimal Example)

```yaml
nodes:
  - name: set_flag
    language: lua
    run: |
      return { should_continue = true }
    goto:
      - if: "state.should_continue"
        to: process
      - to: skip

  - name: process
    language: lua
    run: |
      return { result = "processed" }

  - name: skip
    language: lua
    run: |
      return { result = "skipped" }
```

### Expected Behavior (tea-python)

1. `set_flag` executes Lua, sets `state.should_continue = true`
2. Goto evaluates `state.should_continue` → `true`
3. Flow goes to `process`

### Actual Behavior (tea-rust)

1. Goto evaluates `state.should_continue` → `nil/false` (pre-update state)
2. Flow goes to `skip`
3. `set_flag` Lua updates state (but goto already decided)

## Evidence

### tea-python (Correct)
```
✓ analyze_gaps
✓ generate_transform_prompt    # <-- Correct path
✓ transform_with_llm
✓ extract_transformed
```

### tea-rust (Incorrect)
```
✓ analyze_gaps
✓ passthrough                  # <-- Wrong path, skipped transform
```

## Impact

- Document transformation agents fail to call LLM when using tea-rust
- Conformance pipeline falls back to rule-based transformation (less effective)
- AgentFS `graph-docs conform` command produces suboptimal results with tea-rust

## Workaround

Use `TEA_BINARY=tea-python` when running AgentFS conformance commands:

```bash
export TEA_BINARY=/path/to/tea-python
agentfs graph-docs <db> conform --agents-dir agents /path/to/docs
```

## Recommended Fix (Upstream)

In tea-rust, the goto condition evaluation should happen AFTER the node's Lua/action execution completes and state is updated:

```rust
// Pseudo-code for correct order
async fn execute_node(node: &Node, state: &mut State) {
    // 1. Execute node logic (Lua, llm.chat, etc.)
    let new_state = node.execute(state).await?;

    // 2. Merge new state
    state.merge(new_state);

    // 3. NOW evaluate goto conditions (using updated state)
    let next_node = evaluate_goto(node.goto, state);

    // 4. Continue to next node
    ...
}
```

## Related Files

| File | Description |
|------|-------------|
| `agents/document-transformer-agent.yaml` | Affected agent |
| `agents/document-transformer-claude.yaml` | Affected agent |
| `sdk/rust/src/graphdocs/agent_transformer.rs` | AgentFS transformer using TEA |

## Change Log

| Date | Description | Author |
|------|-------------|--------|
| 2026-01-18 | Bug discovered during manual testing | QA |
| 2026-01-18 | Story created | Quinn (Test Architect) |
