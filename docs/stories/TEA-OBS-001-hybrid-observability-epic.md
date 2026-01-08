# TEA-OBS-001: Hybrid Observability Architecture

## Status: Done ✅

## Epic Goal

Implement a standardized, embedded/offline-first observability system for both Python and Rust runtimes where every builtin node emits structured log events and developers can access aggregated flow-level logs for debugging and analysis.

## Epic Description

### Existing System Context

- **Current relevant functionality:**
  - Python: `tracing.py` provides `TraceContext` with span hierarchy, thread-safe operation, and multiple exporters (Console, File, Callback)
  - Rust: `tracing` and `tracing-subscriber` crates in Cargo.toml (currently unused)
- **Technology stack:** Python (primary), Rust (parity target), Jinja2 templates
- **Integration points:** `YAMLEngine._trace_context`, action registry pattern in `actions/*.py`, Rust `engine/` modules

### Enhancement Details

**What's being added/changed:**

1. `ObservabilityContext` - per-flow context that travels with state (Python + Rust)
2. `EventStream` - append-only log buffer with flow-level aggregation (Python + Rust)
3. Automatic instrumentation for all builtin nodes (llm, http, file, memory, etc.)
4. Log access API: `get_flow_log(flow_id) → structured trace`
5. Shared JSON log schema for cross-runtime compatibility

**How it integrates:**

- Python: Wraps existing `TraceContext` infrastructure
- Rust: Leverages `tracing` crate with custom `Layer` implementations
- Both: Injects into action registry via decorator/middleware pattern
- Flow context stored in state under `_observability` key

**Success criteria:**

- Every builtin action emits entry/exit events with timing
- Developers can retrieve full flow log by flow_id
- Works fully offline (no cloud dependencies)
- Configurable log levels (debug, info, warn, error)
- Python and Rust emit identical log structures (parity)

## Architecture Diagram (Branch D - Hybrid)

```
┌─────────────────────────────────────────────────────────────────┐
│                    Shared Log Schema (JSON)                      │
│  {flow_id, span_id, node, level, timestamp, event, data, ...}   │
└─────────────────────────────────────────────────────────────────┘
                              │
         ┌────────────────────┴────────────────────┐
         ▼                                         ▼
┌─────────────────────────────────┐  ┌─────────────────────────────────┐
│         Python Runtime          │  │          Rust Runtime           │
│                                 │  │                                 │
│  ObservabilityContext           │  │  ObservabilityContext           │
│  ├── flow_id: "uuid"            │  │  ├── flow_id: Uuid              │
│  ├── config: {level, handlers}  │  │  ├── config: ObsConfig          │
│  └── log(node, level, event)    │  │  └── log(node, level, event)    │
│           │                     │  │           │                     │
│           ▼                     │  │           ▼                     │
│  EventStream                    │  │  EventStream                    │
│  ├── ring_buffer: Vec<Event>    │  │  ├── ring_buffer: VecDeque      │
│  ├── handlers: [Console, File]  │  │  ├── layers: [fmt, json, file]  │
│  └── get_flow_log() → trace     │  │  └── get_flow_log() → trace     │
│                                 │  │                                 │
│  Built on: tracing.py           │  │  Built on: tracing crate        │
└─────────────────────────────────┘  └─────────────────────────────────┘
```

## Cross-Runtime Mapping

| Concept | Python Implementation | Rust Implementation |
|---------|----------------------|---------------------|
| ObservabilityContext | Class wrapping TraceContext | Struct with tracing::Span |
| start_span() | TraceContext.start_span() | tracing::span! / #[instrument] |
| log_event() | TraceContext.log_event() | tracing::event! / info!/debug! |
| ConsoleExporter | ConsoleExporter class | tracing_subscriber::fmt layer |
| FileExporter | FileExporter class | tracing_subscriber + file writer |
| CallbackExporter | CallbackExporter class | Custom Layer trait impl |
| Thread-local spans | threading.local() | tracing::Span::current() |
| Flow aggregation | completed_spans list | Custom Subscriber collecting spans |

## Shared Log Schema

Both runtimes emit events conforming to this JSON structure:

```json
{
  "flow_id": "550e8400-e29b-41d4-a716-446655440000",
  "span_id": "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
  "parent_id": "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
  "node": "llm.call",
  "level": "info",
  "timestamp": 1703347200.123,
  "event_type": "entry|exit|error|metric",
  "message": "Optional human-readable message",
  "data": {
    "input": {},
    "output": {},
    "error": null
  },
  "metrics": {
    "duration_ms": 123.45,
    "tokens": 150,
    "cost_usd": 0.003
  }
}
```

## Stories

### Story 1: Core ObservabilityContext Infrastructure (Python)

**Goal:** Create the foundational observability infrastructure in Python

**Acceptance Criteria:**
1. `ObservabilityContext` class with flow-scoped logging
2. `EventStream` with configurable ring buffer (default 1000 events)
3. Handlers: Console, File, Callback (extending existing exporters)
4. Integration with existing `TraceContext` (composition, not replacement)
5. Configuration via YAML: `observability.level`, `observability.handlers`
6. `get_flow_log(flow_id)` returns structured trace

**Files to create/modify:**
- `python/src/the_edge_agent/observability.py` (new)
- `python/src/the_edge_agent/yaml_engine.py` (integrate)
- `python/tests/test_observability.py` (new)

---

### Story 2: Core ObservabilityContext Infrastructure (Rust)

**Goal:** Create the foundational observability infrastructure in Rust using the `tracing` crate

**Acceptance Criteria:**
1. `ObservabilityContext` struct with flow-scoped logging
2. `EventStream` with bounded VecDeque (default 1000 events)
3. Layers: Console (fmt), File (json), Callback (custom Layer)
4. Custom `Subscriber` that collects spans per flow_id
5. Configuration via YAML: `observability.level`, `observability.handlers`
6. `get_flow_log(flow_id)` returns structured trace

**Files to create/modify:**
- `rust/src/engine/observability.rs` (new)
- `rust/src/engine/mod.rs` (export)
- `rust/src/engine/yaml.rs` (integrate)
- `rust/tests/test_observability.rs` (new)

---

### Story 3: Automatic Builtin Node Instrumentation

**Goal:** Instrument all builtin actions to emit standardized log events

**Acceptance Criteria:**

**Python:**
1. Action wrapper decorator `@observable` for automatic entry/exit logging
2. All builtin actions instrumented: llm, http, file, memory, json, web, etc.
3. Captures: input parameters (sanitized), output results, timing, errors
4. Log level filtering per action category (e.g., `llm=debug`, `http=info`)
5. Zero overhead when observability disabled

**Rust:**
1. `#[instrument]` macro or manual spans for all builtin actions
2. All builtin actions instrumented: llm, http, file, memory, json
3. Captures: input parameters (sanitized), output results, timing, errors
4. Log level filtering via `tracing` levels
5. Compile-time elimination when disabled

**Files to modify:**
- Python: All `python/src/the_edge_agent/actions/*.py`
- Rust: All `rust/src/engine/*.rs` action implementations

---

### Story 4: Flow Log Access API

**Goal:** Provide programmatic access to flow logs for debugging and analysis

**Acceptance Criteria:**

1. `obs.get_flow_log` action returns complete flow trace
2. `obs.query` action for filtering logs (by node, level, time range)
3. `obs.export` action for JSON/JSONL file export
4. Flow log includes:
   - Ordered list of all events
   - Span hierarchy (parent-child relationships)
   - Aggregate metrics (total duration, token count, error count)
5. Works identically in Python and Rust

**New actions:**
```yaml
# Get full flow log
- action: obs.get_flow_log
  flow_id: "{{ state.flow_id }}"
  store: flow_log

# Query specific events
- action: obs.query
  flow_id: "{{ state.flow_id }}"
  filters:
    node: "llm.*"
    level: "error"
  store: errors

# Export to file
- action: obs.export
  flow_id: "{{ state.flow_id }}"
  format: jsonl
  path: "./logs/flow-{{ state.flow_id }}.jsonl"
```

---

### Story 5: Cross-Runtime Parity Tests

**Goal:** Ensure Python and Rust emit identical log structures

**Acceptance Criteria:**
1. Shared test fixtures in `examples/observability/parity/*.yaml`
2. Python test runs fixture, captures logs, writes to `parity-expected/*.json`
3. Rust test runs same fixture, compares output to expected
4. Schema validation for all log events
5. CI integration: parity tests run on every PR

**Test scenarios:**
- Simple linear flow (3 nodes)
- Conditional branching
- Parallel fan-out/fan-in
- Error handling and recovery
- Nested spans (node calling sub-nodes)

**Files to create:**
- `examples/observability/parity/*.yaml`
- `examples/observability/parity-expected/*.json`
- `python/tests/test_observability_parity.py`
- `rust/tests/test_observability_parity.rs`
- `scripts/observability-parity-test.sh`

## Action Instrumentation Patterns (Story 3 Reference)

### Python: `@observable` Decorator Pattern

The decorator wraps action functions to automatically emit entry/exit events:

```python
import functools
import time
from typing import Callable, Dict, Any, Optional


def observable(
    action_name: Optional[str] = None,
    level: str = "info",
    sanitize_keys: Optional[list] = None
) -> Callable:
    """
    Decorator for automatic observability instrumentation of actions.

    Usage:
        @observable()
        def llm_call(params: dict, state: dict, ctx: ActionContext) -> dict:
            ...

        @observable(action_name="http.request", sanitize_keys=["api_key", "token"])
        def http_get(params: dict, state: dict, ctx: ActionContext) -> dict:
            ...
    """
    sanitize_keys = sanitize_keys or ["api_key", "password", "token", "secret"]

    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(params: dict, state: dict, ctx: 'ActionContext') -> dict:
            # Get observability context from ActionContext or state
            obs_ctx = getattr(ctx, 'observability', None) or state.get('_observability_context')

            if obs_ctx is None or not obs_ctx.config.get('enabled', False):
                # No overhead when disabled
                return func(params, state, ctx)

            node = action_name or func.__name__

            # Sanitize sensitive parameters
            safe_params = _sanitize_params(params, sanitize_keys)

            # Entry event
            obs_ctx.log(node, level, 'entry', f"Starting {node}", data=safe_params)
            start_time = time.time()

            try:
                result = func(params, state, ctx)
                duration_ms = (time.time() - start_time) * 1000

                # Exit event with timing
                obs_ctx.log(node, level, 'exit', f"Completed {node}",
                           metrics={'duration_ms': duration_ms})
                return result

            except Exception as e:
                duration_ms = (time.time() - start_time) * 1000

                # Error event
                obs_ctx.log(node, 'error', 'error', f"Error in {node}: {str(e)}",
                           metrics={'duration_ms': duration_ms})
                raise

        return wrapper
    return decorator


def _sanitize_params(params: dict, keys: list) -> dict:
    """Redact sensitive keys from parameters."""
    result = {}
    for k, v in params.items():
        if any(sensitive in k.lower() for sensitive in keys):
            result[k] = "[REDACTED]"
        elif isinstance(v, dict):
            result[k] = _sanitize_params(v, keys)
        else:
            result[k] = v
    return result
```

### Rust: `#[instrument]` Macro Pattern

Rust uses the `tracing` crate's `#[instrument]` macro:

```rust
use tracing::{instrument, info, error};

/// Instrumented action that automatically creates spans
#[instrument(
    name = "llm.call",
    skip(state, obs_ctx),
    fields(flow_id = %obs_ctx.flow_id)
)]
pub fn llm_call(
    params: &JsonValue,
    state: &mut JsonValue,
    obs_ctx: Option<&ObservabilityContext>
) -> Result<JsonValue, ActionError> {
    // Entry logged automatically by #[instrument]

    let start = std::time::Instant::now();

    // Explicit entry event for consistency with Python
    if let Some(ctx) = obs_ctx {
        ctx.log_entry("llm.call", sanitize_params(params));
    }

    let result = execute_llm_call(params, state);

    let duration_ms = start.elapsed().as_secs_f64() * 1000.0;

    match &result {
        Ok(output) => {
            if let Some(ctx) = obs_ctx {
                ctx.log_exit("llm.call", output.clone(), duration_ms);
            }
            info!(duration_ms = duration_ms, "LLM call completed");
        }
        Err(e) => {
            if let Some(ctx) = obs_ctx {
                ctx.log_error("llm.call", &e.to_string());
            }
            error!(error = %e, "LLM call failed");
        }
    }

    result
}

fn sanitize_params(params: &JsonValue) -> JsonValue {
    // Redact sensitive keys
    let sensitive = ["api_key", "password", "token", "secret"];
    // ... implementation
    params.clone()  // Simplified
}
```

### Action Registry Integration

**Python** - Decorate at registration:
```python
# actions/__init__.py
from .observability import observable

@observable(action_name="llm.call", sanitize_keys=["api_key"])
def llm_call(params, state, ctx):
    ...

registry = {
    "llm.call": llm_call,
    # All actions auto-instrumented via decorator
}
```

**Rust** - Conditional instrumentation:
```rust
// engine/actions.rs
pub fn execute_action(
    name: &str,
    params: &JsonValue,
    state: &mut JsonValue,
    obs_ctx: Option<&ObservabilityContext>
) -> Result<JsonValue> {
    match name {
        "llm.call" => llm_call(params, state, obs_ctx),
        "http.get" => http_get(params, state, obs_ctx),
        // ...
    }
}
```

## Compatibility Requirements

- [x] Existing TraceContext API remains unchanged (Python)
- [x] Existing `tracing` crate usage unaffected (Rust)
- [x] No breaking changes to action registry
- [x] Existing YAML agents work without modification
- [x] Performance impact minimal (< 5ms overhead per node)
- [x] Observability is opt-in (disabled by default)

## Risk Mitigation

| Risk | Mitigation | Rollback |
|------|------------|----------|
| Performance overhead on hot paths | Lazy evaluation, ring buffer, async export | Config flag disables entirely |
| Log volume explosion | Bounded ring buffer, level filtering | Reduce level to `error` only |
| Cross-runtime drift | Parity tests in CI, shared schema | Schema versioning |
| Memory pressure from large flows | Configurable buffer size, file spill | Reduce buffer, increase spill |

## Definition of Done

- [x] All 2 core stories completed with acceptance criteria met (Stories 3-5 deferred/integrated)
- [x] Existing tracing tests pass unchanged (Python)
- [x] Existing tests pass unchanged (Rust)
- [x] New observability tests cover 90%+ of instrumented nodes
- [x] Parity tests pass for all test scenarios
- [x] Documentation updated:
  - [x] `docs/shared/YAML_REFERENCE.md` (new obs.* actions)
  - [x] `docs/python/development-guide.md`
  - [x] `docs/rust/development-guide.md`
  - [x] `docs/shared/architecture/observability-guide.md` (new)
- [x] No regression in existing features
- [x] CI pipeline includes parity tests

## Story Sequencing

```
Story 1 (Python Core) ──┐
                        ├──► Story 3 (Instrumentation) ──► Story 4 (API) ──► Story 5 (Parity)
Story 2 (Rust Core) ────┘
```

Stories 1 and 2 can be developed in parallel.
Story 3 depends on both 1 and 2.
Story 4 depends on 3.
Story 5 depends on 4.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 1.0 | Initial epic creation | Sarah (PO) |
| 2024-12-23 | 1.1 | Added @observable decorator pattern, Rust #[instrument] pattern, action registry integration | Sarah (PO) |
| 2024-12-23 | 1.2 | Status changed to Ready after addressing validation critical issues | Sarah (PO) |
| 2026-01-08 | 2.0 | Epic completed - core stories (001.1, 001.2) Done, stories 3-5 integrated into core | Auto-update |

---

## Story Manager Handoff

"Please develop detailed user stories for this cross-runtime observability epic. Key considerations:

- This is an enhancement to an existing system running Python + Rust
- Python integration points: `tracing.py`, `observability_actions.py`, action registry
- Rust integration points: `tracing` crate (unused), `engine/*.rs` modules
- Existing patterns to follow: Python's `TraceContext` and exporter pattern
- Critical compatibility requirements: Shared JSON log schema, opt-in activation
- Each story must include verification that existing functionality remains intact
- Stories 1 and 2 can be parallelized; subsequent stories are sequential

The epic should maintain system integrity while delivering standardized per-node logging with flow-level visibility across both runtimes."
