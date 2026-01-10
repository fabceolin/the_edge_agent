# Observability

> Debug, trace, and monitor your YAML agents with structured logging, distributed tracing, and flow-level visibility.

## Why This Matters

When workflows fail in production, you need answers fast. The Edge Agent's observability infrastructure gives you:

- **Flow-level tracing**: Every workflow execution gets a unique flow ID with complete event history
- **Structured logging**: JSON events with timestamps, node context, and metrics
- **Zero external dependencies**: Works fully offline without cloud services
- **Cross-runtime parity**: Python and Rust emit identical log schemas

## Quick Example

```yaml
name: traced-workflow
settings:
  observability:
    enabled: true
    level: info  # debug | info | warn | error
    buffer_size: 1000
    handlers:
      - type: console
        verbose: false
      - type: file
        path: "./logs/flow-{flow_id}.jsonl"

  opik:
    enabled: true
    project_name: my-agent
    llm_tracing: true

nodes:
  - name: generate
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "{{ state.prompt }}"
    store: response

  - name: check_health
    uses: opik.healthcheck
    store: opik_status

edges:
  - from: __start__
    to: generate
  - from: generate
    to: check_health
  - from: check_health
    to: __end__
```

## Key Features

| Feature | Description |
|---------|-------------|
| Flow-scoped logging | Each execution has a unique `flow_id` for isolated debugging |
| Ring buffer | Configurable bounded buffer (default 1000 events) prevents memory growth |
| Handler system | Console, File, and Callback handlers for flexible log routing |
| Automatic instrumentation | Entry/exit/error events for all nodes without manual code |
| LLM tracing | Native Opik integration captures tokens, costs, and latency |

## Integrations

| Integration | Status | Description |
|-------------|--------|-------------|
| **Opik** | Production | LLM observability with `track_openai()` wrapper for token usage, costs, and trace visualization |
| **OpenTelemetry** | Planned | Future integration for cloud-native observability (not yet implemented) |
| **Console** | Production | Human-readable or verbose formatted output to stdout |
| **File** | Production | JSON Lines format for log aggregation and analysis |
| **Callback** | Production | Custom handlers for integration with any logging system |

## Available Actions

| Action | Description |
|--------|-------------|
| `trace.start` | Begin a trace span with optional metadata |
| `trace.log` | Log a custom event within the current span |
| `trace.end` | End the current trace span |
| `obs.get_flow_log` | Get complete structured trace for a flow by flow_id |
| `obs.log_event` | Log a custom event to the observability stream |
| `obs.query_events` | Query logged events with filters (node, level, time range) |
| `opik.healthcheck` | Validate Opik connectivity and authentication |

### Action Examples

```yaml
# Get complete flow trace
- name: get_trace
  uses: obs.get_flow_log
  with:
    flow_id: "{{ state._observability.flow_id }}"
  store: flow_log

# Query only error events
- name: get_errors
  uses: obs.query_events
  with:
    flow_id: "{{ state._observability.flow_id }}"
    filters:
      level: error
  store: errors

# Validate Opik connection
- name: check_opik
  uses: opik.healthcheck
  store: opik_status
```

[Full Actions Reference (Python)](../python/actions-reference.md) | [Full Actions Reference (Rust)](../rust/actions-reference.md)

## Technical Context

### Runtime Support

| Feature | Python | Rust | WASM |
|---------|--------|------|------|
| ObservabilityContext | Done (TEA-OBS-001.1) | Done (TEA-OBS-001.2) | N/A |
| EventStream ring buffer | Done | Done | N/A |
| Console/File/Callback handlers | Done | Done | Callback only |
| YAML configuration | Done | Done | Done |
| Opik integration | Done (TEA-BUILTIN-005.*) | Done (TEA-OBS-002) | Done (TEA-OBS-002) |
| OpenTelemetry | Planned | Planned | Planned |

### Log Event Schema

Both Python and Rust emit events conforming to this structure:

```json
{
  "flow_id": "550e8400-e29b-41d4-a716-446655440000",
  "span_id": "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
  "parent_id": null,
  "node": "llm.call",
  "level": "info",
  "timestamp": 1703347200.123,
  "event_type": "entry",
  "message": "Starting llm.call",
  "data": {},
  "metrics": {
    "duration_ms": 123.45,
    "tokens": 150,
    "cost_usd": 0.003
  }
}
```

### Configuration Precedence

When using Opik, configuration follows this precedence (highest to lowest):

1. **Constructor parameters** - Passed directly to `YAMLEngine()`
2. **Environment variables** - `OPIK_API_KEY`, `OPIK_PROJECT_NAME`, `OPIK_WORKSPACE`, `OPIK_URL_OVERRIDE`
3. **YAML settings** - `settings.opik.*` in your agent file
4. **Defaults** - `project_name: "the-edge-agent"`, disabled by default

### Rust Native Opik Configuration (TEA-OBS-002)

The Rust runtime supports Opik via the `settings.opik` configuration or environment variables. The handler gracefully degrades when no API key is available.

#### YAML Configuration

```yaml
name: rust-traced-agent
settings:
  opik:
    project_name: my-rust-agent
    workspace: default
    url_override: https://custom-opik.example.com/api  # optional
    batch_size: 100    # events per batch (default: 100)
    flush_interval_ms: 5000  # flush interval (default: 5000ms)

nodes:
  - name: generate
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "{{ state.prompt }}"
```

#### Environment Variables

```bash
export OPIK_API_KEY="your-api-key"
export OPIK_PROJECT_NAME="my-rust-agent"
export OPIK_WORKSPACE="default"
export OPIK_URL_OVERRIDE="https://custom-opik.example.com/api"
```

#### Graceful Degradation

The Rust Opik handler operates in "graceful degradation" mode:
- If `OPIK_API_KEY` is not set, the handler logs a warning and disables tracing
- No runtime errors are thrown for missing configuration
- LLM calls continue to work normally without tracing

### WASM Browser Integration (TEA-OBS-002)

The `tea-wasm-llm` package provides Opik tracing for browser-based LLM applications. Traces are sent via JavaScript callback, allowing integration with any transport mechanism.

#### Basic Setup

```typescript
import { initTeaWasmLlm, initOpikTracing, executeYaml } from 'tea-wasm-llm';

// Initialize WASM module
await initTeaWasmLlm();

// Enable Opik tracing (traces sent via callback)
await initOpikTracing({
  projectName: 'my-browser-agent',
  workspace: 'default',
  verbose: true
});

// Execute workflow - LLM calls are automatically traced
const result = await executeYaml(yamlContent, { prompt: 'Hello!' });
```

#### Custom Trace Handler

```typescript
import { registerOpikCallback } from 'tea-wasm-llm';

// Register custom callback for trace handling
registerOpikCallback(async (traceJson: string) => {
  const trace = JSON.parse(traceJson);

  // Send to your backend, analytics, or Opik API
  await fetch('/api/traces', {
    method: 'POST',
    body: traceJson,
    headers: { 'Content-Type': 'application/json' }
  });
});
```

#### Direct API Integration

```typescript
import { registerOpikCallback } from 'tea-wasm-llm';

const OPIK_API_KEY = 'your-api-key';
const OPIK_API_URL = 'https://www.comet.com/opik/api';

registerOpikCallback(async (traceJson: string) => {
  await fetch(`${OPIK_API_URL}/v1/private/traces`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${OPIK_API_KEY}`
    },
    body: traceJson
  });
});
```

#### Trace Schema

WASM Opik traces conform to this structure:

```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "name": "llm_call",
  "project_name": "my-browser-agent",
  "start_time": "2024-01-15T12:30:45.123Z",
  "end_time": "2024-01-15T12:30:47.456Z",
  "input": {
    "prompt": "Hello!",
    "max_tokens": 100
  },
  "output": {
    "content": "Hi there!"
  },
  "usage": {
    "prompt_tokens": 5,
    "completion_tokens": 3,
    "total_tokens": 8
  },
  "metadata": {
    "model": "gemma-3-1b",
    "runtime": "wasm"
  }
}
```

## Learn More

### Documentation

- [Observability Epic](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-OBS-001-hybrid-observability-epic.md) - Architecture and design decisions
- [Python Getting Started](../python/getting-started.md) - Quick start with observability
- [Rust Getting Started](../rust/getting-started.md) - Quick start with Rust runtime
- [YAML Reference](../shared/YAML_REFERENCE.md) - Complete settings reference

### Stories

| Story | Description | Status |
|-------|-------------|--------|
| [TEA-OBS-001.1](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-OBS-001.1-python-observability-core.md) | Python ObservabilityContext infrastructure | Done |
| [TEA-OBS-001.2](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-OBS-001.2-rust-observability-core.md) | Rust ObservabilityContext infrastructure | Done |
| [TEA-OBS-002](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-OBS-002.opik-wasm-rust-integration.md) | Opik integration for Rust and WASM | Done |
| [TEA-BUILTIN-005.1](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-005.1.opik-exporter.md) | OpikExporter backend | Done |
| [TEA-BUILTIN-005.2](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-005.2.opik-llm-instrumentation.md) | Native Opik LLM instrumentation | Done |
| [TEA-BUILTIN-005.3](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-005.3.opik-configuration.md) | Opik configuration and utilities | Done |

### External Resources

- [Comet Opik Documentation](https://www.comet.com/docs/opik/) - Official Opik platform docs
- [Python tracing crate](https://docs.rs/tracing/latest/tracing/) - Rust tracing ecosystem
