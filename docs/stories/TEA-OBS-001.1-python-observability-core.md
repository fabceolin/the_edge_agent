# TEA-OBS-001.1: Core ObservabilityContext Infrastructure (Python)

## Status: Draft

## Story

**As a** developer building workflows with The Edge Agent,
**I want** a standardized observability infrastructure that provides flow-scoped logging with structured events and configurable handlers,
**so that** I can debug complex workflows by accessing aggregated flow-level logs without cloud dependencies.

## Epic Reference

Parent Epic: [TEA-OBS-001: Hybrid Observability Architecture](./TEA-OBS-001-hybrid-observability-epic.md)

## Context

### Current System
The Edge Agent Python runtime currently provides:
- `TraceContext` class for hierarchical span management (`python/src/the_edge_agent/tracing.py`)
- Thread-safe span stacks for parallel execution
- Multiple exporters: `ConsoleExporter`, `FileExporter`, `CallbackExporter`
- Basic observability actions: `trace.start`, `trace.log`, `trace.end` (`python/src/the_edge_agent/actions/observability_actions.py`)

### Enhancement Goal
This story creates a new `ObservabilityContext` layer that wraps and extends the existing `TraceContext` infrastructure to provide:
1. **Flow-scoped logging**: Each workflow execution gets a unique flow ID with aggregated event stream
2. **Event buffering**: Configurable ring buffer for flow-level log retention
3. **Declarative configuration**: YAML-based configuration for log levels and handlers
4. **Programmatic access**: API for retrieving complete flow logs by flow ID

**Key Principle**: This is **composition, not replacement**. `TraceContext` remains unchanged and continues to work as-is. `ObservabilityContext` builds on top of it.

## Acceptance Criteria

### AC1: ObservabilityContext Class with Flow-Scoped Logging
- [ ] Create `ObservabilityContext` class in `python/src/the_edge_agent/observability.py`
- [ ] Constructor accepts `flow_id` (UUID string), `config` (dict), and optional `trace_context` (TraceContext instance)
- [ ] If `trace_context` not provided, creates internal TraceContext automatically
- [ ] Methods:
  - `log(node: str, level: str, event_type: str, message: str = None, data: dict = None, metrics: dict = None)` → None
  - `start_node_span(node: str, metadata: dict = None)` → span_id
  - `end_node_span(node: str, status: str = "ok", error: str = None)` → completed_span
- [ ] All log events include `flow_id`, `span_id`, `node`, `level`, `timestamp`, `event_type`

### AC2: EventStream with Configurable Ring Buffer
- [ ] Create `EventStream` class in `python/src/the_edge_agent/observability.py`
- [ ] Constructor accepts `max_size` (default: 1000), `handlers` (list of exporter-like objects)
- [ ] Uses `collections.deque(maxlen=max_size)` for bounded ring buffer
- [ ] Methods:
  - `append(event: dict)` → None (adds event to buffer and forwards to handlers)
  - `get_all()` → List[dict] (returns all buffered events in order)
  - `query(filters: dict)` → List[dict] (filter by node, level, event_type, time range)
  - `clear()` → None (empties the buffer)
- [ ] Thread-safe with `threading.Lock`

### AC3: Handlers - Console, File, Callback (Extending Existing Exporters)
- [ ] `ObservabilityContext` integrates with existing `TraceContext` exporters
- [ ] Create adapter pattern: `EventStreamHandler` protocol with `handle(event: dict)` method
- [ ] Implement adapters:
  - `ConsoleHandler(verbose: bool = False)` → wraps ConsoleExporter
  - `FileHandler(path: str)` → wraps FileExporter for JSON lines
  - `CallbackHandler(callback: Callable)` → wraps CallbackExporter
- [ ] Handlers receive both individual events and completed spans

### AC4: Integration with Existing TraceContext (Composition, Not Replacement)
- [ ] `ObservabilityContext` contains a `TraceContext` instance (composition)
- [ ] Node span start/end delegates to `TraceContext.start_span()` / `end_span()`
- [ ] Individual log events use `TraceContext.log_event()`
- [ ] Completed spans from `TraceContext` are transformed into flow log events
- [ ] All existing `TraceContext` API remains unchanged and functional
- [ ] Zero breaking changes to existing code using `TraceContext` directly

### AC5: Configuration via YAML
- [ ] Extend `YAMLEngine.__init__()` to accept `observability` config:
  ```yaml
  observability:
    enabled: true
    level: info  # debug | info | warn | error
    buffer_size: 1000
    handlers:
      - type: console
        verbose: false
      - type: file
        path: "./logs/flow-{flow_id}.jsonl"
  ```
- [ ] Parse config in `YAMLEngine.__init__()` and create `ObservabilityContext` when enabled
- [ ] Store in `YAMLEngine._observability_context` (None if disabled)
- [ ] Inject `flow_id` into state under `_observability.flow_id` key
- [ ] Template support: `{flow_id}` in file paths replaced with actual flow ID

### AC6: get_flow_log(flow_id) Returns Structured Trace
- [ ] Add method `ObservabilityContext.get_flow_log()` → dict:
  ```python
  {
    "flow_id": "550e8400-e29b-41d4-a716-446655440000",
    "events": [...],  # All buffered events in order
    "spans": [...],   # All completed spans from TraceContext
    "metrics": {      # Aggregate metrics
      "total_duration_ms": float,
      "node_count": int,
      "error_count": int,
      "event_count": int
    },
    "timeline": [...]  # Ordered timeline of all activity
  }
  ```
- [ ] Timeline merges events and spans, sorted by timestamp
- [ ] Span hierarchy preserved (parent_id relationships)
- [ ] Thread-safe access to flow log

## Tasks / Subtasks

### Task 1: Create ObservabilityContext Core Infrastructure (AC1, AC2)

- [ ] Create file: `python/src/the_edge_agent/observability.py`
- [ ] Import dependencies: `collections.deque`, `threading`, `uuid`, `time`, `typing`
- [ ] Implement `EventStream` class:
  - [ ] Constructor: `__init__(max_size=1000, handlers=None)`
  - [ ] Instance variables: `_buffer: deque`, `_handlers: list`, `_lock: threading.Lock`
  - [ ] Method: `append(event: dict) -> None`
  - [ ] Method: `get_all() -> List[dict]`
  - [ ] Method: `query(filters: dict) -> List[dict]`
  - [ ] Method: `clear() -> None`
- [ ] Implement `ObservabilityContext` class:
  - [ ] Constructor: `__init__(flow_id: str, config: dict, trace_context: TraceContext = None)`
  - [ ] Auto-create `TraceContext` if not provided
  - [ ] Method: `log(node, level, event_type, message=None, data=None, metrics=None)`
  - [ ] Method: `start_node_span(node, metadata=None) -> span_id`
  - [ ] Method: `end_node_span(node, status="ok", error=None) -> completed_span`
  - [ ] Method: `get_flow_log() -> dict`

### Task 2: Create Handler Adapters (AC3)

- [ ] Define `EventStreamHandler` protocol in `observability.py`
- [ ] Implement `ConsoleHandler`:
  - [ ] Wraps `ConsoleExporter` from `tracing.py`
  - [ ] Constructor: `__init__(verbose: bool = False)`
  - [ ] Method: `handle(event: dict)` formats and prints event
- [ ] Implement `FileHandler`:
  - [ ] Constructor: `__init__(path: str)`
  - [ ] Method: `handle(event: dict)` writes JSON line to file
  - [ ] Thread-safe file writes
- [ ] Implement `CallbackHandler`:
  - [ ] Constructor: `__init__(callback: Callable[[dict], None])`
  - [ ] Method: `handle(event: dict)` calls user callback
  - [ ] Swallow exceptions to prevent workflow crashes

### Task 3: Integrate with YAMLEngine (AC4, AC5)

- [ ] In `yaml_engine.py`, add `observability` parameter to `__init__()`
- [ ] Parse config structure (enabled, level, buffer_size, handlers)
- [ ] Create handler instances from config
- [ ] Generate unique `flow_id` for this execution (UUID)
- [ ] Create `ObservabilityContext` instance if enabled
- [ ] Store in `self._observability_context`
- [ ] Inject flow_id into workflow state:
  ```python
  state['_observability'] = {
    'flow_id': self._observability_context.flow_id,
    'enabled': True
  }
  ```
- [ ] Modify `_execute_node()` to use observability:
  - [ ] Before node: `start_node_span(node_name)`
  - [ ] After node: `end_node_span(node_name, status="ok")`
  - [ ] On error: `end_node_span(node_name, status="error", error=str(e))`

### Task 4: Implement get_flow_log API (AC6)

- [ ] In `ObservabilityContext.get_flow_log()`:
  - [ ] Retrieve all events from `_event_stream.get_all()`
  - [ ] Retrieve completed spans from `_trace_context.completed_spans`
  - [ ] Merge into unified timeline (sort by timestamp)
  - [ ] Calculate aggregate metrics
  - [ ] Return structured dict
- [ ] Add `obs.get_flow_log` action in `observability_actions.py`:
  - [ ] Register as `registry['obs.get_flow_log']`

### Task 5: Export in Package API (AC4)

- [ ] In `__init__.py`, add imports:
  ```python
  from .observability import ObservabilityContext, EventStream
  from .observability import ConsoleHandler, FileHandler, CallbackHandler
  ```
- [ ] Add to `__all__`

## Dev Notes

### Source Tree Context

**Location**: `python/src/the_edge_agent/`

| File | Action | Purpose |
|------|--------|---------|
| `tracing.py` | READ ONLY | Existing TraceContext pattern |
| `yaml_engine.py` | MODIFY | Add observability config parsing |
| `observability.py` | CREATE | New ObservabilityContext, EventStream |
| `actions/observability_actions.py` | MODIFY | Add obs.get_flow_log action |

### Existing Patterns to Follow

#### Composition Pattern (Full Implementation)
```python
import time
import uuid
from collections import deque
from typing import Dict, Any, Optional, List
from .tracing import TraceContext, ConsoleExporter, FileExporter, CallbackExporter


class ObservabilityContext:
    """
    Flow-scoped observability context that wraps TraceContext.

    This class COMPOSES (not extends) TraceContext to add:
    - Flow-level event aggregation
    - Ring buffer for bounded log retention
    - Structured log schema for cross-runtime parity
    """

    def __init__(
        self,
        flow_id: str = None,
        config: Dict[str, Any] = None,
        trace_context: TraceContext = None
    ):
        self.flow_id = flow_id or str(uuid.uuid4())
        self.config = config or {}
        self._level = self.config.get('level', 'info')

        # COMPOSITION: Wrap existing TraceContext, don't replace it
        self._trace_context = trace_context or TraceContext(exporters=[])

        # Event stream with configurable buffer size
        buffer_size = self.config.get('buffer_size', 1000)
        handlers = self._create_handlers(self.config.get('handlers', []))
        self._event_stream = EventStream(max_size=buffer_size, handlers=handlers)

        # Track node-to-span mapping for proper span management
        self._node_spans: Dict[str, str] = {}

    def _create_handlers(self, handler_configs: List[Dict]) -> List:
        """Create handler instances from config."""
        handlers = []
        for cfg in handler_configs:
            handler_type = cfg.get('type')
            if handler_type == 'console':
                handlers.append(ConsoleHandler(verbose=cfg.get('verbose', False)))
            elif handler_type == 'file':
                path = cfg.get('path', './logs/flow.jsonl')
                # Template substitution for {flow_id}
                path = path.replace('{flow_id}', self.flow_id)
                handlers.append(FileHandler(path=path))
            elif handler_type == 'callback':
                if 'callback' in cfg:
                    handlers.append(CallbackHandler(callback=cfg['callback']))
        return handlers

    def log(
        self,
        node: str,
        level: str,
        event_type: str,
        message: str = None,
        data: Dict = None,
        metrics: Dict = None
    ) -> None:
        """Log a structured event to the event stream."""
        event = self._create_event(node, level, event_type, message, data, metrics)
        if self._should_log(level):
            self._event_stream.append(event)
            # Also log to TraceContext for span correlation
            self._trace_context.log_event(message=message, metrics=metrics)

    def start_node_span(self, node: str, metadata: Dict = None) -> str:
        """Start a span for a node and emit entry event."""
        # Delegate to TraceContext
        span = self._trace_context.start_span(name=node, metadata=metadata)
        span_id = span['span_id']
        self._node_spans[node] = span_id

        # Emit entry event
        self.log(node, 'info', 'entry', f"Starting {node}", data=metadata)
        return span_id

    def end_node_span(self, node: str, status: str = "ok", error: str = None) -> Optional[Dict]:
        """End a node's span and emit exit/error event."""
        # Delegate to TraceContext
        completed = self._trace_context.end_span(status=status, error=error)

        # Emit exit or error event
        if status == "error":
            self.log(node, 'error', 'error', f"Error in {node}: {error}")
        else:
            duration = completed.get('duration_ms', 0) if completed else 0
            self.log(node, 'info', 'exit', f"Completed {node}",
                    metrics={'duration_ms': duration})

        # Clean up node-span mapping
        self._node_spans.pop(node, None)
        return completed

    def get_flow_log(self) -> Dict[str, Any]:
        """Return complete flow trace with events, spans, and metrics."""
        events = self._event_stream.get_all()
        spans = self._trace_context.completed_spans.copy()

        # Build unified timeline
        timeline = sorted(
            [{'type': 'event', **e} for e in events] +
            [{'type': 'span', **s} for s in spans],
            key=lambda x: x.get('timestamp', x.get('start_time', 0))
        )

        # Calculate aggregate metrics
        error_count = sum(1 for e in events if e.get('event_type') == 'error')
        node_count = len(set(e.get('node') for e in events))
        total_duration = sum(s.get('duration_ms', 0) for s in spans)

        return {
            'flow_id': self.flow_id,
            'events': events,
            'spans': spans,
            'metrics': {
                'total_duration_ms': total_duration,
                'node_count': node_count,
                'error_count': error_count,
                'event_count': len(events)
            },
            'timeline': timeline
        }

    def _create_event(self, node, level, event_type, message, data, metrics) -> Dict:
        """Create a log event conforming to shared schema."""
        current_span = self._trace_context.current_span()
        return {
            'flow_id': self.flow_id,
            'span_id': current_span['span_id'] if current_span else None,
            'parent_id': current_span.get('parent_id') if current_span else None,
            'node': node,
            'level': level,
            'timestamp': time.time(),
            'event_type': event_type,
            'message': message,
            'data': data or {},
            'metrics': metrics or {}
        }

    def _should_log(self, level: str) -> bool:
        """Check if event level meets configured threshold."""
        levels = {'debug': 0, 'info': 1, 'warn': 2, 'error': 3}
        return levels.get(level, 1) >= levels.get(self._level, 1)
```

#### Thread Safety Pattern
```python
class EventStream:
    """Thread-safe ring buffer for flow events."""

    def __init__(self, max_size: int = 1000, handlers: List = None):
        self._lock = threading.Lock()
        self._buffer = deque(maxlen=max_size)
        self._handlers = handlers or []

    def append(self, event: Dict) -> None:
        """Add event to buffer and forward to handlers."""
        with self._lock:
            self._buffer.append(event)
        # Handler calls outside lock to prevent deadlocks
        for handler in self._handlers:
            try:
                handler.handle(event)
            except Exception:
                pass  # Swallow errors to not crash workflow

    def get_all(self) -> List[Dict]:
        """Return all events as list (thread-safe copy)."""
        with self._lock:
            return list(self._buffer)

    def query(self, filters: Dict) -> List[Dict]:
        """Filter events by node, level, event_type, or time range."""
        events = self.get_all()

        if 'node' in filters:
            import fnmatch
            events = [e for e in events if fnmatch.fnmatch(e.get('node', ''), filters['node'])]
        if 'level' in filters:
            events = [e for e in events if e.get('level') == filters['level']]
        if 'event_type' in filters:
            events = [e for e in events if e.get('event_type') == filters['event_type']]
        if 'start_time' in filters:
            events = [e for e in events if e.get('timestamp', 0) >= filters['start_time']]
        if 'end_time' in filters:
            events = [e for e in events if e.get('timestamp', 0) <= filters['end_time']]

        return events

    def clear(self) -> None:
        """Clear all events from buffer."""
        with self._lock:
            self._buffer.clear()
```

### Handler Implementations

```python
import json
import threading
from pathlib import Path
from typing import Dict, Any, Callable, Protocol


class EventStreamHandler(Protocol):
    """Protocol for event handlers."""
    def handle(self, event: Dict[str, Any]) -> None: ...


class ConsoleHandler:
    """Print events to console."""

    def __init__(self, verbose: bool = False):
        self.verbose = verbose

    def handle(self, event: Dict[str, Any]) -> None:
        node = event.get('node', 'unknown')
        level = event.get('level', 'info').upper()
        event_type = event.get('event_type', '?')
        message = event.get('message', '')

        if self.verbose:
            print(f"[{level}] {node} ({event_type}): {message}")
            if event.get('data'):
                print(f"  data: {event['data']}")
            if event.get('metrics'):
                print(f"  metrics: {event['metrics']}")
        else:
            icon = {'entry': '→', 'exit': '←', 'error': '✗', 'metric': '📊'}.get(event_type, '•')
            print(f"[{level}] {icon} {node}: {message}")


class FileHandler:
    """Write events to JSON lines file."""

    def __init__(self, path: str):
        self.path = Path(path)
        self._lock = threading.Lock()
        self.path.parent.mkdir(parents=True, exist_ok=True)

    def handle(self, event: Dict[str, Any]) -> None:
        with self._lock:
            with open(self.path, 'a', encoding='utf-8') as f:
                f.write(json.dumps(event, default=str) + '\n')


class CallbackHandler:
    """Call user-provided function with each event."""

    def __init__(self, callback: Callable[[Dict[str, Any]], None]):
        self.callback = callback

    def handle(self, event: Dict[str, Any]) -> None:
        try:
            self.callback(event)
        except Exception:
            pass  # Swallow to not crash workflow
```

### Shared Log Schema

All events must conform to this structure:
```python
{
  "flow_id": "uuid-string",
  "span_id": "uuid-string",
  "parent_id": "optional-uuid",
  "node": "llm.call",
  "level": "info",  # debug | info | warn | error
  "timestamp": 1703347200.123,  # Unix timestamp (float)
  "event_type": "entry",  # entry | exit | error | metric
  "message": "Optional message",
  "data": {},
  "metrics": {"duration_ms": 123.45}
}
```

### Parity Expected Output Examples

These examples define the exact JSON structure that both Python and Rust must emit for cross-runtime parity tests.

#### Example 1: Simple Linear Flow (3 nodes)
**Input YAML** (`examples/observability/parity/simple-linear.yaml`):
```yaml
name: simple-linear-test
observability:
  enabled: true
  level: info

nodes:
  - name: start
    run: |
      return {"value": 1}
  - name: process
    run: |
      return {"value": state["value"] * 2}
  - name: end
    run: |
      return {"result": state["value"]}

edges:
  - from: __start__
    to: start
  - from: start
    to: process
  - from: process
    to: end
  - from: end
    to: __end__
```

**Expected Output** (`examples/observability/parity-expected/simple-linear.json`):
```json
{
  "flow_id": "{{UUID}}",
  "events": [
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "start",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "entry",
      "message": "Starting start",
      "data": {},
      "metrics": {}
    },
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "start",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "exit",
      "message": "Completed start",
      "data": {},
      "metrics": {"duration_ms": "{{FLOAT}}"}
    },
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "process",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "entry",
      "message": "Starting process",
      "data": {},
      "metrics": {}
    },
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "process",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "exit",
      "message": "Completed process",
      "data": {},
      "metrics": {"duration_ms": "{{FLOAT}}"}
    },
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "end",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "entry",
      "message": "Starting end",
      "data": {},
      "metrics": {}
    },
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "end",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "exit",
      "message": "Completed end",
      "data": {},
      "metrics": {"duration_ms": "{{FLOAT}}"}
    }
  ],
  "metrics": {
    "total_duration_ms": "{{FLOAT}}",
    "node_count": 3,
    "error_count": 0,
    "event_count": 6
  }
}
```

#### Example 2: Error Handling Flow
**Expected Output** (`examples/observability/parity-expected/error-flow.json`):
```json
{
  "flow_id": "{{UUID}}",
  "events": [
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "failing_node",
      "level": "info",
      "timestamp": "{{FLOAT}}",
      "event_type": "entry",
      "message": "Starting failing_node",
      "data": {},
      "metrics": {}
    },
    {
      "flow_id": "{{UUID}}",
      "span_id": "{{UUID}}",
      "parent_id": null,
      "node": "failing_node",
      "level": "error",
      "timestamp": "{{FLOAT}}",
      "event_type": "error",
      "message": "Error in failing_node: Division by zero",
      "data": {},
      "metrics": {}
    }
  ],
  "metrics": {
    "total_duration_ms": "{{FLOAT}}",
    "node_count": 1,
    "error_count": 1,
    "event_count": 2
  }
}
```

#### Parity Validation Rules
1. **UUID fields**: Match pattern `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`
2. **Timestamp fields**: Must be float > 0, order must match expected sequence
3. **Event order**: Entry events before exit events for same node
4. **Field names**: Exact snake_case match (no camelCase)
5. **Metrics aggregation**: Sums must be mathematically correct

### Compatibility Guarantees

**MUST maintain:**
- All existing `TraceContext` tests pass unchanged
- `trace.start`, `trace.log`, `trace.end` actions work as before
- Workflows without observability config run without overhead

**MUST NOT:**
- Modify `tracing.py` (read-only)
- Break backward compatibility with existing YAML agents
- Add performance overhead when observability disabled (< 1ms per node)

## Testing

### Test File Location
`python/tests/test_observability_core.py` (new file)

### Test Categories

#### Unit Tests (12 tests)
1. `test_event_stream_append_and_get_all()` - AC2
2. `test_event_stream_ring_buffer_max_size()` - AC2
3. `test_event_stream_query_filtering()` - AC2
4. `test_event_stream_thread_safety()` - AC2
5. `test_observability_context_flow_id_injection()` - AC1
6. `test_observability_context_start_end_span()` - AC1, AC4
7. `test_observability_context_log_events()` - AC1
8. `test_observability_context_shared_schema_compliance()` - AC1
9. `test_observability_context_composition_pattern()` - AC4
10. `test_console_handler_formatting()` - AC3
11. `test_file_handler_json_lines()` - AC3
12. `test_callback_handler_error_resilience()` - AC3

#### Integration Tests (8 tests)
13. `test_yaml_engine_observability_config_parsing()` - AC5
14. `test_yaml_engine_flow_id_in_state()` - AC5
15. `test_yaml_engine_node_auto_instrumentation()` - AC4, AC5
16. `test_yaml_engine_observability_disabled()` - AC5
17. `test_yaml_engine_multiple_handlers()` - AC5
18. `test_get_flow_log_complete_trace()` - AC6
19. `test_get_flow_log_aggregate_metrics()` - AC6
20. `test_obs_get_flow_log_action()` - AC6

### pytest Commands
```bash
cd python && pytest tests/test_observability_core.py -v
cd python && pytest tests/test_observability_core.py --cov=the_edge_agent.observability
```

## Definition of Done

- [ ] All acceptance criteria (AC1-AC6) implemented and verified
- [ ] All tasks (1-5) and subtasks completed
- [ ] 20 new tests written and passing
- [ ] All existing tracing tests pass unchanged
- [ ] Code coverage > 90%
- [ ] Classes exported in `__init__.py`
- [ ] `obs.get_flow_log` action registered
- [ ] Shared log schema enforced
- [ ] No breaking changes to existing `TraceContext` API
- [ ] Performance: < 1ms overhead when disabled

## Dependencies

**Blocked by:** None (foundational story)

**Blocks:**
- TEA-OBS-001.2 (Rust Core) - shares log schema
- TEA-OBS-001.3 (Builtin Instrumentation) - requires ObservabilityContext
- TEA-OBS-001.4 (Flow Log Access API) - extends `obs.*` actions

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 1.0 | Initial story creation | Sarah (PO) |
