# Story: TEA-STREAM-001.4 - YAML Integration

## Status: Ready for Development

**Epic**: [TEA-STREAM-001 - Unix Pipe Streaming](./TEA-STREAM-001-unix-pipe-streaming-epic.md)
**Estimated Tests**: 20 scenarios
**Dependencies**:
- TEA-STREAM-001.1 (Stream Channel Infrastructure)
- TEA-STREAM-001.2 (Pipe Executor Extension)
- TEA-STREAM-001.3 (Stream Broadcasting)

---

## User Story

**As a** workflow developer,
**I want** to configure streams in YAML using `streams:` on nodes and `stream_mode:` on edges,
**So that** I can declaratively define streaming workflows.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `streams:` block parsed on nodes (stdin, stdout, stderr) | Unit test: YAML parsing |
| AC2 | `stream_mode: broadcast \| direct` parsed on edges | Unit test: edge parsing |
| AC3 | Stream channels auto-registered during YAML compilation | Integration test: registry populated |
| AC4 | Validation: stream name referenced by consumer must exist | Unit test: validation error |
| AC5 | Validation: `streams:` only valid with `parallel_strategy: process` | Unit test: strategy check |
| AC6 | Settings: `settings.parallel.streams.enabled`, `buffer_size` | Unit test: settings parsing |
| AC7 | Error messages guide user to correct configuration | Unit test: message content |

---

## Technical Design

### Files to Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/yaml_engine.py` | Modify | Parse streams config, create registry |
| `python/src/the_edge_agent/yaml_nodes.py` | Modify | Add streams to node schema |

### YAML Schema

```yaml
# ═══════════════════════════════════════════════════════════════════════
# GLOBAL STREAM SETTINGS
# ═══════════════════════════════════════════════════════════════════════

settings:
  parallel:
    strategy: process          # Required for streams
    streams:
      enabled: true            # Default: false (opt-in)
      buffer_size: 131072      # 128KB pipe buffer (default: 65536)
      timeout: 300             # Stream timeout in seconds (default: no timeout)

# ═══════════════════════════════════════════════════════════════════════
# NODE-LEVEL STREAMS
# ═══════════════════════════════════════════════════════════════════════

nodes:
  # Producer node - outputs to named stream
  - name: data_generator
    run: |
      import sys
      for i in range(10000):
        print(f"record_{i}", file=sys.stdout, flush=True)
      return {"records_produced": 10000}
    streams:
      stdout: main_data_stream    # Stream channel name
      stderr: error_stream        # Optional: capture errors

  # Consumer node - reads from named stream
  - name: data_processor
    run: |
      import sys
      count = 0
      for line in sys.stdin:
        process(line.strip())
        count += 1
      return {"records_processed": count}
    streams:
      stdin: main_data_stream     # Must match a producer's stdout

  # Transform node - both input and output
  - name: data_transformer
    run: |
      import sys
      for line in sys.stdin:
        transformed = transform(line.strip())
        print(transformed, file=sys.stdout, flush=True)
      return {"status": "complete"}
    streams:
      stdin: input_stream
      stdout: output_stream

# ═══════════════════════════════════════════════════════════════════════
# EDGE-LEVEL STREAM MODE
# ═══════════════════════════════════════════════════════════════════════

edges:
  # Standard parallel edge (no streaming)
  - from: prepare
    to: [branch_a, branch_b]
    parallel: true
    parallel_strategy: process
    fan_in: merger

  # Streaming parallel edge with broadcast
  - from: generator
    to: [processor_a, processor_b, processor_c]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast       # broadcast | direct (default)
    fan_in: aggregator

# ═══════════════════════════════════════════════════════════════════════
# STREAM MODE OPTIONS
# ═══════════════════════════════════════════════════════════════════════

# stream_mode: direct (default)
#   - Single producer to single consumer
#   - Direct pipe connection
#   - Error if multiple consumers configured

# stream_mode: broadcast
#   - Single producer to multiple consumers
#   - Uses tee/FIFO for duplication
#   - All consumers receive identical data
```

### Implementation Approach

```python
# yaml_nodes.py additions

from dataclasses import dataclass
from typing import Optional, Dict, Literal


@dataclass
class NodeStreamsConfig:
    """Stream configuration for a node."""
    stdin: Optional[str] = None      # Channel name to read from
    stdout: Optional[str] = None     # Channel name to write to
    stderr: Optional[str] = None     # Channel name for errors


@dataclass
class StreamSettings:
    """Global stream settings."""
    enabled: bool = False
    buffer_size: int = 65536
    timeout: Optional[int] = None


# yaml_engine.py additions

from the_edge_agent.streams import StreamRegistry, StreamDirection, validate_platform


class YAMLEngine:
    """Extended with stream support."""

    def _parse_node_streams(self, node_config: dict) -> Optional[NodeStreamsConfig]:
        """Parse streams block from node configuration."""
        streams = node_config.get("streams")
        if not streams:
            return None

        return NodeStreamsConfig(
            stdin=streams.get("stdin"),
            stdout=streams.get("stdout"),
            stderr=streams.get("stderr")
        )

    def _parse_stream_settings(self, settings: dict) -> StreamSettings:
        """Parse global stream settings."""
        parallel = settings.get("parallel", {})
        stream_config = parallel.get("streams", {})

        return StreamSettings(
            enabled=stream_config.get("enabled", False),
            buffer_size=stream_config.get("buffer_size", 65536),
            timeout=stream_config.get("timeout")
        )

    def _parse_edge_stream_mode(
        self,
        edge_config: dict
    ) -> Literal["direct", "broadcast"]:
        """Parse stream_mode from edge configuration."""
        mode = edge_config.get("stream_mode", "direct")
        if mode not in ("direct", "broadcast"):
            raise ValueError(
                f"Invalid stream_mode: '{mode}'. "
                f"Must be 'direct' or 'broadcast'."
            )
        return mode

    def _build_stream_registry(self, yaml_config: dict) -> StreamRegistry:
        """
        Build StreamRegistry from parsed YAML configuration.

        Validates stream configuration and creates registry with all channels.
        """
        registry = StreamRegistry()
        settings = self._parse_stream_settings(yaml_config.get("settings", {}))

        if not settings.enabled:
            return registry  # Empty registry if streams not enabled

        # Validate platform
        validate_platform()

        # Validate parallel strategy
        parallel_config = yaml_config.get("settings", {}).get("parallel", {})
        strategy = parallel_config.get("strategy", "thread")
        if strategy != "process":
            raise ValueError(
                f"Streams require parallel_strategy: process, got '{strategy}'. "
                f"Add 'settings.parallel.strategy: process' to your YAML."
            )

        # Register all stream channels from nodes
        producers = {}  # channel_name -> node_name
        consumers = {}  # channel_name -> [node_names]

        for node in yaml_config.get("nodes", []):
            node_name = node.get("name")
            streams = self._parse_node_streams(node)

            if not streams:
                continue

            # Register producer (stdout/stderr)
            if streams.stdout:
                if streams.stdout in producers:
                    raise ValueError(
                        f"Stream '{streams.stdout}' has multiple producers: "
                        f"'{producers[streams.stdout]}' and '{node_name}'. "
                        f"Each stream can only have one producer."
                    )
                producers[streams.stdout] = node_name
                registry.register(
                    name=streams.stdout,
                    direction=StreamDirection.STDOUT,
                    node_name=node_name,
                    buffer_size=settings.buffer_size
                )

            if streams.stderr:
                if streams.stderr in producers:
                    raise ValueError(
                        f"Stream '{streams.stderr}' has multiple producers."
                    )
                producers[streams.stderr] = node_name
                registry.register(
                    name=streams.stderr,
                    direction=StreamDirection.STDERR,
                    node_name=node_name,
                    buffer_size=settings.buffer_size
                )

            # Register consumer (stdin)
            if streams.stdin:
                if streams.stdin not in consumers:
                    consumers[streams.stdin] = []
                consumers[streams.stdin].append(node_name)
                registry.register(
                    name=streams.stdin,
                    direction=StreamDirection.STDIN,
                    node_name=node_name,
                    buffer_size=settings.buffer_size
                )

        # Validate all consumed streams have producers
        for channel_name, consumer_nodes in consumers.items():
            if channel_name not in producers:
                raise ValueError(
                    f"Stream '{channel_name}' is consumed by {consumer_nodes} "
                    f"but has no producer. Add a node with "
                    f"'streams.stdout: {channel_name}'."
                )

        # Validate stream_mode on edges
        for edge in yaml_config.get("edges", []):
            stream_mode = edge.get("stream_mode")
            if stream_mode:
                targets = edge.get("to", [])
                if isinstance(targets, str):
                    targets = [targets]

                if stream_mode == "direct" and len(targets) > 1:
                    raise ValueError(
                        f"stream_mode: direct requires single target, "
                        f"got {len(targets)}. Use stream_mode: broadcast "
                        f"for multiple targets."
                    )

        return registry

    def _validate_no_interrupts_in_stream_scope(
        self,
        yaml_config: dict,
        registry: StreamRegistry
    ) -> None:
        """
        Validate that nodes with streams don't have interrupt points.

        Stream nodes cannot be checkpointed because stream position
        cannot be restored.
        """
        if not registry.channels:
            return

        stream_nodes = set()
        for node in yaml_config.get("nodes", []):
            streams = node.get("streams")
            if streams:
                stream_nodes.add(node.get("name"))

        for node in yaml_config.get("nodes", []):
            node_name = node.get("name")
            if node_name not in stream_nodes:
                continue

            if node.get("interrupt_before"):
                raise ValueError(
                    f"Node '{node_name}' has streams and interrupt_before=True. "
                    f"Stream nodes cannot be checkpointed. Remove interrupt_before "
                    f"or move checkpoint to a non-streaming node."
                )

            if node.get("interrupt_after"):
                raise ValueError(
                    f"Node '{node_name}' has streams and interrupt_after=True. "
                    f"Stream nodes cannot be checkpointed. Remove interrupt_after "
                    f"or move checkpoint to a non-streaming node."
                )
```

### Error Messages

| Scenario | Error Message |
|----------|---------------|
| Streams with wrong strategy | `Streams require parallel_strategy: process, got 'thread'. Add 'settings.parallel.strategy: process' to your YAML.` |
| Multiple producers | `Stream 'X' has multiple producers: 'A' and 'B'. Each stream can only have one producer.` |
| Missing producer | `Stream 'X' is consumed by ['A', 'B'] but has no producer. Add a node with 'streams.stdout: X'.` |
| Direct mode multi-target | `stream_mode: direct requires single target, got 3. Use stream_mode: broadcast for multiple targets.` |
| Interrupt on stream node | `Node 'X' has streams and interrupt_before=True. Stream nodes cannot be checkpointed.` |
| Windows platform | `Stream channels require Unix-like OS (Linux/macOS). Windows is not supported.` |

---

## Testing

### Test Location

`python/tests/test_yaml_streams.py`

### Test Scenarios (20 total)

#### AC1: Node Streams Parsing (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-001 | Unit | P0 | Parse streams.stdin from node |
| 001.4-UNIT-002 | Unit | P0 | Parse streams.stdout from node |
| 001.4-UNIT-003 | Unit | P1 | Parse streams.stderr from node |
| 001.4-UNIT-004 | Unit | P1 | Node without streams returns None |

#### AC2: Edge Stream Mode (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-005 | Unit | P0 | Parse stream_mode: direct |
| 001.4-UNIT-006 | Unit | P0 | Parse stream_mode: broadcast |
| 001.4-UNIT-007 | Unit | P1 | Invalid stream_mode raises error |

#### AC3: Registry Building (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-INT-001 | Integration | P0 | Registry contains all declared channels |
| 001.4-INT-002 | Integration | P1 | Producer channels registered as STDOUT |
| 001.4-INT-003 | Integration | P1 | Consumer channels registered as STDIN |

#### AC4: Producer/Consumer Validation (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-008 | Unit | P0 | Error if consumer references missing producer |
| 001.4-UNIT-009 | Unit | P0 | Error if multiple producers for same stream |
| 001.4-UNIT-010 | Unit | P1 | Valid config passes validation |

#### AC5: Strategy Validation (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-011 | Unit | P0 | Error if streams with strategy: thread |
| 001.4-UNIT-012 | Unit | P1 | Streams work with strategy: process |

#### AC6: Settings Parsing (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-013 | Unit | P0 | Parse streams.enabled (default false) |
| 001.4-UNIT-014 | Unit | P1 | Parse streams.buffer_size |
| 001.4-UNIT-015 | Unit | P1 | Parse streams.timeout |

#### AC7: Error Messages (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-016 | Unit | P0 | Error messages include fix suggestions |
| 001.4-UNIT-017 | Unit | P1 | Platform error is clear and actionable |

---

## Definition of Done

- [ ] `streams:` block parsing on nodes
- [ ] `stream_mode:` parsing on edges
- [ ] `StreamRegistry` built from YAML config
- [ ] Producer/consumer validation
- [ ] Strategy validation (require process)
- [ ] Interrupt point validation
- [ ] Settings parsing (enabled, buffer_size, timeout)
- [ ] Clear error messages with fix suggestions
- [ ] All 20 test scenarios pass
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| YAML schema breaking change | Low | streams: is additive, no existing fields changed |
| Complex validation logic | Medium | Extract to separate validator class |
| Error messages unclear | Medium | Include exact fix in each error message |

---

## Notes for Developer

1. **Opt-in feature**: `streams.enabled: false` by default. Existing workflows unchanged.

2. **Validation order**:
   1. Check platform (fail fast on Windows)
   2. Check strategy (must be process)
   3. Parse all nodes and build registry
   4. Validate producer/consumer relationships
   5. Validate no interrupts on stream nodes

3. **Error message format**: Include the problem, the cause, and the fix. Example:
   ```
   Stream 'X' is consumed but has no producer.
   Add a node with 'streams.stdout: X'.
   ```

4. **Schema documentation**: Update `docs/shared/YAML_REFERENCE.md` with new schema.

---

## QA Notes

**Test Design Reviewed**: 2026-01-02 by Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 26 (exceeds story estimate of 20) |
| Unit Tests | 18 (69%) |
| Integration Tests | 8 (31%) |
| E2E Tests | 0 (deferred to TEA-STREAM-001.5) |
| P0 Coverage | 12 tests |
| P1 Coverage | 11 tests |
| P2 Coverage | 3 tests |

All 7 acceptance criteria have explicit test coverage with Given-When-Then specifications.

### Risk Areas Identified

1. **Validation Logic Complexity** (Medium Risk)
   - Multiple validation rules (producer/consumer, strategy, interrupts) create potential for edge case failures
   - Mitigated by: Isolated unit tests for each validation rule (001.4-UNIT-010 through 001.4-UNIT-015)

2. **Error Message Quality** (Medium Risk)
   - Actionable error messages are critical for developer experience
   - Mitigated by: Dedicated error message tests (001.4-UNIT-021 through 001.4-INT-008)

3. **Platform Dependency** (Low Risk)
   - Windows unsupported; must fail fast with clear message
   - Mitigated by: Mock-based platform error testing

### Recommended Test Scenarios

**Critical Path (P0)**:
- Parse `streams:` block on nodes (stdin, stdout)
- Parse `stream_mode:` on edges (direct, broadcast)
- Validate producer/consumer relationships
- Validate strategy requirement (must be `process`)
- Error messages include actionable fixes
- Interrupt point validation on stream nodes

**Secondary (P1)**:
- stderr stream parsing
- Default behavior verification
- Buffer size and timeout settings
- Complex multi-stream topology validation

### Concerns or Blockers

1. **Interrupt Validation Gap**: Story mentions interrupt validation in technical design but not in original test scenarios. Test design adds 2 tests (001.4-UNIT-025, 001.4-UNIT-026) to cover this.

2. **E2E Deferral**: No E2E tests in this story. Full streaming workflow validation depends on TEA-STREAM-001.5 completion. Ensure integration story is prioritized to avoid late-stage integration issues.

3. **Dependency Chain**: This story depends on TEA-STREAM-001.1, 001.2, and 001.3. Validate those stories are complete before implementation begins.

### Gate Recommendation

**Status**: READY FOR DEVELOPMENT

Test design is comprehensive with 26 scenarios covering all acceptance criteria. Risk mitigations are mapped to specific tests. Proceed with implementation.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 1.0 | Story created from epic | Sarah (PO) |
| 2026-01-02 | 1.1 | Added QA Notes from test design review | Quinn (QA) |
