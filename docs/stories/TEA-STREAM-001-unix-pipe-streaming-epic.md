# Epic: TEA-STREAM-001 - Unix Pipe Streaming for Process Execution

## Status: Ready for Development

_Status updated: 2026-01-06 - All 5 child stories (TEA-STREAM-001.1 through 001.5) are Ready for Development._

**Created:** 2026-01-02
**Author:** Sarah (PO)
**Origin:** Correct-course discussion on TEA-PARALLEL-001 enhancement

## Epic Title
Unix Pipe Streaming with Hybrid State-Stream Model

## Epic Goal
Permitir que workflows com `parallel_strategy: process` utilizem pipes Unix para streaming de dados entre processos, mantendo compatibilidade com o modelo de estado tradicional através de canais de stream nomeados.

## Epic Description

### Existing System Context

- **Funcionalidade atual**: Fan-out paralelo via `ThreadPoolExecutor` e `ProcessPoolExecutor`
  no `stategraph.py`, com suporte a `parallel_strategy: thread | process | remote`
- **Modelo de dados**: Estado serializado (Dict) passado entre nós de forma discreta (batch)
- **Infraestrutura existente**: `ParallelConfig`, `RetryPolicy`, `CircuitBreaker` em `parallel.py`
- **YAML já suporta**: `parallel: true`, `fan_in:`, `parallel_strategy:` nas edges
- **Dependência**: TEA-PARALLEL-001 (executor abstraction + process backend)

### Enhancement Details

- **O que será adicionado**:
  - Canais de stream nomeados (`streams:`) nos nós YAML
  - Pipes Unix (OS-level file descriptors) entre processos
  - Broadcasting de streams via `tee` ou FIFOs para múltiplos consumidores
  - Modelo híbrido: estado tradicional + streams coexistem

- **Como integra**:
  - Extensão do `ProcessExecutor` para suportar pipes
  - Novo campo `streams:` no schema de nós YAML
  - Stream channels gerenciados pelo `StreamOrchestrator`

- **Success criteria**:
  - Workflows existentes funcionam sem modificação (backward compatible)
  - Processos podem enviar stdout para stdin de outros processos
  - Um stream pode ser broadcast para N processos consumidores
  - Estado e streams coexistem no mesmo workflow

### Hybrid Model Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    HYBRID STATE + STREAM MODEL                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Node A (producer)              Node B (consumer)                       │
│  ─────────────────              ─────────────────                       │
│                                                                         │
│  ┌─────────────┐                ┌─────────────┐                         │
│  │ run: block  │                │ run: block  │                         │
│  │ returns     │───[STATE]─────►│ receives    │                         │
│  │ {"count":N} │   (batch)      │ state       │                         │
│  └─────────────┘                └─────────────┘                         │
│        │                              ▲                                 │
│        │ stdout                       │ stdin                           │
│        ▼                              │                                 │
│  ┌─────────────┐                ┌─────────────┐                         │
│  │ streams:    │═══[PIPE]══════►│ streams:    │                         │
│  │ stdout: X   │   (real-time)  │ stdin: X    │                         │
│  └─────────────┘                └─────────────┘                         │
│                                                                         │
│  Two parallel data paths:                                               │
│  - STATE: Discrete, serialized, checkpoint-able                         │
│  - STREAM: Continuous, real-time, backpressure-aware                    │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### YAML Configuration Preview

```yaml
# Enable streaming for process strategy
settings:
  parallel:
    strategy: process
    streams:
      enabled: true
      buffer_size: 65536  # Pipe buffer (default: OS default ~64KB)

nodes:
  - name: data_producer
    run: |
      import sys
      for i in range(1000):
        print(f"line {i}", file=sys.stdout, flush=True)
      return {"status": "complete", "lines_produced": 1000}
    streams:
      stdout: data_stream  # Named stream channel

  - name: data_processor
    run: |
      import sys
      count = 0
      for line in sys.stdin:
        process(line)
        count += 1
      return {"status": "processed", "lines_consumed": count}
    streams:
      stdin: data_stream  # Connect to producer's stream

  - name: data_logger
    run: |
      import sys
      for line in sys.stdin:
        log_to_file(line)
      return {"logged": True}
    streams:
      stdin: data_stream  # Same stream - broadcast!

edges:
  - from: __start__
    to: data_producer
  - from: data_producer
    to: [data_processor, data_logger]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast  # tee semantics
    fan_in: merge_results
```

---

## Story Dependency Graph

```
                    ┌──────────────────────────┐
                    │ Story 1: Stream Channel  │
                    │ Infrastructure           │
                    │ (TEA-STREAM-001.1)       │
                    └──────────┬───────────────┘
                               │
              ┌────────────────┴────────────────┐
              │                                 │
              ▼                                 ▼
┌─────────────────────────┐       ┌─────────────────────────┐
│ Story 2: Pipe Executor  │       │ Story 3: Stream         │
│ Extension               │       │ Broadcasting            │
│ (TEA-STREAM-001.2)      │       │ (TEA-STREAM-001.3)      │
└───────────┬─────────────┘       └───────────┬─────────────┘
            │                                 │
            └────────────────┬────────────────┘
                             │
                             ▼
              ┌─────────────────────────┐
              │ Story 4: YAML           │
              │ Integration             │
              │ (TEA-STREAM-001.4)      │
              └───────────┬─────────────┘
                          │
                          ▼
              ┌─────────────────────────┐
              │ Story 5: Integration    │
              │ Testing + Documentation │
              │ (TEA-STREAM-001.5)      │
              └─────────────────────────┘
```

---

## Stories

### Story 1: Stream Channel Infrastructure

**ID**: TEA-STREAM-001.1
**Estimated Tests**: 18 scenarios
**Dependencies**: TEA-PARALLEL-001.1 (Executor Abstraction)

**As a** workflow developer,
**I want** named stream channels that can connect node outputs to inputs,
**So that** I can define streaming data flows alongside traditional state passing.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `StreamChannel` dataclass with name, direction (stdin/stdout/stderr), buffer_size | Unit test |
| AC2 | `StreamRegistry` manages named channels across workflow | Unit test |
| AC3 | Channels support multiple consumers (fan-out preparation) | Unit test |
| AC4 | Channel lifecycle: create → connect → stream → close | Integration test |
| AC5 | Graceful handling of broken pipes (SIGPIPE) | Unit test |
| AC6 | Configurable buffer size per channel | Unit test |

#### Technical Notes

**Files to Create**:
- `python/src/the_edge_agent/streams.py` - Core stream abstractions

**Implementation Sketch**:
```python
from dataclasses import dataclass, field
from typing import Literal, Optional, List
from enum import Enum
import os

class StreamDirection(Enum):
    STDIN = "stdin"
    STDOUT = "stdout"
    STDERR = "stderr"

@dataclass
class StreamChannel:
    """Named stream channel for inter-process communication."""
    name: str
    direction: StreamDirection
    buffer_size: int = 65536  # 64KB default

    # Runtime state
    read_fd: Optional[int] = None
    write_fd: Optional[int] = None
    consumers: List[str] = field(default_factory=list)

    def create_pipe(self) -> tuple[int, int]:
        """Create OS pipe, return (read_fd, write_fd)."""
        r, w = os.pipe()
        if self.buffer_size != 65536:
            # Resize pipe buffer on Linux
            try:
                import fcntl
                fcntl.fcntl(w, 1031, self.buffer_size)  # F_SETPIPE_SZ
            except (ImportError, OSError):
                pass  # Not supported, use default
        self.read_fd, self.write_fd = r, w
        return r, w

    def close(self):
        """Close pipe file descriptors."""
        if self.read_fd is not None:
            os.close(self.read_fd)
        if self.write_fd is not None:
            os.close(self.write_fd)


@dataclass
class StreamRegistry:
    """Registry of named stream channels for a workflow execution."""
    channels: dict[str, StreamChannel] = field(default_factory=dict)

    def register(self, name: str, direction: StreamDirection, **kwargs) -> StreamChannel:
        """Register a new stream channel."""
        if name in self.channels:
            # Channel exists - add consumer
            self.channels[name].consumers.append(kwargs.get("node_name", "unknown"))
            return self.channels[name]

        channel = StreamChannel(name=name, direction=direction, **kwargs)
        self.channels[name] = channel
        return channel

    def get(self, name: str) -> Optional[StreamChannel]:
        return self.channels.get(name)

    def cleanup(self):
        """Close all channels."""
        for channel in self.channels.values():
            channel.close()
```

---

### Story 2: Pipe Executor Extension

**ID**: TEA-STREAM-001.2
**Estimated Tests**: 22 scenarios
**Dependencies**: Story 1 (Stream Infrastructure), TEA-PARALLEL-001.1

**As a** workflow developer,
**I want** `ProcessExecutor` to wire pipes between processes based on stream channels,
**So that** stdout from one process flows to stdin of another in real-time.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ProcessExecutor` accepts `StreamRegistry` in execute() | Unit test |
| AC2 | Processes launched with stdout/stdin connected to pipes | Integration test |
| AC3 | Pipe wiring follows `streams:` configuration | Integration test |
| AC4 | SIGPIPE handled gracefully (consumer exits early) | Unit test |
| AC5 | Deadlock prevention: async pipe management | Integration test |
| AC6 | State still serialized/passed alongside streams | Integration test |
| AC7 | Process exit codes captured and reported | Unit test |

#### Technical Notes

**Files to Modify**:
- `python/src/the_edge_agent/parallel_executors.py` - Extend ProcessExecutor

**Implementation Sketch**:
```python
class ProcessExecutor:
    """Process-based executor with optional pipe streaming."""

    def execute_with_streams(
        self,
        flows: List[Dict],
        stream_registry: StreamRegistry,
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Execute flows with pipe connections."""
        import subprocess

        processes = []

        for flow in flows:
            node_name = flow["node_name"]
            node_streams = flow.get("streams", {})

            # Determine stdin/stdout based on stream config
            stdin_pipe = None
            stdout_pipe = None

            if "stdin" in node_streams:
                channel = stream_registry.get(node_streams["stdin"])
                if channel and channel.read_fd:
                    stdin_pipe = channel.read_fd

            if "stdout" in node_streams:
                channel = stream_registry.get(node_streams["stdout"])
                if channel and channel.write_fd:
                    stdout_pipe = channel.write_fd

            # Launch process
            proc = subprocess.Popen(
                self._build_command(flow),
                stdin=stdin_pipe if stdin_pipe else subprocess.PIPE,
                stdout=stdout_pipe if stdout_pipe else subprocess.PIPE,
                stderr=subprocess.PIPE
            )
            processes.append((flow, proc))

        # Wait and collect results
        return self._collect_results(processes, config)
```

---

### Story 3: Stream Broadcasting (Tee)

**ID**: TEA-STREAM-001.3
**Estimated Tests**: 16 scenarios
**Dependencies**: Story 1 (Stream Infrastructure)

**As a** workflow developer,
**I want** a single stream to broadcast to multiple consumer processes,
**So that** I can implement fan-out patterns with streaming data.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `stream_mode: broadcast` duplicates stream to N consumers | Integration test |
| AC2 | Uses named FIFOs (`mkfifo`) for multi-consumer support | Unit test |
| AC3 | `TeeOrchestrator` manages FIFO lifecycle | Unit test |
| AC4 | Automatic FIFO cleanup on workflow completion | Integration test |
| AC5 | Handles slow consumers without blocking producer | Integration test |
| AC6 | Error when consumer count exceeds system FIFO limit | Unit test |

#### Technical Notes

**Files to Create**:
- `python/src/the_edge_agent/stream_broadcast.py` - Tee/FIFO orchestration

**Implementation Sketch**:
```python
import os
import tempfile
import subprocess
from pathlib import Path
from typing import List, Optional

class TeeOrchestrator:
    """Orchestrate stream broadcasting via tee and FIFOs."""

    def __init__(self, work_dir: Optional[Path] = None):
        self.work_dir = work_dir or Path(tempfile.mkdtemp(prefix="tea_streams_"))
        self.fifos: List[Path] = []
        self.tee_proc: Optional[subprocess.Popen] = None

    def create_broadcast(
        self,
        source_fd: int,
        consumer_count: int
    ) -> List[Path]:
        """Create FIFOs for broadcasting and start tee process."""
        # Create FIFOs
        for i in range(consumer_count):
            fifo_path = self.work_dir / f"stream_{i}.fifo"
            os.mkfifo(fifo_path)
            self.fifos.append(fifo_path)

        # Start tee process
        # tee writes to all FIFOs except last, which gets stdout
        tee_targets = [str(f) for f in self.fifos[:-1]]

        self.tee_proc = subprocess.Popen(
            ["tee"] + tee_targets,
            stdin=source_fd,
            stdout=open(self.fifos[-1], 'w')
        )

        return self.fifos

    def cleanup(self):
        """Remove FIFOs and terminate tee."""
        if self.tee_proc:
            self.tee_proc.terminate()
            self.tee_proc.wait()

        for fifo in self.fifos:
            fifo.unlink(missing_ok=True)

        if self.work_dir.exists():
            self.work_dir.rmdir()
```

---

### Story 4: YAML Integration

**ID**: TEA-STREAM-001.4
**Estimated Tests**: 20 scenarios
**Dependencies**: Stories 1-3

**As a** workflow developer,
**I want** to configure streams in YAML using `streams:` on nodes and `stream_mode:` on edges,
**So that** I can declaratively define streaming workflows.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `streams:` block parsed on nodes (stdin, stdout, stderr) | Unit test |
| AC2 | `stream_mode: broadcast \| direct` parsed on edges | Unit test |
| AC3 | Stream channels auto-registered during YAML compilation | Integration test |
| AC4 | Validation: stream name referenced by consumer must exist | Unit test |
| AC5 | Validation: `streams:` only valid with `parallel_strategy: process` | Unit test |
| AC6 | Settings: `settings.parallel.streams.enabled`, `buffer_size` | Unit test |
| AC7 | Error messages guide user to correct configuration | Unit test |

#### Technical Notes

**Files to Modify**:
- `python/src/the_edge_agent/yaml_engine.py` - Parse streams config
- `python/src/the_edge_agent/yaml_nodes.py` - Add streams to node schema

**YAML Schema Addition**:
```yaml
# Node-level streams
nodes:
  - name: producer
    streams:
      stdout: my_stream      # Produce to named stream
      stderr: error_stream   # Optional: capture stderr too
    run: ...

  - name: consumer
    streams:
      stdin: my_stream       # Consume from named stream
    run: ...

# Edge-level stream mode
edges:
  - from: producer
    to: [consumer_a, consumer_b]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast   # broadcast | direct (default)
    fan_in: merger

# Global stream settings
settings:
  parallel:
    strategy: process
    streams:
      enabled: true          # Default: false (opt-in)
      buffer_size: 131072    # 128KB pipe buffer
      timeout: 300           # Stream timeout in seconds
```

---

### Story 5: Integration Testing & Documentation

**ID**: TEA-STREAM-001.5
**Estimated Tests**: 12 scenarios
**Dependencies**: Stories 1-4

**As a** workflow developer,
**I want** comprehensive examples and documentation for stream workflows,
**So that** I can learn and implement streaming patterns effectively.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | E2E test: producer → consumer pipe streaming | E2E test |
| AC2 | E2E test: producer → broadcast → N consumers | E2E test |
| AC3 | E2E test: hybrid state + stream workflow | E2E test |
| AC4 | Performance test: 1GB stream throughput | Performance test |
| AC5 | Documentation in `YAML_REFERENCE.md` | Manual review |
| AC6 | Example: `examples/yaml/stream_pipeline.yaml` | Manual review |
| AC7 | Example: `examples/yaml/stream_broadcast.yaml` | Manual review |
| AC8 | Troubleshooting guide for common stream errors | Manual review |

#### Documentation Updates

**YAML_REFERENCE.md additions**:
- New section: "Stream Channels"
- Subsection: "Hybrid State + Stream Model"
- Subsection: "Stream Broadcasting"
- Subsection: "Limitations and Caveats"

**Trade-offs Table**:

| Aspect | State Model | Stream Model |
|--------|-------------|--------------|
| Data transfer | Discrete batches | Continuous flow |
| Serialization | JSON/Pickle | Raw bytes |
| Checkpointing | Supported | Not supported |
| Backpressure | N/A | OS-managed |
| Memory usage | Load full state | Line-by-line |
| Use case | Structured data | Large files, logs |

---

## Feature Interaction Limitations

This section documents how stream channels interact with other TEA features.

### Interaction Matrix

| Feature | State Model | Stream Model | Notes |
|---------|-------------|--------------|-------|
| **Checkpointing** | ✅ Supported | ❌ Not supported | Streams cannot be checkpointed |
| **Interrupts** | ✅ Supported | ❌ Not supported | Cannot pause mid-stream |
| **`parallel_strategy: thread`** | ✅ Available | ❌ Not available | Pipes require separate processes |
| **`parallel_strategy: process`** | ✅ Available | ✅ Available | Primary use case |
| **`parallel_strategy: remote`** | ✅ Available | ⚠️ Future | Requires network streaming |
| **`ratelimit.wrap`** | ✅ Available | ⚠️ Per-process | No cross-stream coordination |
| **LTM (Long-Term Memory)** | ✅ Available | ✅ Available | State still passes through |
| **Secrets** | ✅ Available | ✅ Available | Via state, not stream |

### Platform Compatibility

| Platform | Support | Notes |
|----------|---------|-------|
| Linux | ✅ Full | Native pipes, FIFOs, `tee` |
| macOS | ✅ Full | Native pipes, FIFOs, `tee` |
| Windows | ❌ Not supported | No `mkfifo`, different pipe model |

### Checkpoint Incompatibility

**Critical Limitation:** Stream-enabled workflows cannot use checkpointing for nodes with active streams.

```yaml
# ❌ INVALID - Will error at compile time
nodes:
  - name: streamer
    streams:
      stdout: data
    interrupt_after: true  # ERROR: Cannot checkpoint stream node

# ✅ VALID - Checkpoint before/after stream section
nodes:
  - name: prepare
    interrupt_after: true  # OK - no streams
    run: return {"ready": True}

  - name: stream_producer
    streams:
      stdout: data
    run: ...  # No checkpoint here
```

---

## Risk Mitigation

| ID | Risk | Impact | Likelihood | Mitigation |
|----|------|--------|------------|------------|
| R1 | **Deadlock on full pipe buffer** | High | High | Async I/O, configurable buffer, timeout detection |
| R2 | **Broken pipe crashes workflow** | High | Medium | SIGPIPE handler, graceful degradation, clear errors |
| R3 | **No checkpoint/resume for streams** | Medium | High | Compile-time validation, documentation |
| R4 | **Windows incompatibility** | Medium | High | Platform check, clear error message |
| R5 | **Memory exhaustion (buffered reads)** | High | Medium | Documentation, example patterns, optional limit |
| R6 | **FIFO limit exceeded** | Low | Low | Pre-flight check, clear error |
| R7 | **Orphan processes on failure** | Medium | Medium | Process group management, cleanup handlers |

### Detailed Mitigations

**R1: Deadlock Prevention**
```python
# Use asyncio for non-blocking pipe management
async def manage_pipes(producer, consumers):
    async with asyncio.TaskGroup() as tg:
        tg.create_task(forward_stream(producer.stdout, consumers))
        tg.create_task(monitor_backpressure(producer, timeout=30))
```

**R2: SIGPIPE Handling**
```python
import signal

def ignore_sigpipe():
    """Ignore SIGPIPE to handle broken pipes gracefully."""
    signal.signal(signal.SIGPIPE, signal.SIG_IGN)
```

**R4: Platform Check**
```python
import sys

def validate_stream_support():
    if sys.platform == "win32":
        raise PlatformError(
            "Stream channels require Unix-like OS (Linux/macOS). "
            "Windows is not supported. Use parallel_strategy: thread instead."
        )
```

---

## Dependencies

### Runtime Dependencies

| Dependency | Required | Purpose |
|------------|----------|---------|
| Python 3.9+ | ✅ Yes | `asyncio.TaskGroup`, improved subprocess |
| `tee` command | ✅ Yes (for broadcast) | Stream duplication |
| `mkfifo` syscall | ✅ Yes (for broadcast) | Named pipes |
| Unix-like OS | ✅ Yes | Linux or macOS |

### Epic Dependencies

| Epic | Relationship | Status |
|------|--------------|--------|
| **TEA-PARALLEL-001** | Required | Ready for Development |
| TEA-PARALLEL-001.1 | Executor abstraction | Required for Story 2 |
| TEA-BUILTIN-012 | Secrets backend | Compatible (uses state) |

---

## Compatibility Requirements

- [x] Existing workflows without `streams:` work unchanged
- [x] Default `streams.enabled: false` (opt-in feature)
- [x] Existing tests pass without modification
- [x] `parallel_strategy: thread` unaffected
- [x] State serialization unchanged (streams are additive)

---

## Definition of Done

- [ ] **Story 1**: Stream channel infrastructure implemented
- [ ] **Story 2**: Pipe executor extension complete
- [ ] **Story 3**: Stream broadcasting with tee/FIFO working
- [ ] **Story 4**: YAML integration complete
- [ ] **Story 5**: Documentation and examples published
- [ ] All 88 test scenarios pass
- [ ] Platform validation (Linux, macOS)
- [ ] YAML_REFERENCE.md updated
- [ ] No regression in existing functionality
- [ ] Performance benchmarks documented

---

## Test Summary

| Story | Unit | Integration | E2E | Total |
|-------|------|-------------|-----|-------|
| TEA-STREAM-001.1 | 12 | 6 | 0 | 18 |
| TEA-STREAM-001.2 | 8 | 12 | 2 | 22 |
| TEA-STREAM-001.3 | 6 | 8 | 2 | 16 |
| TEA-STREAM-001.4 | 14 | 6 | 0 | 20 |
| TEA-STREAM-001.5 | 0 | 4 | 8 | 12 |
| **Total** | **40** | **36** | **12** | **88** |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial epic draft from correct-course discussion | Sarah (PO) |
