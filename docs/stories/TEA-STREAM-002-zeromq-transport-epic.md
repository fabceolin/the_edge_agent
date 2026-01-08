# Epic: TEA-STREAM-002 - ZeroMQ Transport for Stream Channels

## Status: Draft

**Created:** 2026-01-08
**Author:** Sarah (PO)
**Origin:** Enhancement request for cross-platform, network-capable streaming alternative

## Epic Title
ZeroMQ Transport Backend for Cross-Platform and Distributed Streaming

## Epic Goal
Provide a ZeroMQ-based transport alternative to Unix pipes for stream channels, enabling cross-platform (Windows) support, network-capable distributed streaming, and flexible messaging patterns (PUB/SUB, PUSH/PULL, REQ/REP).

## Epic Description

### Existing System Context

- **Current streaming**: TEA-STREAM-001 implemented Unix pipe-based streaming (`streams.py`, `stream_broadcast.py`)
- **Transport model**: OS pipes via `os.pipe()` and FIFOs via `mkfifo`
- **Platform support**: Linux/macOS only (Windows not supported)
- **Topology**: Local only (same machine, same process tree)
- **Patterns**: Direct (1:1) and Broadcast (1:N) via `tee` command
- **YAML config**: `settings.parallel.streams.enabled`, `stream_mode: broadcast|direct`

### Enhancement Details

- **What will be added**:
  - ZeroMQ socket-based transport layer (`zeromq_transport.py`)
  - Three messaging patterns: PUB/SUB, PUSH/PULL, REQ/REP
  - Two transport protocols: IPC (local) and TCP (network)
  - Windows compatibility for all patterns
  - YAML configuration: `transport: zeromq` with pattern/protocol options
  - Connection discovery for distributed workflows

- **How it integrates**:
  - Alternative backend selectable via `settings.parallel.streams.transport`
  - Existing `StreamChannel` interface extended with transport abstraction
  - `StreamRegistry` gains transport factory methods
  - Coexists with Unix pipes (non-breaking change)

- **Success criteria**:
  - All existing stream workflows work unchanged (backward compatible)
  - ZeroMQ transport works on Windows, Linux, macOS
  - TCP transport enables remote node communication
  - Pattern selection optimizes for use case (broadcast vs load-balanced)
  - Performance comparable to Unix pipes for local IPC

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     STREAM TRANSPORT ABSTRACTION                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  YAML Config                        Transport Factory                       │
│  ────────────                       ─────────────────                       │
│  transport: unix    ───────────────►  UnixPipeTransport (current)           │
│  transport: zeromq  ───────────────►  ZeroMQTransport (new)                 │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ZeroMQ Transport Layer                                                     │
│  ──────────────────────                                                     │
│                                                                             │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐         │
│  │   PUB/SUB       │    │   PUSH/PULL     │    │   REQ/REP       │         │
│  │   (fan-out)     │    │   (pipeline)    │    │   (RPC-style)   │         │
│  └────────┬────────┘    └────────┬────────┘    └────────┬────────┘         │
│           │                      │                      │                   │
│           ▼                      ▼                      ▼                   │
│  ┌─────────────────────────────────────────────────────────────────┐       │
│  │                     Socket Manager                               │       │
│  │  - Connection lifecycle (bind/connect)                          │       │
│  │  - High-water mark configuration                                │       │
│  │  - Timeout handling                                              │       │
│  └────────┬─────────────────────┬──────────────────────────────────┘       │
│           │                     │                                           │
│           ▼                     ▼                                           │
│  ┌─────────────────┐    ┌─────────────────┐                                │
│  │   IPC Transport │    │   TCP Transport │                                │
│  │   ipc:///tmp/   │    │   tcp://host:p  │                                │
│  │   (local, fast) │    │   (network)     │                                │
│  └─────────────────┘    └─────────────────┘                                │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### ZeroMQ Pattern Selection Guide

| Pattern | Topology | Message Flow | Use Case |
|---------|----------|--------------|----------|
| **PUB/SUB** | 1:N | Push (fire-forget) | Event broadcast, log streaming, metrics |
| **PUSH/PULL** | N:1 or 1:N | Load-balanced | Pipeline stages, work distribution |
| **REQ/REP** | 1:1 | Request-response | RPC, confirmations, synchronous calls |

### YAML Configuration Preview

```yaml
name: distributed-processor
description: Process data across multiple machines

settings:
  parallel:
    strategy: process
    streams:
      enabled: true
      transport: zeromq        # unix (default) | zeromq
      protocol: tcp            # ipc (default) | tcp
      pattern: push_pull       # pub_sub | push_pull | req_rep

      # ZeroMQ-specific settings
      zeromq:
        bind_address: "tcp://*:5555"          # For producers
        connect_addresses:                     # For consumers
          - "tcp://worker1:5555"
          - "tcp://worker2:5555"
        high_water_mark: 1000                  # Message queue limit
        linger: 1000                           # Close timeout (ms)
        reconnect_interval: 100                # Reconnect delay (ms)

nodes:
  - name: producer
    run: |
      import sys
      for i in range(10000):
        print(f"task_{i}", file=sys.stdout, flush=True)
      return {"produced": 10000}
    streams:
      stdout: task_queue

  - name: worker
    run: |
      import sys
      count = 0
      for line in sys.stdin:
        result = process(line)
        count += 1
      return {"processed": count}
    streams:
      stdin: task_queue

edges:
  - from: __start__
    to: producer
  - from: producer
    to: [worker_1, worker_2, worker_3]
    parallel: true
    parallel_strategy: process
    stream_mode: load_balance    # New mode for PUSH/PULL
    fan_in: aggregator
```

---

## Story Dependency Graph

```
                    ┌──────────────────────────────────┐
                    │ Story 1: Transport Abstraction   │
                    │ Layer                            │
                    │ (TEA-STREAM-002.1)               │
                    └──────────────┬───────────────────┘
                                   │
              ┌────────────────────┼────────────────────┐
              │                    │                    │
              ▼                    ▼                    ▼
┌─────────────────────┐ ┌─────────────────────┐ ┌─────────────────────┐
│ Story 2: PUB/SUB    │ │ Story 3: PUSH/PULL  │ │ Story 4: REQ/REP    │
│ Pattern             │ │ Pattern             │ │ Pattern             │
│ (TEA-STREAM-002.2)  │ │ (TEA-STREAM-002.3)  │ │ (TEA-STREAM-002.4)  │
└──────────┬──────────┘ └──────────┬──────────┘ └──────────┬──────────┘
           │                       │                       │
           └───────────────────────┼───────────────────────┘
                                   │
                                   ▼
                    ┌──────────────────────────────────┐
                    │ Story 5: YAML Integration &      │
                    │ TCP Discovery                    │
                    │ (TEA-STREAM-002.5)               │
                    └──────────────┬───────────────────┘
                                   │
                                   ▼
                    ┌──────────────────────────────────┐
                    │ Story 6: Integration Testing &   │
                    │ Documentation                    │
                    │ (TEA-STREAM-002.6)               │
                    └──────────────────────────────────┘
```

---

## Stories

### Story 1: Transport Abstraction Layer

**ID**: TEA-STREAM-002.1
**Estimated Tests**: 20 scenarios
**Dependencies**: TEA-STREAM-001 (completed)

**As a** workflow developer,
**I want** a transport abstraction layer for stream channels,
**So that** I can switch between Unix pipes and ZeroMQ without changing workflow logic.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `Transport` base class defines `send()`, `receive()`, `close()` interface | Unit test |
| AC2 | `UnixPipeTransport` wraps existing `StreamChannel` with Transport interface | Unit test |
| AC3 | `TransportFactory` creates transport based on `transport:` setting | Unit test |
| AC4 | Existing Unix pipe workflows work unchanged when `transport: unix` (default) | Integration test |
| AC5 | `StreamRegistry` accepts `transport_factory` parameter | Unit test |
| AC6 | Transport errors raise `TransportError` with helpful messages | Unit test |

#### Technical Notes

**Files to Create**:
- `python/src/the_edge_agent/transports/__init__.py`
- `python/src/the_edge_agent/transports/base.py` - Abstract base class
- `python/src/the_edge_agent/transports/unix.py` - Unix pipe wrapper
- `python/src/the_edge_agent/transports/factory.py` - Factory function

**Implementation Sketch**:
```python
from abc import ABC, abstractmethod
from typing import Optional, Any
from dataclasses import dataclass

class TransportError(Exception):
    """Base exception for transport errors."""
    pass

@dataclass
class TransportConfig:
    """Configuration for a transport instance."""
    transport_type: str  # "unix" | "zeromq"
    protocol: str = "ipc"  # "ipc" | "tcp"
    pattern: str = "push_pull"  # "pub_sub" | "push_pull" | "req_rep"
    options: dict = None

class Transport(ABC):
    """Abstract base class for stream transports."""

    @abstractmethod
    def bind(self, address: str) -> None:
        """Bind transport as producer (server side)."""
        pass

    @abstractmethod
    def connect(self, address: str) -> None:
        """Connect transport as consumer (client side)."""
        pass

    @abstractmethod
    def send(self, data: bytes, flags: int = 0) -> None:
        """Send data through transport."""
        pass

    @abstractmethod
    def receive(self, flags: int = 0) -> bytes:
        """Receive data from transport."""
        pass

    @abstractmethod
    def close(self) -> None:
        """Close transport and release resources."""
        pass

    @property
    @abstractmethod
    def is_connected(self) -> bool:
        """Check if transport is connected."""
        pass


def create_transport(config: TransportConfig) -> Transport:
    """Factory function to create appropriate transport."""
    if config.transport_type == "unix":
        from .unix import UnixPipeTransport
        return UnixPipeTransport(config)
    elif config.transport_type == "zeromq":
        from .zeromq import ZeroMQTransport
        return ZeroMQTransport(config)
    else:
        raise TransportError(f"Unknown transport type: {config.transport_type}")
```

---

### Story 2: PUB/SUB Pattern Implementation

**ID**: TEA-STREAM-002.2
**Estimated Tests**: 18 scenarios
**Dependencies**: Story 1 (Transport Abstraction)

**As a** workflow developer,
**I want** ZeroMQ PUB/SUB pattern for stream channels,
**So that** I can broadcast data to multiple subscribers without coordinator overhead.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ZeroMQPubSubTransport` implements `Transport` interface | Unit test |
| AC2 | Publisher binds to address, subscribers connect | Integration test |
| AC3 | All subscribers receive all published messages | Integration test |
| AC4 | Topic filtering with `subscribe_filter` option | Unit test |
| AC5 | High-water mark prevents memory exhaustion | Unit test |
| AC6 | Works with IPC protocol on Linux/macOS | Integration test |
| AC7 | Works with IPC protocol on Windows | Integration test |
| AC8 | Works with TCP protocol across machines | Integration test |

#### Technical Notes

**Files to Create**:
- `python/src/the_edge_agent/transports/zeromq.py` - ZeroMQ transport implementations

**Implementation Sketch**:
```python
import zmq
from .base import Transport, TransportConfig

class ZeroMQPubSubTransport(Transport):
    """PUB/SUB pattern transport using ZeroMQ."""

    def __init__(self, config: TransportConfig, context: zmq.Context = None):
        self.config = config
        self.context = context or zmq.Context.instance()
        self.socket = None
        self._is_publisher = False

    def bind(self, address: str) -> None:
        """Bind as publisher."""
        self.socket = self.context.socket(zmq.PUB)
        self.socket.setsockopt(zmq.SNDHWM, self.config.options.get("high_water_mark", 1000))
        self.socket.bind(address)
        self._is_publisher = True

    def connect(self, address: str) -> None:
        """Connect as subscriber."""
        self.socket = self.context.socket(zmq.SUB)
        self.socket.setsockopt(zmq.RCVHWM, self.config.options.get("high_water_mark", 1000))

        # Subscribe to all messages by default, or use topic filter
        topic_filter = self.config.options.get("subscribe_filter", b"")
        self.socket.setsockopt(zmq.SUBSCRIBE, topic_filter)

        self.socket.connect(address)

    def send(self, data: bytes, flags: int = 0) -> None:
        """Publish message."""
        self.socket.send(data, flags)

    def receive(self, flags: int = 0) -> bytes:
        """Receive message (blocking by default)."""
        return self.socket.recv(flags)

    def close(self) -> None:
        if self.socket:
            self.socket.close()
            self.socket = None
```

---

### Story 3: PUSH/PULL Pattern Implementation

**ID**: TEA-STREAM-002.3
**Estimated Tests**: 18 scenarios
**Dependencies**: Story 1 (Transport Abstraction)

**As a** workflow developer,
**I want** ZeroMQ PUSH/PULL pattern for stream channels,
**So that** I can distribute work across multiple workers with automatic load balancing.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ZeroMQPushPullTransport` implements `Transport` interface | Unit test |
| AC2 | PUSH socket distributes messages round-robin to PULL sockets | Integration test |
| AC3 | Each message is received by exactly one worker (no duplicates) | Integration test |
| AC4 | Fair queuing when workers have different processing speeds | Integration test |
| AC5 | Works with N:1 topology (many pushers, one puller) | Integration test |
| AC6 | Works with 1:N topology (one pusher, many pullers) | Integration test |
| AC7 | High-water mark prevents slow consumer blocking | Unit test |
| AC8 | Works on Windows with IPC transport | Integration test |

#### Technical Notes

**Implementation Sketch**:
```python
class ZeroMQPushPullTransport(Transport):
    """PUSH/PULL pattern transport using ZeroMQ."""

    def __init__(self, config: TransportConfig, context: zmq.Context = None):
        self.config = config
        self.context = context or zmq.Context.instance()
        self.socket = None
        self._is_pusher = False

    def bind(self, address: str) -> None:
        """Bind as pusher (work distributor)."""
        self.socket = self.context.socket(zmq.PUSH)
        self._configure_socket()
        self.socket.bind(address)
        self._is_pusher = True

    def connect(self, address: str) -> None:
        """Connect as puller (worker)."""
        self.socket = self.context.socket(zmq.PULL)
        self._configure_socket()
        self.socket.connect(address)

    def _configure_socket(self):
        hwm = self.config.options.get("high_water_mark", 1000)
        self.socket.setsockopt(zmq.SNDHWM, hwm)
        self.socket.setsockopt(zmq.RCVHWM, hwm)

        linger = self.config.options.get("linger", 1000)
        self.socket.setsockopt(zmq.LINGER, linger)
```

---

### Story 4: REQ/REP Pattern Implementation

**ID**: TEA-STREAM-002.4
**Estimated Tests**: 16 scenarios
**Dependencies**: Story 1 (Transport Abstraction)

**As a** workflow developer,
**I want** ZeroMQ REQ/REP pattern for stream channels,
**So that** I can implement synchronous request-response communication between nodes.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ZeroMQReqRepTransport` implements `Transport` interface | Unit test |
| AC2 | REQ socket enforces send-receive-send-receive order | Unit test |
| AC3 | REP socket enforces receive-send-receive-send order | Unit test |
| AC4 | Timeout on `receive()` with `timeout` option | Unit test |
| AC5 | Works for RPC-style node communication | Integration test |
| AC6 | Error handling for protocol violations | Unit test |
| AC7 | Multiple REQ clients can connect to one REP server | Integration test |
| AC8 | Works on Windows with IPC transport | Integration test |

#### Technical Notes

**Implementation Sketch**:
```python
class ZeroMQReqRepTransport(Transport):
    """REQ/REP pattern transport for synchronous request-response."""

    def __init__(self, config: TransportConfig, context: zmq.Context = None):
        self.config = config
        self.context = context or zmq.Context.instance()
        self.socket = None
        self._is_server = False
        self._expecting_reply = False

    def bind(self, address: str) -> None:
        """Bind as REP (server)."""
        self.socket = self.context.socket(zmq.REP)
        self._configure_socket()
        self.socket.bind(address)
        self._is_server = True

    def connect(self, address: str) -> None:
        """Connect as REQ (client)."""
        self.socket = self.context.socket(zmq.REQ)
        self._configure_socket()
        self.socket.connect(address)

    def send(self, data: bytes, flags: int = 0) -> None:
        """Send request (client) or reply (server)."""
        self.socket.send(data, flags)
        if not self._is_server:
            self._expecting_reply = True

    def receive(self, flags: int = 0) -> bytes:
        """Receive reply (client) or request (server)."""
        timeout = self.config.options.get("timeout")
        if timeout:
            self.socket.setsockopt(zmq.RCVTIMEO, timeout)

        try:
            data = self.socket.recv(flags)
            if not self._is_server:
                self._expecting_reply = False
            return data
        except zmq.Again:
            raise TransportError("Receive timeout")
```

---

### Story 5: YAML Integration & TCP Discovery

**ID**: TEA-STREAM-002.5
**Estimated Tests**: 22 scenarios
**Dependencies**: Stories 1-4

**As a** workflow developer,
**I want** to configure ZeroMQ transports in YAML,
**So that** I can declaratively define distributed streaming workflows.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `settings.parallel.streams.transport: zeromq` enables ZeroMQ | Unit test |
| AC2 | `settings.parallel.streams.protocol: tcp` enables TCP transport | Unit test |
| AC3 | `settings.parallel.streams.pattern: pub_sub|push_pull|req_rep` | Unit test |
| AC4 | `stream_mode: load_balance` uses PUSH/PULL pattern | Unit test |
| AC5 | `stream_mode: broadcast` uses PUB/SUB pattern | Unit test |
| AC6 | Per-stream protocol override via `streams.protocol` on node | Unit test |
| AC7 | `zeromq.bind_address` and `zeromq.connect_addresses` configuration | Integration test |
| AC8 | Auto-discovery via `zeromq.discovery: static|env|consul` | Integration test |
| AC9 | Validation: TCP requires explicit addresses | Unit test |
| AC10 | Environment variable expansion in addresses | Unit test |

#### Technical Notes

**Files to Modify**:
- `python/src/the_edge_agent/yaml_engine.py` - Parse zeromq settings
- `python/src/the_edge_agent/yaml_nodes.py` - Add zeromq schema

**YAML Schema Addition**:
```yaml
# Global ZeroMQ settings
settings:
  parallel:
    strategy: process
    streams:
      enabled: true
      transport: zeromq           # unix | zeromq
      protocol: ipc               # ipc | tcp
      pattern: push_pull          # pub_sub | push_pull | req_rep

      zeromq:
        # IPC settings (local)
        ipc_dir: /tmp/tea-zmq     # Directory for IPC sockets

        # TCP settings (network)
        bind_address: "tcp://*:5555"
        connect_addresses:
          - "tcp://worker1:5555"
          - "tcp://worker2:5555"

        # Socket options
        high_water_mark: 1000     # Queue depth before blocking
        linger: 1000              # Close timeout (ms)
        reconnect_interval: 100   # Reconnect delay (ms)

        # Discovery (optional)
        discovery: static          # static | env | consul

# Per-node transport override
nodes:
  - name: distributed_producer
    streams:
      stdout: task_queue
      protocol: tcp                # Override global protocol
      bind_address: "tcp://*:5556" # Override global address
```

---

### Story 6: Integration Testing & Documentation

**ID**: TEA-STREAM-002.6
**Estimated Tests**: 15 scenarios
**Dependencies**: Stories 1-5

**As a** workflow developer,
**I want** comprehensive documentation and examples for ZeroMQ streaming,
**So that** I can learn and implement distributed streaming patterns effectively.

#### Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | E2E test: PUB/SUB broadcast with 3 subscribers | E2E test |
| AC2 | E2E test: PUSH/PULL load balancing with 3 workers | E2E test |
| AC3 | E2E test: REQ/REP request-response pattern | E2E test |
| AC4 | E2E test: TCP transport across processes | E2E test |
| AC5 | E2E test: Mixed Unix + ZeroMQ in same workflow | E2E test |
| AC6 | Performance test: 10K msg/s throughput | Performance test |
| AC7 | Documentation in `YAML_REFERENCE.md` | Manual review |
| AC8 | Example: `examples/yaml/zeromq_pubsub.yaml` | Manual review |
| AC9 | Example: `examples/yaml/zeromq_pipeline.yaml` | Manual review |
| AC10 | Example: `examples/yaml/zeromq_distributed.yaml` | Manual review |
| AC11 | Troubleshooting guide for ZeroMQ issues | Manual review |
| AC12 | Migration guide from Unix pipes | Manual review |

#### Documentation Updates

**New Documentation**:
- `docs/shared/yaml-reference/zeromq.md` - Full ZeroMQ reference
- Update `docs/shared/yaml-reference/streams.md` - Add transport selection section
- Update `docs/shared/YAML_REFERENCE.md` - Add ZeroMQ to TOC

**Examples**:
- `examples/yaml/zeromq_pubsub.yaml` - Event broadcasting
- `examples/yaml/zeromq_pipeline.yaml` - Load-balanced pipeline
- `examples/yaml/zeromq_distributed.yaml` - Multi-machine workflow
- `examples/yaml/zeromq_hybrid.yaml` - Mixed transports

---

## Feature Comparison

### Unix Pipes vs ZeroMQ

| Aspect | Unix Pipes | ZeroMQ |
|--------|-----------|--------|
| **Platform** | Linux, macOS | Linux, macOS, Windows |
| **Topology** | Local only | Local + Network |
| **Protocols** | OS pipes, FIFOs | IPC, TCP, inproc |
| **Patterns** | 1:1, 1:N (tee) | PUB/SUB, PUSH/PULL, REQ/REP |
| **Load balancing** | No | Yes (PUSH/PULL) |
| **Backpressure** | OS buffer | High-water mark |
| **Dependencies** | None | pyzmq |
| **Setup complexity** | Low | Medium |
| **Latency** | Lowest | Very low |
| **Throughput** | High | High |

### Pattern Selection Guide

| Use Case | Recommended Pattern | Why |
|----------|---------------------|-----|
| Log streaming | PUB/SUB | Fire-and-forget, all consumers get all logs |
| Work distribution | PUSH/PULL | Automatic load balancing, fair queuing |
| Parallel processing | PUSH/PULL | Each task processed by exactly one worker |
| Event broadcast | PUB/SUB | Multiple listeners, topic filtering |
| RPC/Confirmations | REQ/REP | Synchronous request-response |
| Status updates | PUB/SUB | No need to track consumers |

---

## Risk Mitigation

| ID | Risk | Impact | Likelihood | Mitigation |
|----|------|--------|------------|------------|
| R1 | **pyzmq installation issues** | High | Medium | Provide conda/pip install docs, test on CI |
| R2 | **ZeroMQ version incompatibility** | Medium | Low | Pin pyzmq version, document requirements |
| R3 | **TCP firewall blocking** | Medium | Medium | Document port requirements, provide IPC fallback |
| R4 | **Message ordering issues** | High | Low | Document pattern guarantees, provide examples |
| R5 | **Resource leaks on error** | High | Medium | Context managers, cleanup handlers |
| R6 | **Windows IPC path issues** | Medium | Medium | Use `ipc:///tmp/` on Unix, inproc on Windows |
| R7 | **High-water mark confusion** | Medium | High | Clear documentation, sensible defaults |
| R8 | **Discovery complexity** | Low | Medium | Start with static config, add discovery later |

---

## Dependencies

### Runtime Dependencies

| Dependency | Required | Purpose |
|------------|----------|---------|
| pyzmq >= 25.0 | Yes | ZeroMQ Python bindings |
| Python 3.9+ | Yes | Async features, type hints |

### Installation

```bash
# pip
pip install the_edge_agent[zeromq]

# conda
conda install pyzmq

# AppImage
# pyzmq bundled in AppImage builds
```

### Epic Dependencies

| Epic | Relationship | Status |
|------|--------------|--------|
| **TEA-STREAM-001** | Required (streams infrastructure) | Done |
| TEA-PARALLEL-001 | Optional (process strategy) | Ready |

---

## Compatibility Requirements

- [x] Existing workflows without ZeroMQ config work unchanged
- [x] Default `transport: unix` preserves current behavior
- [x] Unix pipe streams continue to work on Linux/macOS
- [x] All existing stream tests pass without modification
- [x] Optional dependency: ZeroMQ only loaded when `transport: zeromq`

---

## Definition of Done

- [ ] **Story 1**: Transport abstraction layer implemented
- [ ] **Story 2**: PUB/SUB pattern working on all platforms
- [ ] **Story 3**: PUSH/PULL pattern with load balancing
- [ ] **Story 4**: REQ/REP pattern for RPC-style communication
- [ ] **Story 5**: YAML integration complete
- [ ] **Story 6**: Documentation and examples published
- [ ] All ~109 test scenarios pass
- [ ] Windows CI green (GitHub Actions)
- [ ] pyzmq optional dependency configured correctly
- [ ] No regression in existing Unix pipe functionality
- [ ] Performance benchmarks documented

---

## Test Summary

| Story | Unit | Integration | E2E | Total |
|-------|------|-------------|-----|-------|
| TEA-STREAM-002.1 | 14 | 6 | 0 | 20 |
| TEA-STREAM-002.2 | 8 | 8 | 2 | 18 |
| TEA-STREAM-002.3 | 8 | 8 | 2 | 18 |
| TEA-STREAM-002.4 | 8 | 6 | 2 | 16 |
| TEA-STREAM-002.5 | 16 | 6 | 0 | 22 |
| TEA-STREAM-002.6 | 0 | 5 | 10 | 15 |
| **Total** | **54** | **39** | **16** | **109** |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial epic draft | Sarah (PO) |
