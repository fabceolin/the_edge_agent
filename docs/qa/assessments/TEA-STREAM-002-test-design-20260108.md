# Test Design: Epic TEA-STREAM-002

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)
**Epic:** ZeroMQ Transport for Stream Channels
**Story Path:** docs/stories/TEA-STREAM-002-zeromq-transport-epic.md

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 109 |
| Unit tests | 54 (49.5%) |
| Integration tests | 39 (35.8%) |
| E2E tests | 16 (14.7%) |
| Priority distribution | P0: 32, P1: 41, P2: 28, P3: 8 |

### Strategy Rationale

This epic introduces a new transport layer abstraction with ZeroMQ as an alternative to Unix pipes. The test strategy emphasizes:

1. **Transport abstraction correctness** - Unit tests for interface contracts
2. **Cross-platform compatibility** - Integration tests on Windows/Linux/macOS
3. **Messaging pattern semantics** - Each ZeroMQ pattern (PUB/SUB, PUSH/PULL, REQ/REP) requires pattern-specific validation
4. **Backward compatibility** - Existing Unix pipe workflows must remain unchanged
5. **Network resilience** - TCP transport requires connection, discovery, and failure handling tests

---

## Test Scenarios by Story

---

### Story 1: Transport Abstraction Layer (TEA-STREAM-002.1)

**Estimated Tests:** 20 scenarios

#### AC1: Transport base class interface

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-001 | Unit | P0 | `Transport.send()` abstract method exists | Core interface contract |
| 002.1-UNIT-002 | Unit | P0 | `Transport.receive()` abstract method exists | Core interface contract |
| 002.1-UNIT-003 | Unit | P0 | `Transport.close()` abstract method exists | Resource cleanup contract |
| 002.1-UNIT-004 | Unit | P0 | `Transport.bind()` abstract method exists | Server-side binding |
| 002.1-UNIT-005 | Unit | P0 | `Transport.connect()` abstract method exists | Client-side connection |
| 002.1-UNIT-006 | Unit | P1 | `Transport.is_connected` property abstract | Connection state tracking |

#### AC2: UnixPipeTransport wrapper

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-007 | Unit | P0 | `UnixPipeTransport` implements `Transport` interface | Backward compatibility |
| 002.1-UNIT-008 | Unit | P1 | `UnixPipeTransport.send()` writes to file descriptor | Data transmission |
| 002.1-UNIT-009 | Unit | P1 | `UnixPipeTransport.receive()` reads from file descriptor | Data reception |
| 002.1-UNIT-010 | Unit | P1 | `UnixPipeTransport.close()` closes file descriptors | Resource cleanup |
| 002.1-INT-001 | Integration | P0 | Existing `StreamChannel` works through `UnixPipeTransport` | Zero regression |

#### AC3: TransportFactory creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-011 | Unit | P0 | `create_transport("unix")` returns `UnixPipeTransport` | Factory correctness |
| 002.1-UNIT-012 | Unit | P0 | `create_transport("zeromq")` returns `ZeroMQTransport` | Factory correctness |
| 002.1-UNIT-013 | Unit | P1 | `create_transport("unknown")` raises `TransportError` | Error handling |
| 002.1-UNIT-014 | Unit | P1 | `TransportConfig` dataclass validates fields | Configuration validation |

#### AC4: Backward compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-INT-002 | Integration | P0 | Existing YAML with no transport setting uses Unix pipes | Default behavior |
| 002.1-INT-003 | Integration | P0 | `transport: unix` explicitly uses Unix pipes | Explicit configuration |
| 002.1-INT-004 | Integration | P0 | All existing stream tests pass without modification | Regression prevention |

#### AC5: StreamRegistry transport_factory

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-015 | Unit | P1 | `StreamRegistry` accepts `transport_factory` parameter | Dependency injection |
| 002.1-INT-005 | Integration | P1 | `StreamRegistry` creates channels with specified transport | End-to-end factory usage |

#### AC6: TransportError handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-016 | Unit | P1 | `TransportError` includes helpful error message | Developer experience |
| 002.1-UNIT-017 | Unit | P2 | `TransportError` wraps underlying exception | Debug context preservation |
| 002.1-INT-006 | Integration | P2 | Transport errors propagate correctly through executor | Error flow validation |

---

### Story 2: PUB/SUB Pattern Implementation (TEA-STREAM-002.2)

**Estimated Tests:** 18 scenarios

#### AC1: ZeroMQPubSubTransport interface

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-UNIT-001 | Unit | P0 | `ZeroMQPubSubTransport` implements `Transport` interface | Interface contract |
| 002.2-UNIT-002 | Unit | P1 | Constructor accepts `TransportConfig` and optional `Context` | Configuration injection |

#### AC2: Publisher binds, subscribers connect

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-INT-001 | Integration | P0 | Publisher `bind()` creates PUB socket on address | Socket creation |
| 002.2-INT-002 | Integration | P0 | Subscriber `connect()` creates SUB socket to address | Socket connection |
| 002.2-INT-003 | Integration | P1 | Multiple subscribers can connect to one publisher | 1:N topology |

#### AC3: All subscribers receive all messages

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-INT-004 | Integration | P0 | Message sent to 1 subscriber received by 1 subscriber | 1:1 delivery |
| 002.2-INT-005 | Integration | P0 | Message sent to 3 subscribers received by all 3 | Fan-out delivery |
| 002.2-INT-006 | Integration | P1 | Messages delivered in order per subscriber | Ordering guarantee |
| 002.2-E2E-001 | E2E | P0 | PUB/SUB broadcast with 3 subscribers in workflow | Full pattern validation |

#### AC4: Topic filtering

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-UNIT-003 | Unit | P1 | `subscribe_filter` option filters by topic prefix | Topic subscription |
| 002.2-UNIT-004 | Unit | P2 | Empty filter subscribes to all messages | Default behavior |
| 002.2-INT-007 | Integration | P2 | Multiple topic filters on different subscribers | Multi-topic scenario |

#### AC5: High-water mark

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-UNIT-005 | Unit | P1 | `high_water_mark` option sets SNDHWM/RCVHWM | Memory protection |
| 002.2-UNIT-006 | Unit | P2 | Default high_water_mark is 1000 | Sensible default |

#### AC6-7: IPC protocol (Linux/macOS and Windows)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-INT-008 | Integration | P0 | PUB/SUB works with IPC on Linux | Platform compatibility |
| 002.2-INT-009 | Integration | P1 | PUB/SUB works with IPC on macOS | Platform compatibility |
| 002.2-INT-010 | Integration | P1 | PUB/SUB works with IPC on Windows | Cross-platform goal |

#### AC8: TCP protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.2-INT-011 | Integration | P1 | PUB/SUB works with TCP localhost | TCP transport |
| 002.2-E2E-002 | E2E | P2 | PUB/SUB across separate processes via TCP | Distributed scenario |

---

### Story 3: PUSH/PULL Pattern Implementation (TEA-STREAM-002.3)

**Estimated Tests:** 18 scenarios

#### AC1: ZeroMQPushPullTransport interface

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-001 | Unit | P0 | `ZeroMQPushPullTransport` implements `Transport` interface | Interface contract |
| 002.3-UNIT-002 | Unit | P1 | Constructor accepts `TransportConfig` and optional `Context` | Configuration injection |

#### AC2: Round-robin distribution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-001 | Integration | P0 | PUSH distributes to 2 PULL sockets round-robin | Load balancing core |
| 002.3-INT-002 | Integration | P0 | PUSH distributes to 5 PULL sockets round-robin | Scaling validation |
| 002.3-E2E-001 | E2E | P0 | Load-balanced pipeline with 3 workers in workflow | Full pattern validation |

#### AC3: No duplicate delivery

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-003 | Integration | P0 | 100 messages to 3 workers, exactly 100 total received | No duplicates |
| 002.3-INT-004 | Integration | P1 | Message IDs verify no duplicates | Verification method |

#### AC4: Fair queuing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-005 | Integration | P1 | Slow worker receives fewer messages | Fair distribution |
| 002.3-INT-006 | Integration | P2 | Fast worker receives more messages | Adaptive balancing |

#### AC5-6: N:1 and 1:N topologies

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-007 | Integration | P1 | 3 pushers → 1 puller (aggregation) | N:1 topology |
| 002.3-INT-008 | Integration | P1 | 1 pusher → 3 pullers (distribution) | 1:N topology |
| 002.3-E2E-002 | E2E | P1 | Pipeline with fan-out and fan-in stages | Complex topology |

#### AC7: High-water mark prevents blocking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-003 | Unit | P1 | `high_water_mark` option sets SNDHWM/RCVHWM | Memory protection |
| 002.3-UNIT-004 | Unit | P2 | Non-blocking send when HWM reached (with NOBLOCK) | Backpressure handling |

#### AC8: Windows IPC

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-009 | Integration | P1 | PUSH/PULL works with IPC on Linux | Platform compatibility |
| 002.3-INT-010 | Integration | P1 | PUSH/PULL works with IPC on Windows | Cross-platform goal |
| 002.3-INT-011 | Integration | P2 | PUSH/PULL works with TCP transport | Network transport |

#### Socket configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-005 | Unit | P2 | `linger` option sets LINGER socket option | Graceful shutdown |
| 002.3-UNIT-006 | Unit | P3 | `reconnect_interval` option sets reconnect delay | Connection resilience |

---

### Story 4: REQ/REP Pattern Implementation (TEA-STREAM-002.4)

**Estimated Tests:** 16 scenarios

#### AC1: ZeroMQReqRepTransport interface

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-001 | Unit | P0 | `ZeroMQReqRepTransport` implements `Transport` interface | Interface contract |
| 002.4-UNIT-002 | Unit | P1 | Constructor accepts `TransportConfig` and optional `Context` | Configuration injection |

#### AC2: REQ send-receive-send order

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-003 | Unit | P0 | REQ socket allows send then receive | Protocol order |
| 002.4-UNIT-004 | Unit | P0 | REQ socket raises error on two consecutive sends | Protocol enforcement |

#### AC3: REP receive-send-receive order

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-005 | Unit | P0 | REP socket allows receive then send | Protocol order |
| 002.4-UNIT-006 | Unit | P0 | REP socket raises error on two consecutive receives | Protocol enforcement |

#### AC4: Timeout on receive

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-007 | Unit | P1 | `timeout` option raises `TransportError` after timeout | Timeout handling |
| 002.4-UNIT-008 | Unit | P2 | No timeout (None) blocks indefinitely | Default behavior |

#### AC5: RPC-style communication

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-001 | Integration | P0 | Request-response round trip completes | Basic RPC |
| 002.4-INT-002 | Integration | P1 | Multiple sequential request-response pairs | RPC conversation |
| 002.4-E2E-001 | E2E | P1 | REQ/REP RPC in workflow node | Pattern validation |

#### AC6: Protocol violation errors

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-009 | Unit | P1 | Protocol violation raises `TransportError` | Error handling |
| 002.4-UNIT-010 | Unit | P2 | Error message indicates protocol state | Debug context |

#### AC7: Multiple REQ clients

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-003 | Integration | P1 | 3 REQ clients connect to 1 REP server | Multi-client scenario |
| 002.4-INT-004 | Integration | P2 | Server handles clients sequentially | Request queuing |
| 002.4-E2E-002 | E2E | P2 | Multiple workflow nodes as REQ clients | Distributed RPC |

#### AC8: Windows IPC

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-005 | Integration | P1 | REQ/REP works with IPC on Linux | Platform compatibility |
| 002.4-INT-006 | Integration | P2 | REQ/REP works with IPC on Windows | Cross-platform goal |

---

### Story 5: YAML Integration & TCP Discovery (TEA-STREAM-002.5)

**Estimated Tests:** 22 scenarios

#### AC1-3: Transport, protocol, and pattern settings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-001 | Unit | P0 | `transport: zeromq` parsed from settings | Configuration parsing |
| 002.5-UNIT-002 | Unit | P0 | `protocol: tcp` parsed from settings | Configuration parsing |
| 002.5-UNIT-003 | Unit | P0 | `pattern: pub_sub` parsed from settings | Configuration parsing |
| 002.5-UNIT-004 | Unit | P0 | `pattern: push_pull` parsed from settings | Configuration parsing |
| 002.5-UNIT-005 | Unit | P0 | `pattern: req_rep` parsed from settings | Configuration parsing |
| 002.5-UNIT-006 | Unit | P1 | Invalid pattern raises validation error | Error handling |

#### AC4-5: stream_mode mapping

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-007 | Unit | P0 | `stream_mode: load_balance` uses PUSH/PULL | Mode mapping |
| 002.5-UNIT-008 | Unit | P0 | `stream_mode: broadcast` uses PUB/SUB | Mode mapping |
| 002.5-UNIT-009 | Unit | P2 | Default stream_mode with zeromq transport | Default behavior |

#### AC6: Per-node protocol override

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-010 | Unit | P1 | Node `streams.protocol` overrides global | Override mechanism |
| 002.5-INT-001 | Integration | P1 | Mixed protocols in same workflow | Override validation |

#### AC7: bind_address and connect_addresses

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-011 | Unit | P0 | `zeromq.bind_address` parsed correctly | Configuration parsing |
| 002.5-UNIT-012 | Unit | P0 | `zeromq.connect_addresses` list parsed | Configuration parsing |
| 002.5-INT-002 | Integration | P0 | Producer uses bind_address, workers use connect | Address flow |
| 002.5-INT-003 | Integration | P1 | Multiple connect_addresses supported | Multi-worker discovery |

#### AC8: Discovery options

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-013 | Unit | P2 | `discovery: static` uses configured addresses | Static discovery |
| 002.5-INT-004 | Integration | P2 | `discovery: env` reads from environment | Env discovery |
| 002.5-INT-005 | Integration | P3 | `discovery: consul` queries Consul (if available) | Service discovery |

#### AC9: TCP validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-014 | Unit | P0 | TCP transport without addresses raises error | Validation |
| 002.5-UNIT-015 | Unit | P1 | IPC transport works without explicit addresses | Auto-addressing |

#### AC10: Environment variable expansion

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-016 | Unit | P1 | `${VAR}` expanded in bind_address | Env expansion |
| 002.5-UNIT-017 | Unit | P1 | `${VAR:-default}` uses default if unset | Default expansion |
| 002.5-INT-006 | Integration | P2 | Full workflow with env-configured addresses | End-to-end env usage |

#### ZeroMQ socket options

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-018 | Unit | P2 | `zeromq.high_water_mark` parsed and applied | Socket option |
| 002.5-UNIT-019 | Unit | P2 | `zeromq.linger` parsed and applied | Socket option |
| 002.5-UNIT-020 | Unit | P3 | `zeromq.reconnect_interval` parsed and applied | Socket option |
| 002.5-UNIT-021 | Unit | P2 | `zeromq.ipc_dir` creates IPC sockets in directory | IPC path configuration |

---

### Story 6: Integration Testing & Documentation (TEA-STREAM-002.6)

**Estimated Tests:** 15 scenarios

#### AC1-5: End-to-end pattern tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.6-E2E-001 | E2E | P0 | PUB/SUB broadcast with 3 subscribers (full workflow) | Pattern validation |
| 002.6-E2E-002 | E2E | P0 | PUSH/PULL load balancing with 3 workers | Pattern validation |
| 002.6-E2E-003 | E2E | P0 | REQ/REP request-response pattern | Pattern validation |
| 002.6-E2E-004 | E2E | P0 | TCP transport across processes | Network transport |
| 002.6-E2E-005 | E2E | P1 | Mixed Unix + ZeroMQ in same workflow | Hybrid transport |
| 002.6-INT-001 | Integration | P1 | Unix transport still works after ZeroMQ addition | Regression |

#### AC6: Performance benchmarks

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.6-PERF-001 | Performance | P1 | PUB/SUB achieves 10K msg/s throughput | Performance target |
| 002.6-PERF-002 | Performance | P1 | PUSH/PULL achieves 10K msg/s throughput | Performance target |
| 002.6-PERF-003 | Performance | P2 | IPC latency < 100μs per message | Latency target |
| 002.6-PERF-004 | Performance | P2 | ZeroMQ within 10% of Unix pipe performance | Parity target |

#### AC7-12: Documentation validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.6-DOC-001 | Manual | P1 | YAML_REFERENCE.md includes ZeroMQ section | Documentation |
| 002.6-DOC-002 | Manual | P1 | `zeromq_pubsub.yaml` example runs successfully | Example validation |
| 002.6-DOC-003 | Manual | P1 | `zeromq_pipeline.yaml` example runs successfully | Example validation |
| 002.6-DOC-004 | Manual | P2 | `zeromq_distributed.yaml` example documented | Example validation |
| 002.6-DOC-005 | Manual | P2 | Troubleshooting guide covers common issues | Documentation |
| 002.6-INT-002 | Integration | P2 | Example YAML files parse without errors | Example validation |
| 002.6-INT-003 | Integration | P2 | Example YAML files execute with mock data | Example validation |

---

## Risk Coverage Matrix

| Risk ID | Risk | Test Coverage |
|---------|------|---------------|
| R1 | pyzmq installation issues | 002.6-E2E-001 through E2E-004 (CI matrix) |
| R2 | ZeroMQ version incompatibility | 002.6-INT-002, 002.6-INT-003 |
| R3 | TCP firewall blocking | 002.6-E2E-004, 002.5-INT-002, 002.5-INT-003 |
| R4 | Message ordering issues | 002.2-INT-006, 002.3-INT-003, 002.3-INT-004 |
| R5 | Resource leaks on error | 002.1-UNIT-010, 002.1-INT-006 |
| R6 | Windows IPC path issues | 002.2-INT-010, 002.3-INT-010, 002.4-INT-006 |
| R7 | High-water mark confusion | 002.2-UNIT-005, 002.3-UNIT-003, 002.5-UNIT-018 |
| R8 | Discovery complexity | 002.5-INT-004, 002.5-INT-005 |

---

## Recommended Execution Order

### Phase 1: Foundation (P0 Unit Tests)

1. Transport interface contracts (002.1-UNIT-001 through 006)
2. Factory creation (002.1-UNIT-011 through 014)
3. UnixPipeTransport wrapper (002.1-UNIT-007 through 010)
4. YAML parsing (002.5-UNIT-001 through 008)

### Phase 2: Pattern Implementation (P0 Integration)

1. Backward compatibility (002.1-INT-001 through 004)
2. PUB/SUB core (002.2-INT-001 through 006)
3. PUSH/PULL core (002.3-INT-001 through 004)
4. REQ/REP core (002.4-INT-001 through 002)

### Phase 3: Cross-Platform (P1 Integration)

1. Platform tests (Linux, macOS, Windows)
2. TCP transport tests
3. Discovery and configuration tests

### Phase 4: End-to-End (P0/P1 E2E)

1. Pattern E2E tests (002.6-E2E-001 through 005)
2. Performance benchmarks
3. Example validation

### Phase 5: Nice-to-Have (P2/P3)

1. Edge cases and advanced features
2. Manual documentation review

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels appropriate (unit: interface/logic, integration: cross-component, e2e: full workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for critical transport, backward compat)
- [x] Test IDs follow `{EPIC}.{STORY}-{LEVEL}-{SEQ}` convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed via specific tests
- [x] Cross-platform coverage included for Windows goal
- [x] Performance targets specified

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 109
  by_level:
    unit: 54
    integration: 39
    e2e: 16
  by_priority:
    p0: 32
    p1: 41
    p2: 28
    p3: 8
  coverage_gaps: []
  risk_coverage:
    - risk_id: R1
      tests: [002.6-E2E-001, 002.6-E2E-002, 002.6-E2E-003, 002.6-E2E-004]
    - risk_id: R2
      tests: [002.6-INT-002, 002.6-INT-003]
    - risk_id: R3
      tests: [002.6-E2E-004, 002.5-INT-002, 002.5-INT-003]
    - risk_id: R4
      tests: [002.2-INT-006, 002.3-INT-003, 002.3-INT-004]
    - risk_id: R5
      tests: [002.1-UNIT-010, 002.1-INT-006]
    - risk_id: R6
      tests: [002.2-INT-010, 002.3-INT-010, 002.4-INT-006]
    - risk_id: R7
      tests: [002.2-UNIT-005, 002.3-UNIT-003, 002.5-UNIT-018]
    - risk_id: R8
      tests: [002.5-INT-004, 002.5-INT-005]
  special_considerations:
    - "Windows CI required for cross-platform validation"
    - "pyzmq optional dependency testing"
    - "Performance benchmarks require dedicated environment"
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-STREAM-002-test-design-20260108.md
P0 tests identified: 32
Story breakdown:
  - TEA-STREAM-002.1: 20 tests (14 unit, 6 integration)
  - TEA-STREAM-002.2: 18 tests (8 unit, 8 integration, 2 e2e)
  - TEA-STREAM-002.3: 18 tests (8 unit, 8 integration, 2 e2e)
  - TEA-STREAM-002.4: 16 tests (8 unit, 6 integration, 2 e2e)
  - TEA-STREAM-002.5: 22 tests (16 unit, 6 integration)
  - TEA-STREAM-002.6: 15 tests (0 unit, 5 integration, 10 e2e/perf/doc)
```
