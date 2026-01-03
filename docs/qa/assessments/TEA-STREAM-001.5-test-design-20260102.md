# Test Design: Story TEA-STREAM-001.5

Date: 2026-01-02
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 0 (0%)
- Integration tests: 4 (22%)
- E2E tests: 8 (44%)
- Performance tests: 2 (11%)
- Manual validation: 4 (22%)
- Priority distribution: P0: 6, P1: 6, P2: 4, P3: 2

## Story Context

**Epic**: TEA-STREAM-001 - Unix Pipe Streaming with Hybrid State-Stream Model
**Story**: TEA-STREAM-001.5 - Integration Testing & Documentation
**Dependencies**: TEA-STREAM-001.1, 001.2, 001.3, 001.4

This story focuses on integration testing and documentation validation for the complete streaming workflow system. It validates that all prior stories (1-4) work together correctly.

---

## Test Scenarios by Acceptance Criteria

### AC1: E2E test: producer → consumer pipe streaming

**Requirement**: Validate basic producer to consumer streaming works end-to-end.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-E2E-001 | E2E | P0 | Producer streams 1000 JSON records to consumer via pipe | Critical path - validates core streaming functionality works end-to-end | R1 (Deadlock), R2 (Broken pipe) |
| 001.5-E2E-002 | E2E | P0 | Producer → Transformer → Consumer 3-node chain | Multi-hop validation ensures intermediate nodes correctly forward streams | R1 (Deadlock) |
| 001.5-INT-001 | Integration | P1 | State and stream coexist - producer returns count, streams data | Validates hybrid model where state and stream operate independently | - |

**Test Details:**

```yaml
001.5-E2E-001:
  description: "Basic producer → consumer streaming with 1000 records"
  preconditions:
    - Process parallel strategy enabled
    - Streams enabled in settings
  test_data:
    - 1000 JSON records {"id": N, "value": "item_N"}
  steps:
    - Load stream_pipeline.yaml example
    - Execute workflow with empty initial state
    - Verify consumer receives all 1000 records
    - Verify final state contains counts from all nodes
  expected_results:
    - No records lost in transit
    - final_state.records_generated == 1000
    - final_state.records_processed == 1000
  timeout: 30s

001.5-E2E-002:
  description: "Three-node streaming chain"
  preconditions:
    - Process parallel strategy enabled
    - Streams enabled
  test_data:
    - 500 records through generator → processor → aggregator
  steps:
    - Execute stream_pipeline.yaml
    - Verify each node receives correct input
    - Verify transformer correctly modifies records
    - Verify aggregator receives transformed records
  expected_results:
    - All transformations applied correctly
    - No data corruption between hops
    - State correctly passed alongside stream
  timeout: 30s

001.5-INT-001:
  description: "Hybrid state + stream model validation"
  preconditions:
    - Workflow with both streaming nodes and state-returning nodes
  steps:
    - Execute hybrid workflow
    - Verify streaming data flows via pipe
    - Verify state.count correctly reflects records produced
    - Verify final state merges all node outputs
  expected_results:
    - Stream and state operate independently
    - State correctly serialized alongside stream
    - No interference between data paths
  timeout: 30s
```

---

### AC2: E2E test: producer → broadcast → N consumers

**Requirement**: Validate stream broadcasting to multiple consumers.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-E2E-003 | E2E | P0 | Broadcast to 3 consumers - all receive same data | Core broadcast functionality using tee/FIFOs | R6 (FIFO limit) |
| 001.5-E2E-004 | E2E | P1 | Broadcast with slow consumer - no blocking | Validates backpressure handling | R1 (Deadlock) |
| 001.5-INT-002 | Integration | P2 | Fan-in aggregator receives all parallel results | Validates broadcast + fan-in pattern | - |

**Test Details:**

```yaml
001.5-E2E-003:
  description: "Broadcast stream to 3 consumers"
  preconditions:
    - stream_mode: broadcast on edge
    - 3 consumer nodes defined
  test_data:
    - 300 lines of text (100 iterations x 3 sentences)
  steps:
    - Load stream_broadcast.yaml example
    - Execute with empty initial state
    - Verify word_counter receives all lines
    - Verify line_counter receives all lines
    - Verify char_counter receives all lines
    - Verify aggregator merges all results
  expected_results:
    - All 3 consumers receive identical data
    - word_count, line_count, char_count all correct
    - No data loss or duplication
  timeout: 60s

001.5-E2E-004:
  description: "Broadcast resilience with slow consumer"
  preconditions:
    - One consumer artificially slowed (sleep per line)
  test_data:
    - 100 lines streamed
    - One consumer has 10ms delay per line
  steps:
    - Execute broadcast workflow
    - Verify producer not blocked by slow consumer
    - Verify all consumers eventually complete
    - Verify no deadlock occurs
  expected_results:
    - Workflow completes successfully
    - All consumers receive all data
    - Elapsed time reasonable (not multiplied by slow consumer)
  timeout: 120s

001.5-INT-002:
  description: "Fan-in aggregator receives parallel results"
  preconditions:
    - Broadcast to N consumers with fan_in node
  steps:
    - Execute broadcast workflow
    - Verify aggregator receives parallel_results list
    - Verify list contains N elements
    - Verify each result has correct structure
  expected_results:
    - len(parallel_results) == N
    - Each result contains expected keys
    - Aggregator can merge results correctly
  timeout: 30s
```

---

### AC3: E2E test: hybrid state + stream workflow

**Requirement**: Validate workflows combining state passing and streaming.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-E2E-005 | E2E | P0 | Hybrid workflow: checkpoint on non-stream node, resume | Critical - validates state model unchanged | R3 (No checkpoint) |
| 001.5-E2E-006 | E2E | P1 | Hybrid: metadata in state, bulk data in stream | Real-world pattern validation | - |
| 001.5-INT-003 | Integration | P1 | Non-streaming node can checkpoint, streaming node cannot | Compile-time validation | R3 |

**Test Details:**

```yaml
001.5-E2E-005:
  description: "Checkpoint on non-stream node, then stream"
  preconditions:
    - Checkpointer configured
    - interrupt_after on prepare node
  test_data:
    - stream_hybrid.yaml example
  steps:
    - Execute workflow until interrupt
    - Verify checkpoint saved (file_size, checksum in state)
    - Resume workflow from checkpoint
    - Verify streaming continues from file_reader
    - Verify final output correct
  expected_results:
    - Checkpoint contains prepare node's state
    - Resume skips prepare, starts at file_reader
    - Stream flows correctly after resume
    - Final state includes checksum from prepare
  timeout: 60s

001.5-E2E-006:
  description: "Metadata via state, bulk via stream pattern"
  preconditions:
    - Workflow with metadata node (state) and content node (stream)
  steps:
    - Execute stream_hybrid.yaml with input file
    - Verify metadata (size, checksum) captured in state
    - Verify file content streamed through transformer
    - Verify output file created correctly
  expected_results:
    - state.file_size correct
    - state.checksum matches input file
    - Output file contains transformed content
    - Line numbers correctly prepended
  timeout: 60s

001.5-INT-003:
  description: "Compile-time validation of stream + checkpoint incompatibility"
  preconditions:
    - YAML with interrupt_after on streaming node
  steps:
    - Attempt to compile invalid YAML
    - Verify error message raised
  expected_results:
    - CompilationError raised
    - Error message mentions "Stream nodes cannot be checkpointed"
    - Suggests moving checkpoint to non-streaming node
  timeout: 5s
```

---

### AC4: Performance test: 1GB stream throughput

**Requirement**: Validate streaming performance at scale.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-PERF-001 | Perf | P1 | 1GB throughput > 100MB/s | Performance baseline for large data | R5 (Memory exhaustion) |
| 001.5-PERF-002 | Perf | P2 | 10 consumers broadcast overhead < 20% | Validates broadcast scalability | R1 (Deadlock) |

**Test Details:**

```yaml
001.5-PERF-001:
  description: "1GB stream throughput benchmark"
  preconditions:
    - buffer_size: 131072 (128KB)
    - Single producer → single consumer
  test_data:
    - 1GB of line-delimited data (~10M lines)
  steps:
    - Start timer
    - Execute producer → consumer workflow
    - Stop timer when complete
    - Calculate throughput (bytes/second)
  expected_results:
    - Throughput > 100MB/s
    - No memory spike (line-by-line processing)
    - No timeout or deadlock
  timeout: 60s
  environment:
    cpu: "Document CI runner specs"
    memory: "Document available RAM"
    disk: "Document disk type (SSD recommended)"

001.5-PERF-002:
  description: "Broadcast to 10 consumers overhead measurement"
  preconditions:
    - buffer_size: 131072
    - 10 identical consumer nodes
  test_data:
    - 100MB of data
  steps:
    - Baseline: measure single consumer throughput
    - Broadcast: measure 10 consumer throughput
    - Calculate overhead percentage
  expected_results:
    - Overhead < 20% compared to single consumer
    - All consumers receive complete data
    - No deadlock with 10 FIFOs
  timeout: 120s
```

---

### AC5: Documentation in `YAML_REFERENCE.md`

**Requirement**: Validate documentation is complete and accurate.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-DOC-001 | Manual | P0 | YAML_REFERENCE.md Stream Channels section complete | Documentation accuracy | - |
| 001.5-DOC-002 | Manual | P1 | Troubleshooting table covers all common errors | User experience | R2, R4 |

**Test Details:**

```yaml
001.5-DOC-001:
  description: "YAML_REFERENCE.md documentation validation"
  type: manual_review
  reviewer: Quinn
  checklist:
    - [ ] Stream Channels section exists
    - [ ] Overview explains hybrid model
    - [ ] Enabling Streams subsection with YAML example
    - [ ] Node Configuration (producer/consumer) documented
    - [ ] Broadcasting subsection with example
    - [ ] Limitations section (no checkpoint, process only, Unix only)
    - [ ] Version badge (0.9.0+)
    - [ ] Platform requirements (Linux/macOS)

001.5-DOC-002:
  description: "Troubleshooting guide completeness"
  type: manual_review
  reviewer: Quinn
  checklist:
    - [ ] Error: "Streams require parallel_strategy: process"
    - [ ] Error: "Stream 'X' has no producer"
    - [ ] Error: "Stream nodes cannot be checkpointed"
    - [ ] Error: "Platform not supported" (Windows)
    - [ ] Each error has cause and fix columns
    - [ ] Fix instructions are actionable
```

---

### AC6-AC7: Example files

**Requirement**: Validate example YAML files are functional.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-DOC-003 | Manual | P0 | stream_pipeline.yaml executes successfully | Example correctness | - |
| 001.5-DOC-004 | Manual | P0 | stream_broadcast.yaml executes successfully | Example correctness | - |
| 001.5-INT-004 | Integration | P1 | Example files import test in pytest | Automated example validation | - |

**Test Details:**

```yaml
001.5-DOC-003:
  description: "Validate stream_pipeline.yaml example"
  type: manual_execution
  location: examples/yaml/stream_pipeline.yaml
  steps:
    - tea run examples/yaml/stream_pipeline.yaml
    - Verify output shows records_generated, records_processed
  expected_results:
    - No errors during execution
    - Output shows expected counts
    - Demonstrates producer → transformer → aggregator pattern

001.5-DOC-004:
  description: "Validate stream_broadcast.yaml example"
  type: manual_execution
  location: examples/yaml/stream_broadcast.yaml
  steps:
    - tea run examples/yaml/stream_broadcast.yaml
    - Verify word_count, line_count, char_count in output
  expected_results:
    - No errors during execution
    - All three counters have non-zero values
    - Demonstrates broadcast pattern

001.5-INT-004:
  description: "Automated example validation test"
  type: integration
  location: python/tests/test_examples_streaming.py
  steps:
    - Import each streaming example YAML
    - Compile and execute with minimal state
    - Assert no exceptions raised
    - Assert final state contains expected keys
  expected_results:
    - All streaming examples execute without error
    - Final states match expected schema
  timeout: 60s
```

---

### AC8: Troubleshooting guide for common stream errors

**Requirement**: Validate troubleshooting guidance exists.

| ID | Level | Priority | Test | Justification | Mitigates Risks |
|----|-------|----------|------|---------------|-----------------|
| 001.5-E2E-007 | E2E | P2 | Timeout kills stuck producer | Error handling validation | R1 (Deadlock) |
| 001.5-E2E-008 | E2E | P2 | Large data (100MB) without deadlock | Stress test | R1, R5 |

**Test Details:**

```yaml
001.5-E2E-007:
  description: "Stream timeout terminates stuck producer"
  preconditions:
    - settings.parallel.streams.timeout: 5
    - Producer with infinite loop
  steps:
    - Execute workflow with stuck producer
    - Wait for timeout (5s + grace period)
    - Verify workflow terminates with error
    - Verify error message mentions timeout
  expected_results:
    - Workflow does not hang indefinitely
    - TimeoutError or equivalent raised
    - Error message actionable ("increase timeout or fix producer")
  timeout: 30s

001.5-E2E-008:
  description: "100MB stream without deadlock"
  preconditions:
    - buffer_size: 65536 (default)
    - Producer generates 100MB data
  test_data:
    - 100MB of JSON lines
  steps:
    - Execute producer → consumer workflow
    - Monitor for deadlock (no progress for 30s)
    - Verify workflow completes successfully
  expected_results:
    - No deadlock occurs
    - All data transferred correctly
    - Memory usage stays reasonable (< 200MB peak)
  timeout: 120s
```

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Test Coverage |
|---------|------------------|---------------|
| R1 | Deadlock on full pipe buffer | 001.5-E2E-004, 001.5-E2E-008, 001.5-PERF-002 |
| R2 | Broken pipe crashes workflow | 001.5-E2E-001, 001.5-DOC-002 (troubleshooting) |
| R3 | No checkpoint/resume for streams | 001.5-E2E-005, 001.5-INT-003 |
| R4 | Windows incompatibility | 001.5-DOC-002 (troubleshooting) |
| R5 | Memory exhaustion | 001.5-PERF-001 |
| R6 | FIFO limit exceeded | 001.5-E2E-003 |

---

## Recommended Execution Order

### Phase 1: P0 Critical (Must Pass Before Merge)
1. 001.5-E2E-001 - Basic producer → consumer
2. 001.5-E2E-002 - Three-node chain
3. 001.5-E2E-003 - Broadcast to 3 consumers
4. 001.5-E2E-005 - Checkpoint + stream hybrid
5. 001.5-DOC-001 - YAML_REFERENCE validation
6. 001.5-DOC-003 - stream_pipeline.yaml works

### Phase 2: P1 High Priority
7. 001.5-INT-001 - Hybrid state + stream
8. 001.5-E2E-004 - Slow consumer resilience
9. 001.5-E2E-006 - Metadata pattern
10. 001.5-INT-003 - Compile-time validation
11. 001.5-PERF-001 - 1GB throughput
12. 001.5-DOC-004 - stream_broadcast.yaml works

### Phase 3: P2 Medium Priority
13. 001.5-INT-002 - Fan-in aggregator
14. 001.5-INT-004 - Automated example tests
15. 001.5-PERF-002 - Broadcast overhead
16. 001.5-E2E-007 - Timeout handling
17. 001.5-E2E-008 - 100MB stress test
18. 001.5-DOC-002 - Troubleshooting completeness

---

## Test Environment Requirements

### CI Environment
- **OS**: Linux (Ubuntu 22.04+), macOS (Ventura+)
- **Python**: 3.9+
- **Commands**: `tee`, `mkfifo` available
- **Disk**: 2GB free for temporary files
- **RAM**: 512MB minimum

### Local Development
- Same as CI
- Windows testing explicitly excluded (feature not supported)

### Performance Testing
- Document CPU model/cores
- Document RAM available
- Use SSD for 1GB throughput tests
- Note: Results are "typical" not "guaranteed"

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (E2E-heavy due to integration focus)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core functionality)
- [x] Test IDs follow naming convention (001.5-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed
- [x] Performance tests have defined thresholds

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 18
  by_level:
    unit: 0
    integration: 4
    e2e: 8
    performance: 2
    manual: 4
  by_priority:
    p0: 6
    p1: 6
    p2: 4
    p3: 2
  coverage_gaps: []
  notes:
    - Story focuses on integration/E2E due to nature (testing previous stories together)
    - No unit tests - all unit-level concerns covered in TEA-STREAM-001.1-001.4
    - Performance tests establish baselines for future regression detection
    - Manual validation required for documentation quality
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-STREAM-001.5-test-design-20260102.md
P0 tests identified: 6
Total scenarios: 18
Story file: docs/stories/TEA-STREAM-001.5-integration-documentation.md
Epic file: docs/stories/TEA-STREAM-001-unix-pipe-streaming-epic.md
```
