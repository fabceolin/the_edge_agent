# Test Design: Story TEA-BUILTIN-001.7.5

**Story:** Neo4j APOC Triggers Support
**Date:** 2024-12-30
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 47
- **Unit tests:** 23 (49%)
- **Integration tests:** 18 (38%)
- **E2E tests:** 6 (13%)
- **Priority distribution:** P0: 18, P1: 16, P2: 10, P3: 3

### Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| APOC library not installed | High | Medium | Graceful degradation tests |
| Trigger config disabled | High | Medium | Pre-flight checks |
| Transaction context corruption | Critical | Low | Isolation tests |
| Trigger cascade loops | Critical | Low | Guard/timeout tests |
| Cleanup failure on close | Medium | Medium | Lifecycle tests |
| Prefix collision | Medium | Low | Naming convention tests |

---

## Test Scenarios by Acceptance Criteria

### AC-1 to AC-4: APOC Detection

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-001 | Unit | P0 | `check_apoc_available()` returns True when APOC procedure exists | Pure function testing APOC detection logic |
| 1.7.5-UNIT-002 | Unit | P0 | `check_apoc_available()` returns False when APOC missing | Tests graceful degradation path |
| 1.7.5-UNIT-003 | Unit | P1 | `get_apoc_version()` parses version string correctly | Version parsing logic |
| 1.7.5-UNIT-004 | Unit | P1 | `get_apoc_version()` returns None when APOC unavailable | Error handling path |
| 1.7.5-UNIT-005 | Unit | P0 | `check_triggers_enabled()` returns True when config set | Configuration check logic |
| 1.7.5-UNIT-006 | Unit | P0 | `check_triggers_enabled()` returns False when config unset | Graceful degradation |
| 1.7.5-UNIT-007 | Unit | P1 | `APOC_AVAILABLE` property reflects detection result | Property accessor |
| 1.7.5-UNIT-008 | Unit | P1 | `TRIGGERS_ENABLED` property reflects config check | Property accessor |
| 1.7.5-INT-001 | Integration | P0 | Backend initializes with correct APOC detection against real Neo4j | Real database detection |
| 1.7.5-INT-002 | Integration | P1 | Backend reports correct APOC version from live Neo4j | Live version query |

---

### AC-5 to AC-6: Trigger Registration with Selectors

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-009 | Unit | P0 | `register_trigger()` builds correct Cypher for `createdNodes` selector | Cypher generation for most common selector |
| 1.7.5-UNIT-010 | Unit | P0 | `register_trigger()` builds correct Cypher for `deletedNodes` selector | Critical deletion tracking |
| 1.7.5-UNIT-011 | Unit | P1 | `register_trigger()` builds correct Cypher for `createdRelationships` | Relationship creation tracking |
| 1.7.5-UNIT-012 | Unit | P1 | `register_trigger()` builds correct Cypher for `deletedRelationships` | Relationship deletion tracking |
| 1.7.5-UNIT-013 | Unit | P2 | `register_trigger()` builds correct Cypher for `assignedLabels` | Label tracking |
| 1.7.5-UNIT-014 | Unit | P2 | `register_trigger()` builds correct Cypher for `removedLabels` | Label removal tracking |
| 1.7.5-UNIT-015 | Unit | P2 | `register_trigger()` builds correct Cypher for `assignedNodeProperties` | Property tracking |
| 1.7.5-UNIT-016 | Unit | P2 | `register_trigger()` builds correct Cypher for `assignedRelationshipProperties` | Relationship property tracking |
| 1.7.5-UNIT-017 | Unit | P0 | `register_trigger()` returns success response with trigger name | Return value validation |
| 1.7.5-UNIT-018 | Unit | P1 | `register_trigger()` validates trigger name uniqueness | Duplicate prevention |
| 1.7.5-UNIT-019 | Unit | P1 | `register_trigger()` rejects invalid selector type | Input validation |
| 1.7.5-INT-003 | Integration | P0 | Trigger registers successfully in Neo4j via APOC | Actual trigger creation |
| 1.7.5-INT-004 | Integration | P0 | Registered trigger fires on node creation | Trigger execution verification |
| 1.7.5-INT-005 | Integration | P1 | Multiple triggers can watch same selector | Trigger stacking |
| 1.7.5-INT-006 | Integration | P1 | Trigger with 'before' phase executes pre-commit | Phase timing |
| 1.7.5-INT-007 | Integration | P1 | Trigger with 'after' phase executes post-commit | Default phase |

---

### AC-7 to AC-10: Trigger Management Operations

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-020 | Unit | P0 | `unregister_trigger()` builds correct APOC remove call | Core CRUD operation |
| 1.7.5-UNIT-021 | Unit | P1 | `list_triggers()` parses APOC response correctly | List parsing |
| 1.7.5-UNIT-022 | Unit | P1 | `pause_trigger()` builds correct APOC pause call | Pause operation |
| 1.7.5-UNIT-023 | Unit | P1 | `resume_trigger()` builds correct APOC resume call | Resume operation |
| 1.7.5-INT-008 | Integration | P0 | Unregistered trigger no longer fires | Removal verification |
| 1.7.5-INT-009 | Integration | P1 | `list_triggers()` returns all registered triggers from Neo4j | Live listing |
| 1.7.5-INT-010 | Integration | P1 | Paused trigger does not fire | Pause verification |
| 1.7.5-INT-011 | Integration | P1 | Resumed trigger fires again | Resume verification |
| 1.7.5-INT-012 | Integration | P2 | Unregistering non-existent trigger returns graceful error | Error handling |
| 1.7.5-INT-013 | Integration | P2 | Pausing non-existent trigger returns graceful error | Error handling |

---

### AC-11 to AC-12: Trigger Execution and Context

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-INT-014 | Integration | P0 | Trigger receives `$createdNodes` list with correct data | Transaction context |
| 1.7.5-INT-015 | Integration | P0 | Trigger receives `$deletedNodes` list with correct data | Deletion context |
| 1.7.5-INT-016 | Integration | P1 | Trigger can create new nodes in response | Side effect capability |
| 1.7.5-INT-017 | Integration | P1 | Trigger can update existing nodes in response | Modification capability |
| 1.7.5-E2E-001 | E2E | P0 | Complete event chain: node create -> trigger -> log node created | Full reactive workflow |
| 1.7.5-E2E-002 | E2E | P1 | Trigger calling external procedure executes correctly | External call path |

---

### AC-13 to AC-14: Callback Mechanisms

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-INT-018 | Integration | P0 | HTTP callback trigger posts to specified URL | Webhook integration |
| 1.7.5-E2E-003 | E2E | P0 | Webhook callback delivers payload with triggered data | Full webhook flow |
| 1.7.5-E2E-004 | E2E | P1 | State update trigger writes to specified agent state key | Agent state integration |
| 1.7.5-E2E-005 | E2E | P2 | State update with transform function processes data correctly | Transform logic |

---

### AC-15 to AC-17: Lifecycle Management

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-024 | Unit | P0 | `cleanup_triggers()` filters by prefix correctly | Cleanup logic |
| 1.7.5-UNIT-025 | Unit | P1 | Trigger names include agent/session prefix | Naming convention |
| 1.7.5-INT-019 | Integration | P0 | `cleanup_triggers()` removes all matching triggers from Neo4j | Bulk cleanup |
| 1.7.5-INT-020 | Integration | P0 | Auto-cleanup on backend close removes session triggers | Session cleanup |
| 1.7.5-INT-021 | Integration | P1 | Cleanup does not affect triggers with different prefix | Isolation |
| 1.7.5-E2E-006 | E2E | P0 | Agent shutdown cleans up all registered triggers | Full lifecycle |

---

### AC-18: Configuration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-026 | Unit | P1 | YAML trigger settings parsed correctly | Config parsing |
| 1.7.5-UNIT-027 | Unit | P2 | Default trigger settings applied when not specified | Default handling |
| 1.7.5-UNIT-028 | Unit | P2 | `cleanup_on_close` setting respected | Config application |
| 1.7.5-UNIT-029 | Unit | P2 | `default_phase` setting applied to new triggers | Default phase |

---

### AC-19: Action Registration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-030 | Unit | P1 | `graph.register_trigger` action registered in action registry | Action availability |
| 1.7.5-UNIT-031 | Unit | P1 | `graph.unregister_trigger` action registered | Action availability |
| 1.7.5-UNIT-032 | Unit | P2 | `graph.list_triggers` action registered | Action availability |
| 1.7.5-UNIT-033 | Unit | P2 | `graph.pause_trigger` action registered | Action availability |
| 1.7.5-UNIT-034 | Unit | P2 | `graph.resume_trigger` action registered | Action availability |

---

### AC-20 to AC-22: Graceful Degradation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.5-UNIT-035 | Unit | P0 | Clear error message when APOC not available | User feedback |
| 1.7.5-UNIT-036 | Unit | P0 | Clear error message when triggers not enabled | User feedback |
| 1.7.5-UNIT-037 | Unit | P1 | Warning logged when trigger registration fails | Observability |
| 1.7.5-INT-022 | Integration | P0 | Backend operates without triggers when APOC unavailable | Graceful degradation |
| 1.7.5-INT-023 | Integration | P1 | Trigger operations fail gracefully with informative errors | Error quality |

---

## Risk Coverage Matrix

| Risk | Test IDs |
|------|----------|
| APOC library not installed | 1.7.5-UNIT-002, 1.7.5-UNIT-035, 1.7.5-INT-022 |
| Trigger config disabled | 1.7.5-UNIT-006, 1.7.5-UNIT-036, 1.7.5-INT-023 |
| Transaction context corruption | 1.7.5-INT-014, 1.7.5-INT-015 |
| Cleanup failure on close | 1.7.5-INT-019, 1.7.5-INT-020, 1.7.5-E2E-006 |
| Prefix collision | 1.7.5-UNIT-025, 1.7.5-INT-021 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on logic errors)
   - APOC detection (1.7.5-UNIT-001, 002, 005, 006)
   - Core registration (1.7.5-UNIT-009, 010, 017, 020, 024)
   - Graceful degradation (1.7.5-UNIT-035, 036)

2. **P0 Integration tests** (validate real Neo4j interaction)
   - APOC detection live (1.7.5-INT-001)
   - Trigger CRUD (1.7.5-INT-003, 004, 008)
   - Context validation (1.7.5-INT-014, 015)
   - Callback/webhook (1.7.5-INT-018)
   - Lifecycle (1.7.5-INT-019, 020)
   - Degradation (1.7.5-INT-022)

3. **P0 E2E tests** (critical paths)
   - Full reactive workflow (1.7.5-E2E-001)
   - Webhook flow (1.7.5-E2E-003)
   - Agent shutdown cleanup (1.7.5-E2E-006)

4. **P1 tests** in order (Unit -> Integration -> E2E)

5. **P2+ tests** as time permits

---

## Test Environment Requirements

### Unit Tests
- Mock Neo4j driver
- Mock APOC procedure responses
- No external dependencies

### Integration Tests
- Local Neo4j instance with APOC installed
- Docker container recommended: `neo4j:5.x` with APOC plugin
- Test isolation: unique trigger prefixes per test

### E2E Tests
- Full TEA agent runtime
- Neo4j with APOC and triggers enabled
- Mock webhook server (httpbin or similar)

### Neo4j Configuration for Tests

```properties
# neo4j.conf
apoc.trigger.enabled=true
apoc.trigger.refresh=1000  # Fast refresh for tests
```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 23
    integration: 18
    e2e: 6
  by_priority:
    p0: 18
    p1: 16
    p2: 10
    p3: 3
  coverage_gaps: []
  risks_mitigated:
    - APOC unavailable graceful degradation
    - Trigger config disabled handling
    - Transaction context integrity
    - Cleanup on session close
    - Prefix isolation
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.7.5-test-design-20251230.md
P0 tests identified: 18
Test file location: python/tests/test_neo4j_triggers.py
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for APOC interaction, E2E for workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (APOC detection and graceful degradation are P0)
- [x] Test IDs follow naming convention (1.7.5-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to specific tests
