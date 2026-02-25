# Risk Profile: Story TEA-STREAM-002.1

Date: 2026-02-01
Reviewer: Quinn (Test Architect)

## Executive Summary

- Total Risks Identified: 10
- Critical Risks: 0
- High Risks: 3
- Medium Risks: 2
- Low Risks: 5
- Risk Score: 60/100 (Moderate Risk)

## High Risks Requiring Attention

### 1. TECH-001: ABC Interface Lock-in

**Score: 6 (High)**
**Probability**: Medium - API design decisions made now will constrain future ZeroMQ story
**Impact**: High - Interface changes after dependents exist causes breaking changes
**Mitigation**:
- Design Transport ABC methods with ZeroMQ requirements in mind
- Review interface against `zmq.Socket` API before finalizing
- Consider extensibility points (flags parameter, options dict)
**Testing Focus**: Verify interface supports all planned transport types

### 2. OPS-001: Resource Leak (File Descriptor Exhaustion)

**Score: 6 (High)**
**Probability**: Medium - Easy to forget cleanup in error paths
**Impact**: High - System-wide file descriptor exhaustion affects all processes
**Mitigation**:
- Context manager tests (UNIT-022, 023, 024) are P0
- Idempotent `close()` implementation required
- Test double-close and close-while-pending scenarios
**Testing Focus**: UNIT-024 (exception during use), UNIT-026 (close pending operations)

### 3. TECH-003: Task vs AC Conflict

**Score: 6 (High)**
**Probability**: High - Task 2 subtask says "wrap StreamChannel", AC3 says "reimplement"
**Impact**: Medium - Implementer confusion, potential rework if wrong choice made
**Mitigation**:
- **AC3 takes precedence** - reimplement pipe logic independently
- Update Task 2 subtask language to align with AC3
- This is a documentation bug to fix before development
**Testing Focus**: Code review to verify no StreamChannel delegation

## Risk Distribution

### By Category

- Technical (TECH): 4 risks (3 high)
- Operational (OPS): 2 risks (1 high)
- Performance (PERF): 1 risk (0 high)
- Data (DATA): 1 risk (0 high)
- Business (BUS): 1 risk (0 high)
- Security (SEC): 1 risk (0 high)

### By Component

- Transport Base Module: 3 risks (TECH-001, TECH-004, SEC-001)
- UnixPipeTransport: 4 risks (TECH-002, OPS-001, PERF-001, DATA-001)
- TransportFactory: 1 risk (TECH-003)
- StreamRegistry Integration: 1 risk (BUS-001)
- CI/Build: 1 risk (OPS-002)

## Detailed Risk Register

| Risk ID | Category | Title | Probability | Impact | Score | Priority |
|---------|----------|-------|-------------|--------|-------|----------|
| TECH-001 | Technical | ABC Interface Lock-in | Medium (2) | High (3) | 6 | High |
| TECH-003 | Technical | Task/AC Conflict | High (3) | Medium (2) | 6 | High |
| OPS-001 | Operational | Resource Leak | Medium (2) | High (3) | 6 | High |
| TECH-002 | Technical | Platform Dependency | Medium (2) | Medium (2) | 4 | Medium |
| TECH-004 | Technical | Mutable Default Bug | Medium (2) | Medium (2) | 4 | Medium |
| BUS-001 | Business | Backward Compatibility | Low (1) | High (3) | 3 | Low |
| PERF-001 | Performance | Blocking I/O | Low (1) | Medium (2) | 2 | Low |
| DATA-001 | Data | State Corruption | Low (1) | Medium (2) | 2 | Low |
| OPS-002 | Operational | CI Environment | Low (1) | Medium (2) | 2 | Low |
| SEC-001 | Security | FD Exhaustion DoS | Low (1) | Medium (2) | 2 | Low |

## Risk-Based Testing Strategy

### Priority 1: High Risk Tests (Score 6)

| Risk | Test Scenarios | Test Type |
|------|----------------|-----------|
| TECH-001 | Verify ABC interface supports all required methods | Unit |
| TECH-001 | Test that ZeroMQ could implement same interface | Design Review |
| OPS-001 | Context manager cleanup (UNIT-022, 023, 024) | Unit |
| OPS-001 | Idempotent close (UNIT-026) | Unit |
| OPS-001 | Close while operation pending (INT-007) | Integration |
| TECH-003 | Code review: no StreamChannel delegation | Code Review |

### Priority 2: Medium Risk Tests (Score 4)

| Risk | Test Scenarios | Test Type |
|------|----------------|-----------|
| TECH-002 | Document Windows incompatibility | Documentation |
| TECH-004 | Options dict isolation (UNIT-006) | Unit |
| TECH-004 | Multiple TransportConfig instances don't share state | Unit |

### Priority 3: Low Risk Tests (Score 2-3)

| Risk | Test Scenarios | Test Type |
|------|----------------|-----------|
| BUS-001 | Backward compatibility (INT-003, 004, 005) | Integration |
| PERF-001 | BrokenPipeError handling (UNIT-025) | Unit |
| DATA-001 | is_connected state accuracy (UNIT-019, 020, 021) | Unit |
| OPS-002 | Verify CI supports os.pipe() | CI Config |
| SEC-001 | Consider adding fd limit check | Future Enhancement |

## Risk Acceptance Criteria

### Must Fix Before Production

- TECH-003: Resolve Task/AC conflict before development starts
- OPS-001: All context manager tests must pass
- BUS-001: All backward compatibility tests must pass

### Can Deploy with Mitigation

- TECH-001: Document API stability expectations; plan review before ZeroMQ story
- TECH-002: Document Windows limitation in Transport ABC docstring
- TECH-004: Code review to verify default_factory usage

### Accepted Risks

- PERF-001: Blocking I/O acceptable for MVP; async transport is future scope
- SEC-001: FD exhaustion unlikely in normal usage; add limit in future if needed
- OPS-002: CI environment risk is low; most CI supports basic POSIX

## Monitoring Requirements

Post-deployment monitoring not applicable for infrastructure code. However:
- Add debug logging for fd open/close operations
- Consider adding fd count metric for observability

## Risk Review Triggers

Review and update risk profile when:
- ZeroMQ transport story begins (re-evaluate TECH-001)
- Windows support requested (re-evaluate TECH-002)
- Resource leak reported in production (re-evaluate OPS-001)
- Performance issues reported (re-evaluate PERF-001)

## Gate Recommendation

**Gate Status: CONCERNS**

Rationale:
- No critical risks (score 9)
- Three high risks (score 6) require attention
- TECH-003 should be resolved before development starts

**Action Required**: Update Task 2 subtask to align with AC3 before assigning to developer.

---

Risk profile: docs/qa/assessments/TEA-STREAM-002.1-risk-20260201.md
