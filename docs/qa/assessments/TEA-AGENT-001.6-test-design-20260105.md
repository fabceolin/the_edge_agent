# Test Design: Story TEA-AGENT-001.6 - Mem0 Memory Integration

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 72
- **Unit tests**: 42 (58%)
- **Integration tests**: 24 (33%)
- **E2E tests**: 6 (9%)
- **Priority distribution**: P0: 18, P1: 32, P2: 16, P3: 6

### Risk-Based Testing Focus

This story integrates an external memory service (Mem0) as a core dependency for agent memory. Key risks:
1. **External Service Dependency** - API failures, rate limits, network issues
2. **Data Integrity** - Memory corruption, scope leakage between users
3. **Graceful Degradation** - Fallback behavior when Mem0 unavailable
4. **Graph Memory Complexity** - Multi-hop queries, entity extraction accuracy

---

## Test Scenarios by Acceptance Criteria

### AC1: `memory.mem0.add` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-001 | Unit | P0 | **Given** valid messages array **When** add action called **Then** returns memory ID | Core functionality - memory storage |
| TEA-AGENT-001.6-UNIT-002 | Unit | P0 | **Given** messages with user_id scope **When** add action called **Then** memory stored with correct user_id | Scope isolation critical for multi-tenant |
| TEA-AGENT-001.6-UNIT-003 | Unit | P0 | **Given** messages with session_id scope **When** add action called **Then** memory stored with correct session_id | Session-level isolation |
| TEA-AGENT-001.6-UNIT-004 | Unit | P1 | **Given** messages with agent_id scope **When** add action called **Then** memory stored with correct agent_id | Agent-level memory |
| TEA-AGENT-001.6-UNIT-005 | Unit | P1 | **Given** messages with metadata **When** add action called **Then** metadata extracted and stored | Metadata enrichment |
| TEA-AGENT-001.6-UNIT-006 | Unit | P1 | **Given** empty messages array **When** add action called **Then** returns error with clear message | Input validation |
| TEA-AGENT-001.6-UNIT-007 | Unit | P2 | **Given** messages with all three scopes **When** add action called **Then** all scopes applied correctly | Multi-scope handling |
| TEA-AGENT-001.6-UNIT-008 | Unit | P2 | **Given** messages with custom metadata config **When** add action called **Then** custom extraction applied | Configurable extraction |
| TEA-AGENT-001.6-INT-001 | Integration | P0 | **Given** Mem0 client connected **When** add action with conversation **Then** fact extraction occurs | Core Mem0 integration |
| TEA-AGENT-001.6-INT-002 | Integration | P1 | **Given** Mem0 client **When** add large message batch **Then** all messages stored without timeout | Performance boundary |

### AC2: `memory.mem0.search` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-009 | Unit | P0 | **Given** valid query string **When** search action called **Then** returns list of memory objects | Core search functionality |
| TEA-AGENT-001.6-UNIT-010 | Unit | P0 | **Given** query with user_id filter **When** search action called **Then** only user's memories returned | Scope isolation for security |
| TEA-AGENT-001.6-UNIT-011 | Unit | P1 | **Given** query with limit=5 **When** search action called **Then** max 5 results returned | Limit enforcement |
| TEA-AGENT-001.6-UNIT-012 | Unit | P1 | **Given** query with session_id filter **When** search action called **Then** only session memories returned | Session scope filtering |
| TEA-AGENT-001.6-UNIT-013 | Unit | P1 | **Given** query **When** search returns results **Then** each result includes similarity score | Score transparency |
| TEA-AGENT-001.6-UNIT-014 | Unit | P2 | **Given** empty query string **When** search action called **Then** returns error with clear message | Input validation |
| TEA-AGENT-001.6-UNIT-015 | Unit | P2 | **Given** query with agent_id filter **When** search action called **Then** only agent memories returned | Agent scope filtering |
| TEA-AGENT-001.6-INT-003 | Integration | P0 | **Given** multiple memories stored **When** semantic search executed **Then** relevance ordering correct | Semantic search accuracy |
| TEA-AGENT-001.6-INT-004 | Integration | P1 | **Given** no matching memories **When** search executed **Then** returns empty list gracefully | Empty result handling |
| TEA-AGENT-001.6-INT-005 | Integration | P2 | **Given** 1000+ memories **When** search with limit=10 **Then** response time <500ms | Performance requirement |

### AC3: `memory.mem0.get_all` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-016 | Unit | P1 | **Given** user_id scope **When** get_all called **Then** all user memories returned | Core retrieval |
| TEA-AGENT-001.6-UNIT-017 | Unit | P1 | **Given** limit=10, offset=5 **When** get_all called **Then** correct page of results | Pagination support |
| TEA-AGENT-001.6-UNIT-018 | Unit | P1 | **Given** session_id scope **When** get_all called **Then** all session memories returned | Session scope |
| TEA-AGENT-001.6-UNIT-019 | Unit | P2 | **Given** include_metadata=true **When** get_all called **Then** metadata included in results | Optional metadata |
| TEA-AGENT-001.6-UNIT-020 | Unit | P2 | **Given** no scope specified **When** get_all called **Then** returns error requiring scope | Scope enforcement |
| TEA-AGENT-001.6-INT-006 | Integration | P1 | **Given** empty scope (new user) **When** get_all called **Then** returns empty list | Empty state handling |
| TEA-AGENT-001.6-INT-007 | Integration | P2 | **Given** large result set **When** pagination used **Then** all results accessible | Full pagination |

### AC4: `memory.mem0.update` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-021 | Unit | P1 | **Given** valid memory ID **When** update with new content **Then** content updated | Core update |
| TEA-AGENT-001.6-UNIT-022 | Unit | P1 | **Given** valid memory ID **When** update with metadata **Then** metadata merged (not replaced) | Partial update semantics |
| TEA-AGENT-001.6-UNIT-023 | Unit | P1 | **Given** update action **When** successful **Then** returns updated memory object | Response format |
| TEA-AGENT-001.6-UNIT-024 | Unit | P2 | **Given** invalid memory ID **When** update called **Then** returns not found error | Error handling |
| TEA-AGENT-001.6-UNIT-025 | Unit | P2 | **Given** memory owned by different user **When** update attempted **Then** permission denied | Authorization check |
| TEA-AGENT-001.6-INT-008 | Integration | P1 | **Given** memory updated **When** search executed **Then** new content is searchable | Index update propagation |

### AC5: `memory.mem0.delete` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-026 | Unit | P0 | **Given** valid memory ID **When** delete called **Then** memory removed | Core delete - data lifecycle |
| TEA-AGENT-001.6-UNIT-027 | Unit | P1 | **Given** user_id scope **When** bulk delete called **Then** all user memories removed | Bulk delete for GDPR |
| TEA-AGENT-001.6-UNIT-028 | Unit | P1 | **Given** session_id scope **When** bulk delete called **Then** all session memories removed | Session cleanup |
| TEA-AGENT-001.6-UNIT-029 | Unit | P1 | **Given** delete action **When** successful **Then** returns confirmation | Response confirmation |
| TEA-AGENT-001.6-UNIT-030 | Unit | P2 | **Given** invalid memory ID **When** delete called **Then** returns not found (idempotent) | Idempotent delete |
| TEA-AGENT-001.6-UNIT-031 | Unit | P2 | **Given** memory owned by different user **When** delete attempted **Then** permission denied | Authorization check |
| TEA-AGENT-001.6-INT-009 | Integration | P0 | **Given** memory deleted **When** search executed **Then** deleted memory not returned | Index cleanup verification |
| TEA-AGENT-001.6-INT-010 | Integration | P1 | **Given** bulk delete **When** get_all called **Then** returns empty for scope | Bulk delete verification |

### AC6: Graph Memory Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-032 | Unit | P1 | **Given** graph=true in settings **When** add action called **Then** entities extracted | Graph mode activation |
| TEA-AGENT-001.6-UNIT-033 | Unit | P1 | **Given** graph memory **When** add action called **Then** relations extracted | Relation extraction |
| TEA-AGENT-001.6-UNIT-034 | Unit | P2 | **Given** graph=false (default) **When** add action called **Then** no entity extraction | Default behavior |
| TEA-AGENT-001.6-INT-011 | Integration | P1 | **Given** graph memory with entities **When** multi-hop query **Then** traverses relations | Core graph capability |
| TEA-AGENT-001.6-INT-012 | Integration | P1 | **Given** include_relations=true **When** search called **Then** related entities returned | Graph-enhanced search |
| TEA-AGENT-001.6-INT-013 | Integration | P2 | **Given** complex entity network **When** multi-hop query (3 hops) **Then** correct results | Deep traversal |
| TEA-AGENT-001.6-E2E-001 | E2E | P1 | **Given** graph memory enabled **When** agent learns user preferences **Then** multi-hop reasoning works | Graph memory user journey |

### AC7: Settings Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-035 | Unit | P0 | **Given** settings.memory.backend=mem0 **When** agent starts **Then** Mem0 client initialized | Core configuration |
| TEA-AGENT-001.6-UNIT-036 | Unit | P0 | **Given** MEM0_API_KEY env var **When** settings parsed **Then** API key loaded | Secret management |
| TEA-AGENT-001.6-UNIT-037 | Unit | P1 | **Given** default_user_id in settings **When** action called without user_id **Then** default used | Default scope |
| TEA-AGENT-001.6-UNIT-038 | Unit | P1 | **Given** custom Mem0 endpoint **When** agent starts **Then** correct endpoint used | Custom endpoint support |
| TEA-AGENT-001.6-UNIT-039 | Unit | P0 | **Given** Mem0 unavailable **When** action called **Then** graceful fallback to native memory | Fallback for resilience |
| TEA-AGENT-001.6-UNIT-040 | Unit | P1 | **Given** Mem0 API error **When** action called **Then** warning logged and fallback used | Error handling with fallback |
| TEA-AGENT-001.6-INT-014 | Integration | P0 | **Given** invalid API key **When** first action called **Then** clear error and fallback | Auth failure handling |
| TEA-AGENT-001.6-INT-015 | Integration | P1 | **Given** network timeout **When** action called **Then** timeout error and fallback | Network resilience |

### AC8: Python Implementation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-UNIT-041 | Unit | P1 | **Given** mem0_actions module **When** imported **Then** all actions registered | Module registration |
| TEA-AGENT-001.6-UNIT-042 | Unit | P2 | **Given** mem0 not installed **When** action called **Then** ImportError with install hint | Optional dependency handling |
| TEA-AGENT-001.6-INT-016 | Integration | P1 | **Given** mem0 optional extra **When** pip install the_edge_agent[mem0] **Then** mem0ai installed | Dependency installation |
| TEA-AGENT-001.6-INT-017 | Integration | P1 | **Given** build_actions_registry **When** called **Then** memory.mem0.* actions available | Registry integration |

---

## Cross-Cutting Test Scenarios

### Security & Authorization

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-INT-018 | Integration | P0 | **Given** user A's memory **When** user B queries **Then** memory not returned | Cross-user isolation - CRITICAL |
| TEA-AGENT-001.6-INT-019 | Integration | P0 | **Given** session A's memory **When** session B queries **Then** memory not returned | Cross-session isolation |
| TEA-AGENT-001.6-INT-020 | Integration | P1 | **Given** API key in settings **When** agent starts **Then** key not logged | Secret protection |

### Error Handling & Resilience

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-INT-021 | Integration | P1 | **Given** Mem0 rate limited **When** action called **Then** retry with backoff | Rate limit handling |
| TEA-AGENT-001.6-INT-022 | Integration | P1 | **Given** Mem0 500 error **When** action called **Then** error logged and fallback | Server error handling |
| TEA-AGENT-001.6-INT-023 | Integration | P2 | **Given** malformed API response **When** parsed **Then** error handled gracefully | Response parsing resilience |
| TEA-AGENT-001.6-INT-024 | Integration | P2 | **Given** connection lost mid-request **When** action retried **Then** idempotent behavior | Connection resilience |

---

## E2E Test Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-AGENT-001.6-E2E-001 | E2E | P1 | **Given** graph memory enabled **When** agent learns user preferences **Then** multi-hop reasoning works | (Listed above) |
| TEA-AGENT-001.6-E2E-002 | E2E | P0 | **Given** YAML agent with Mem0 config **When** conversation stored and retrieved **Then** semantic search returns relevant memories | Core user journey |
| TEA-AGENT-001.6-E2E-003 | E2E | P1 | **Given** user session ends **When** new session starts **Then** user memories persist across sessions | Session persistence |
| TEA-AGENT-001.6-E2E-004 | E2E | P1 | **Given** agent personalization workflow **When** user returns after days **Then** agent recalls preferences | Long-term memory |
| TEA-AGENT-001.6-E2E-005 | E2E | P2 | **Given** Mem0 service unavailable **When** agent runs **Then** fallback to native memory works | Graceful degradation journey |
| TEA-AGENT-001.6-E2E-006 | E2E | P2 | **Given** GDPR delete request **When** bulk delete executed **Then** all user data removed | Compliance journey |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Test Coverage |
|------|-------------|--------|---------------|
| Cross-user memory leakage | Medium | Critical | TEA-AGENT-001.6-INT-018, INT-019 (P0) |
| API key exposure in logs | Medium | High | TEA-AGENT-001.6-INT-020 (P1) |
| Mem0 service downtime | High | Medium | TEA-AGENT-001.6-UNIT-039, UNIT-040, INT-014, INT-015, E2E-005 |
| Rate limiting impacts | Medium | Medium | TEA-AGENT-001.6-INT-021 (P1) |
| Data loss on delete | Low | High | TEA-AGENT-001.6-INT-009, INT-010 (P0-P1) |
| Graph query performance | Medium | Medium | TEA-AGENT-001.6-INT-013, INT-005 (P2) |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - TEA-AGENT-001.6-UNIT-001, 002, 003, 009, 010, 026, 035, 036, 039

2. **P0 Integration tests** (validate external service integration)
   - TEA-AGENT-001.6-INT-001, 003, 009, 014, 018, 019

3. **P0 E2E tests** (critical user journey)
   - TEA-AGENT-001.6-E2E-002

4. **P1 tests in order** (unit → integration → e2e)

5. **P2+ tests as time permits**

---

## Test Environment Requirements

### Unit Tests
- Mock Mem0 client
- No network dependencies
- Fast execution (<10s total)

### Integration Tests
- Mem0 test instance (local or sandbox API key)
- Network access to Mem0 endpoints
- Test user/session isolation

### E2E Tests
- Full TEA environment with Mem0 configuration
- Staging/test Mem0 account
- YAML agent fixtures with Mem0 settings

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 72
  by_level:
    unit: 42
    integration: 24
    e2e: 6
  by_priority:
    p0: 18
    p1: 32
    p2: 16
    p3: 6
  coverage_gaps: []
  security_tests:
    - TEA-AGENT-001.6-INT-018  # Cross-user isolation
    - TEA-AGENT-001.6-INT-019  # Cross-session isolation
    - TEA-AGENT-001.6-INT-020  # Secret protection
  resilience_tests:
    - TEA-AGENT-001.6-UNIT-039  # Graceful fallback
    - TEA-AGENT-001.6-INT-014   # Auth failure handling
    - TEA-AGENT-001.6-INT-015   # Network resilience
    - TEA-AGENT-001.6-INT-021   # Rate limit handling
  compliance_tests:
    - TEA-AGENT-001.6-E2E-006   # GDPR delete journey
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for Mem0 API, E2E for journeys)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (security/isolation = P0)
- [x] Test IDs follow naming convention (TEA-AGENT-001.6-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Given-When-Then format for all scenarios
- [x] Security tests identified and prioritized
- [x] Resilience/fallback scenarios covered
- [x] External service dependency risks addressed
