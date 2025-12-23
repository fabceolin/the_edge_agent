# Test Design: Story TEA-BUILTIN-008.1

**Date**: 2024-12-22
**Designer**: Quinn (Test Architect)
**Story**: Core LlamaExtract Actions

## Test Strategy Overview

- **Total test scenarios**: 32
- **Unit tests**: 18 (56%)
- **Integration tests**: 12 (38%)
- **E2E tests**: 2 (6%)
- **Priority distribution**: P0: 14, P1: 12, P2: 6

## Test Scenarios by Acceptance Criteria

### AC1: llamaextract.extract - Extract structured data from documents

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-UNIT-001 | Unit | P0 | Extract with file URL input | Core input validation |
| 008.1-UNIT-002 | Unit | P0 | Extract with base64 content input | Alternative input path |
| 008.1-UNIT-003 | Unit | P0 | Extract with local file path input | File system integration |
| 008.1-UNIT-004 | Unit | P0 | Extract with inline JSON schema | Schema handling |
| 008.1-UNIT-005 | Unit | P1 | Extract with schema reference (uses:) | Deferred to Story 008.2 |
| 008.1-UNIT-006 | Unit | P0 | Extract mode BALANCED | Default mode validation |
| 008.1-UNIT-007 | Unit | P1 | Extract mode MULTIMODAL | Vision extraction path |
| 008.1-UNIT-008 | Unit | P0 | Extract mode PREMIUM | Premium tier validation |
| 008.1-UNIT-009 | Unit | P1 | Extract mode FAST | Speed-optimized path |
| 008.1-UNIT-010 | Unit | P1 | Extract returns data matching schema | Output structure |
| 008.1-INT-001 | Integration | P0 | Extract multi-page PDF document | Real document handling |
| 008.1-INT-002 | Integration | P1 | Extract image-based document | Multimodal pipeline |

### AC2: llamaextract.upload_agent - Create/update extraction agent

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-UNIT-011 | Unit | P0 | Create new agent (name doesn't exist) | Happy path creation |
| 008.1-UNIT-012 | Unit | P0 | Agent exists without force flag (skip) | Idempotency behavior |
| 008.1-UNIT-013 | Unit | P0 | Agent exists with force=true (update) | Force update path |
| 008.1-UNIT-014 | Unit | P1 | Upload with BALANCED mode | Mode configuration |
| 008.1-UNIT-015 | Unit | P1 | Upload with PREMIUM mode | Premium mode config |
| 008.1-INT-003 | Integration | P0 | Upload returns agent ID and status | API response validation |

### AC3: llamaextract.list_agents - List available extraction agents

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-UNIT-016 | Unit | P1 | List all agents (no filter) | Basic listing |
| 008.1-UNIT-017 | Unit | P2 | List agents with name filter | Filter logic |
| 008.1-INT-004 | Integration | P1 | List returns agent names and IDs | Response parsing |

### AC4: llamaextract.get_agent - Get agent details

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-UNIT-018 | Unit | P1 | Get agent by ID | Primary lookup |
| 008.1-INT-005 | Integration | P1 | Get agent by name | Name-based lookup |
| 008.1-INT-006 | Integration | P1 | Returns full config and schema | Complete response |

### AC5: llamaextract.delete_agent - Delete extraction agent

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-INT-007 | Integration | P2 | Delete agent by ID | Deletion path |
| 008.1-INT-008 | Integration | P2 | Delete agent by name | Name-based deletion |
| 008.1-INT-009 | Integration | P2 | Delete non-existent agent (error) | Error handling |

### AC6: Environment variable authentication

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-UNIT-019 | Unit | P0 | Use LLAMAEXTRACT_API_KEY when set | Primary auth |
| 008.1-UNIT-020 | Unit | P0 | Fallback to LLAMAPARSE_API_KEY | Fallback auth |
| 008.1-UNIT-021 | Unit | P0 | Error when no API key set | Config validation |

### AC7: Retry logic with exponential backoff

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-INT-010 | Integration | P0 | Retry on 429 rate limit | Rate limit handling |
| 008.1-INT-011 | Integration | P1 | Retry on 500 server error | Transient failure |
| 008.1-INT-012 | Integration | P1 | Max retries exceeded | Retry exhaustion |

### AC8: Clear error messages

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-UNIT-022 | Unit | P0 | Configuration error message | Missing config |
| 008.1-UNIT-023 | Unit | P1 | Rate limit error message | User feedback |
| 008.1-UNIT-024 | Unit | P1 | API error message | Server errors |
| 008.1-UNIT-025 | Unit | P2 | Timeout error message | Network issues |

### E2E Validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.1-E2E-001 | E2E | P0 | Full extraction workflow (YAML agent) | Critical path |
| 008.1-E2E-002 | E2E | P1 | Agent CRUD lifecycle | Complete lifecycle |

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| API key exposure | UNIT-019,020,021 | Validate env var handling |
| Rate limiting | INT-010,011,012 | Test retry behavior |
| Data extraction accuracy | INT-001,002 | Mock API responses |
| Mode compatibility | UNIT-006,007,008,009 | Test all modes |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - UNIT-001 through UNIT-006, UNIT-008, UNIT-011-013, UNIT-019-022
2. **P0 Integration tests** (API interaction)
   - INT-001, INT-003, INT-010
3. **P0 E2E tests** (critical path)
   - E2E-001
4. **P1 tests in order**
5. **P2+ as time permits**

## Test Data Requirements

| Data | Description | Source |
|------|-------------|--------|
| Sample PDF | Multi-page invoice | fixtures/invoice.pdf |
| Sample Image | Scanned document | fixtures/scan.png |
| Test Schema | Invoice extraction | fixtures/invoice-schema.json |
| Mock API Key | Test authentication | env: TEST_API_KEY |

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
