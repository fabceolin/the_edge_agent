# Test Design: Story TEA-BUILTIN-005.3

Date: 2026-01-07
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 21
- Unit tests: 17 (81%)
- Integration tests: 4 (19%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 6, P1: 11, P2: 4

## Test Scenarios by Acceptance Criteria

### AC1: YAML `settings.opik` section supports declarative Opik configuration

**Risk Level:** High - configuration is foundation for all Opik features

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-001 | Unit | P0 | Parse complete YAML settings.opik section | Core configuration parsing - must work |
| 005.3-UNIT-002 | Unit | P1 | Parse partial YAML settings with defaults | Common use case - minimal config |
| 005.3-INT-001 | Integration | P1 | YAML settings applied to OpikExporter | Validates settings integration |

**Coverage Notes:**
- UNIT-001 validates all fields: `enabled`, `api_key`, `workspace`, `project_name`, `url`, `llm_tracing`, `trace_export`
- UNIT-002 tests that missing fields use sensible defaults
- INT-001 ensures settings flow through to actual Opik components

---

### AC2: Configuration precedence: constructor params > environment vars > YAML settings > defaults

**Risk Level:** Critical - wrong precedence causes misconfiguration in production

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-003 | Unit | P0 | Verify default configuration values | Baseline for precedence testing |
| 005.3-UNIT-004 | Unit | P0 | YAML settings override defaults | Second level of precedence |
| 005.3-UNIT-005 | Unit | P0 | Environment vars override YAML | Third level of precedence |
| 005.3-UNIT-006 | Unit | P0 | Constructor params override env vars | Highest precedence level |
| 005.3-UNIT-007 | Unit | P1 | Full precedence chain test | All levels active simultaneously |

**Coverage Notes:**
- Tests must use `unittest.mock.patch.dict(os.environ, {}, clear=True)` to ensure clean environment
- Each test validates specific precedence level in isolation
- UNIT-007 validates complete precedence chain with all levels active

**Critical Test Data:**

```python
# UNIT-003: Defaults
expected = {
    "enabled": False,
    "api_key": None,
    "workspace": None,
    "project_name": "the-edge-agent",
    "url": None,
    "llm_tracing": False,
    "trace_export": False,
}

# UNIT-004: YAML overrides
yaml_settings = {
    "opik": {
        "enabled": True,
        "project_name": "yaml-project",
    }
}
expected_project = "yaml-project"

# UNIT-005: Env overrides YAML
os.environ["OPIK_PROJECT_NAME"] = "env-project"
expected_project = "env-project"  # Not "yaml-project"

# UNIT-006: Constructor overrides env
engine = YAMLEngine(yaml_path, opik_config={"project_name": "constructor-project"})
expected_project = "constructor-project"  # Not "env-project"
```

---

### AC3: `opik.healthcheck` action validates connectivity and authentication

**Risk Level:** High - users rely on healthcheck for troubleshooting

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-008 | Unit | P1 | Healthcheck success returns valid response | Happy path validation |
| 005.3-UNIT-009 | Unit | P1 | Healthcheck with invalid API key | Common user error |
| 005.3-UNIT-010 | Unit | P1 | Healthcheck with network error | Common deployment issue |
| 005.3-INT-002 | Integration | P0 | opik.healthcheck registered in actions | Action must be callable from YAML |

**Coverage Notes:**
- UNIT-008 validates response structure: `{"success": bool, "latency_ms": float, "workspace": str, "project": str}`
- UNIT-009 and UNIT-010 validate error response structure includes helpful messages
- INT-002 confirms action registration in both `opik.healthcheck` and `healthcheck` namespaces

**Expected Response Structures:**

```python
# Success
{
    "success": True,
    "latency_ms": 123.45,
    "workspace": "my-workspace",
    "project": "the-edge-agent",
    "message": "Connected to Opik successfully"
}

# Error
{
    "success": False,
    "error": "Invalid API key",
    "message": "Invalid API key. Please verify your key at https://www.comet.com/opik/account"
}
```

---

### AC4: Project auto-creation when `OPIK_PROJECT_NAME` refers to non-existent project

**Risk Level:** Medium - Opik SDK handles this, just need logging

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-011 | Unit | P1 | Project created on first trace export | Validates auto-creation behavior |
| 005.3-UNIT-012 | Unit | P2 | Info log emitted on project creation | User awareness of auto-creation |

**Coverage Notes:**
- UNIT-011 validates Opik SDK's native project creation is not blocked
- UNIT-012 ensures users see info-level log: "Project 'X' created in Opik"

---

### AC5: Self-hosted Opik URL configurable via `OPIK_URL_OVERRIDE` or `settings.opik.url`

**Risk Level:** Medium - enterprise feature, less common but important

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-013 | Unit | P1 | OPIK_URL_OVERRIDE env var sets URL | Common self-hosted pattern |
| 005.3-UNIT-014 | Unit | P2 | settings.opik.url sets URL | YAML-based self-hosted config |
| 005.3-INT-003 | Integration | P1 | Self-hosted URL applied to Opik client | Validates URL propagation |

**Coverage Notes:**
- UNIT-013 tests environment variable path (most common for self-hosted)
- UNIT-014 tests YAML path (declarative configuration)
- INT-003 ensures URL reaches Opik SDK configuration

---

### AC6: Clear error messages guide users to correct configuration issues

**Risk Level:** High - poor error messages cause support burden

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-015 | Unit | P1 | Error message for missing SDK | Most common first-time user issue |
| 005.3-UNIT-016 | Unit | P1 | Error message for missing API key | Common Cloud configuration error |
| 005.3-UNIT-017 | Unit | P2 | Error message for invalid API key | Authentication troubleshooting |
| 005.3-UNIT-018 | Unit | P2 | Error message for network connectivity | Deployment troubleshooting |

**Coverage Notes:**
- All error messages must include actionable guidance (URLs, commands, next steps)
- Tests validate specific message content, not just error presence

**Required Error Message Content:**

| Error Condition | Must Include |
|-----------------|--------------|
| Missing SDK | "pip install opik" command |
| Missing API key | Link to https://www.comet.com/opik |
| Invalid API key | Link to account settings |
| Network error | URL that failed + "Check network connectivity" |

---

### AC7: Configuration examples for Cloud, self-hosted, and local development scenarios

**Risk Level:** Low - documentation quality, no code risk

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-DOC-001 | Manual | P2 | CLAUDE.md contains Cloud example | User onboarding documentation |
| 005.3-DOC-002 | Manual | P2 | CLAUDE.md contains self-hosted example | Enterprise documentation |
| 005.3-DOC-003 | Manual | P2 | CLAUDE.md contains local dev example | Developer documentation |

**Coverage Notes:**
- Manual review of CLAUDE.md documentation section
- Not included in automated test count

---

### AC8: All Opik features work with `opik` SDK as optional dependency

**Risk Level:** Critical - must not break engine when SDK missing

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-INT-004 | Integration | P0 | YAMLEngine initializes without opik SDK | Engine must not require optional dependency |
| 005.3-UNIT-019 | Unit | P1 | Healthcheck returns error when SDK missing | Graceful degradation |
| 005.3-UNIT-020 | Unit | P1 | OpikExporter disabled when SDK missing | Feature graceful degradation |

**Coverage Notes:**
- INT-004 is critical - engine must start without opik installed
- UNIT-019 validates healthcheck action returns helpful error
- UNIT-020 validates exporter initialization handles missing SDK

---

## Risk Coverage

### Configuration Risks

| Risk ID | Description | Mitigating Tests | Priority |
|---------|-------------|------------------|----------|
| RISK-001 | Wrong precedence causes production misconfiguration | 005.3-UNIT-003 to 007 | P0 |
| RISK-002 | Missing SDK breaks engine initialization | 005.3-INT-004 | P0 |
| RISK-003 | Users unable to troubleshoot connection issues | 005.3-UNIT-008 to 010, INT-002 | P1 |
| RISK-004 | Self-hosted configuration undiscoverable | 005.3-UNIT-013, 014, INT-003 | P1 |
| RISK-005 | Error messages unhelpful for debugging | 005.3-UNIT-015 to 018 | P1 |

### Test Gap Analysis

**Gaps Identified:**
1. No E2E test for actual Opik Cloud connectivity (acceptable - would require real API key in CI)
2. No test for concurrent healthcheck calls (low risk - action is stateless)
3. No test for YAML syntax errors in settings.opik (covered by general YAML validation)

**Gap Justification:**
- Real API E2E tests belong in separate smoke test suite with credentials
- Concurrent access not a concern for read-only healthcheck
- YAML validation is YAMLEngine responsibility, not Opik-specific

---

## Test Implementation Guidance

### Critical Test Setup Pattern

```python
import unittest
from unittest.mock import patch, MagicMock
import os

class TestOpikConfiguration(unittest.TestCase):
    def setUp(self):
        """Clean environment before each test."""
        # Clear all OPIK_ environment variables
        self.env_patcher = patch.dict(
            os.environ,
            {},
            clear=False  # Don't clear all env vars, just override
        )
        self.env_patcher.start()

        # Remove any existing OPIK_ vars
        for key in list(os.environ.keys()):
            if key.startswith("OPIK_"):
                os.environ.pop(key)

    def tearDown(self):
        """Restore environment."""
        self.env_patcher.stop()
```

### Mocking Opik SDK (for tests without SDK installed)

```python
# Option 1: Skip tests when SDK not available
try:
    import opik
    OPIK_AVAILABLE = True
except ImportError:
    OPIK_AVAILABLE = False

@unittest.skipUnless(OPIK_AVAILABLE, "opik SDK not installed")
def test_healthcheck_success(self):
    # Test code here
    pass

# Option 2: Mock sys.modules (allows testing without SDK)
@patch.dict('sys.modules', {'opik': MagicMock()})
def test_healthcheck_error_handling(self):
    # Test code here
    pass
```

### Healthcheck Response Validation

```python
def _validate_success_response(self, response):
    """Helper to validate healthcheck success response."""
    self.assertIn("success", response)
    self.assertIn("latency_ms", response)
    self.assertIn("workspace", response)
    self.assertIn("project", response)
    self.assertTrue(response["success"])
    self.assertIsInstance(response["latency_ms"], (int, float))
    self.assertGreater(response["latency_ms"], 0)

def _validate_error_response(self, response):
    """Helper to validate healthcheck error response."""
    self.assertIn("success", response)
    self.assertIn("error", response)
    self.assertIn("message", response)
    self.assertFalse(response["success"])
    self.assertTrue(len(response["message"]) > 0)
```

---

## Recommended Execution Order

1. **P0 Unit tests (fail fast on critical issues)**
   - 005.3-UNIT-003: Defaults baseline
   - 005.3-UNIT-004: YAML precedence
   - 005.3-UNIT-005: Env precedence
   - 005.3-UNIT-006: Constructor precedence
   - 005.3-UNIT-001: YAML parsing

2. **P0 Integration tests**
   - 005.3-INT-004: Engine works without SDK
   - 005.3-INT-002: Healthcheck action registered

3. **P1 Unit tests (core functionality)**
   - 005.3-UNIT-007: Full precedence chain
   - 005.3-UNIT-008: Healthcheck success
   - 005.3-UNIT-009: Healthcheck invalid key
   - 005.3-UNIT-010: Healthcheck network error
   - 005.3-UNIT-011: Project auto-creation
   - 005.3-UNIT-013: Self-hosted env var
   - 005.3-UNIT-015: Missing SDK error
   - 005.3-UNIT-016: Missing API key error
   - 005.3-UNIT-019: SDK missing graceful degradation
   - 005.3-UNIT-020: Exporter disabled without SDK

4. **P1 Integration tests**
   - 005.3-INT-001: Settings applied to exporter
   - 005.3-INT-003: Self-hosted URL propagation

5. **P2 tests (edge cases and polish)**
   - 005.3-UNIT-002: Partial YAML parsing
   - 005.3-UNIT-012: Project creation logging
   - 005.3-UNIT-014: Self-hosted YAML config
   - 005.3-UNIT-017: Invalid API key message
   - 005.3-UNIT-018: Network error message

6. **P2 Documentation review (manual)**
   - 005.3-DOC-001, 002, 003

---

## Quality Checklist

Before finalizing, verify:

- [x] Every AC has test coverage
- [x] Test levels are appropriate (prefer unit over integration)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (005.3-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Configuration precedence tested comprehensively (RISK-001)
- [x] Optional dependency handling tested (RISK-002)
- [x] Error messages validated for actionable content (RISK-005)

---

## Implementation Status Alignment

**Existing Test File:** `tests/test_opik_configuration.py`

The story indicates 24 tests already implemented. This test design recommends 21 scenarios (20 automated + 3 manual doc reviews). The existing implementation may have additional edge case tests beyond the core ACs.

**Test Mocking Issue (from story QA Results):**

The story notes 7 tests fail when `opik` SDK not installed due to `@patch('opik.Opik')` attempting to import the module. This test design recommends:

```python
# Instead of:
@patch('opik.Opik')  # Fails if opik not installed

# Use:
@unittest.skipUnless(OPIK_AVAILABLE, "opik SDK not installed")
# OR
@patch.dict('sys.modules', {'opik': MagicMock()})
```

This allows tests to pass in environments without the optional dependency while still validating behavior.

---

## Test Traceability Matrix

| AC | Requirement | Test IDs | P0 | P1 | P2 |
|----|-------------|----------|----|----|-----|
| AC1 | YAML settings support | 005.3-UNIT-001, 002, INT-001 | 1 | 2 | 0 |
| AC2 | Config precedence | 005.3-UNIT-003-007 | 4 | 1 | 0 |
| AC3 | opik.healthcheck action | 005.3-UNIT-008-010, INT-002 | 1 | 3 | 0 |
| AC4 | Project auto-creation | 005.3-UNIT-011, 012 | 0 | 1 | 1 |
| AC5 | Self-hosted URL config | 005.3-UNIT-013, 014, INT-003 | 0 | 2 | 1 |
| AC6 | Clear error messages | 005.3-UNIT-015-018 | 0 | 2 | 2 |
| AC7 | Documentation examples | 005.3-DOC-001-003 (manual) | 0 | 0 | 3 |
| AC8 | Optional dependency | 005.3-INT-004, UNIT-019, 020 | 1 | 2 | 0 |

**Total:** 6 P0, 13 P1, 7 P2 (including manual doc reviews)

---

## Key Principles Applied

- **Shift left**: 81% unit tests vs 19% integration tests
- **Risk-based**: P0 tests focus on config precedence (RISK-001) and optional dependency (RISK-002)
- **Efficient coverage**: No E2E tests needed - integration tests validate boundaries
- **Maintainability**: Tests isolated with clean environment setup/teardown
- **Fast feedback**: Unit tests run in milliseconds, fail fast on config issues
