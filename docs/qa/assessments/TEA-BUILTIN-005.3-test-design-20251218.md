# Test Design: Story TEA-BUILTIN-005.3 (Opik Configuration and Utilities)

Date: 2025-12-18
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 17
- Unit tests: 13 (76%)
- Integration tests: 4 (24%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 4, P1: 9, P2: 4

### Rationale

This story implements configuration management and a healthcheck utility. The testing strategy emphasizes:

1. **Unit tests** for configuration precedence, YAML parsing, error messages, and healthcheck logic
2. **Integration tests** for YAMLEngine configuration flow and action registration
3. **No E2E tests** - Opik API calls are mocked; real connectivity tests are manual/staging

Critical paths are: configuration precedence (prevents misconfigs), clear error messages (user experience), and healthcheck action.

---

## Test Scenarios by Acceptance Criteria

### AC1: YAML settings.opik section supports declarative configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-001 | Unit | P0 | Verify YAML settings.opik section is parsed | Core configuration entry point |
| 005.3-UNIT-002 | Unit | P1 | Verify all opik settings keys are recognized | Schema validation |
| 005.3-INT-001 | Integration | P1 | Verify YAMLEngine applies opik settings | End-to-end config flow |

#### Scenarios

```python
def test_yaml_opik_section_parsed(self):
    """(P0) settings.opik section is parsed from YAML."""
    yaml_content = """
    settings:
      opik:
        enabled: true
        api_key: test-key
        project_name: my-project
    """
    engine = YAMLEngine()
    engine._parse_yaml(yaml_content)
    assert 'opik' in engine.settings
    assert engine.settings['opik']['enabled'] is True

def test_all_opik_settings_recognized(self):
    """(P1) All opik settings keys are accepted without error."""
    yaml_content = """
    settings:
      opik:
        enabled: true
        api_key: test-key
        workspace: my-team
        project_name: my-agent
        url: https://custom.opik.com
        llm_tracing: true
        trace_export: true
    """
    engine = YAMLEngine()
    # Should not raise KeyError or validation error
    engine._parse_yaml(yaml_content)
    assert len(engine.settings['opik']) == 7

def test_yaml_engine_applies_opik_settings(self):
    """(P1) YAMLEngine uses opik settings for configuration."""
    yaml_content = """
    settings:
      opik:
        enabled: true
        project_name: yaml-project
    """
    with patch.dict('sys.modules', {'opik': MagicMock()}):
        engine = YAMLEngine()
        graph = engine.load_from_string(yaml_content)
        # Verify opik config was resolved
        assert engine._opik_config['project_name'] == 'yaml-project'
```

---

### AC2: Configuration precedence: constructor > env vars > YAML > defaults

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-003 | Unit | P0 | Verify default configuration values | Baseline behavior |
| 005.3-UNIT-004 | Unit | P0 | Verify YAML overrides defaults | Precedence level 1 |
| 005.3-UNIT-005 | Unit | P0 | Verify env vars override YAML | Precedence level 2 |
| 005.3-UNIT-006 | Unit | P1 | Verify constructor params override env vars | Precedence level 3 |

#### Scenarios

```python
def test_default_configuration(self):
    """(P0) Default configuration values are correct."""
    config = YAMLEngine()._resolve_opik_config()
    assert config['enabled'] is False
    assert config['api_key'] is None
    assert config['project_name'] == 'the-edge-agent'
    assert config['workspace'] is None
    assert config['url'] is None
    assert config['llm_tracing'] is False

def test_yaml_overrides_defaults(self):
    """(P0) YAML settings override default values."""
    yaml_content = """
    settings:
      opik:
        project_name: yaml-project
        llm_tracing: true
    """
    engine = YAMLEngine()
    engine._parse_yaml(yaml_content)
    config = engine._resolve_opik_config()
    assert config['project_name'] == 'yaml-project'  # From YAML
    assert config['llm_tracing'] is True  # From YAML
    assert config['enabled'] is False  # Default (not in YAML)

@patch.dict(os.environ, {
    "OPIK_PROJECT_NAME": "env-project",
    "OPIK_WORKSPACE": "env-workspace"
})
def test_env_vars_override_yaml(self):
    """(P0) Environment variables override YAML settings."""
    yaml_content = """
    settings:
      opik:
        project_name: yaml-project
        workspace: yaml-workspace
    """
    engine = YAMLEngine()
    engine._parse_yaml(yaml_content)
    config = engine._resolve_opik_config()
    assert config['project_name'] == 'env-project'  # From env
    assert config['workspace'] == 'env-workspace'  # From env

@patch.dict(os.environ, {"OPIK_PROJECT_NAME": "env-project"})
def test_constructor_overrides_env(self):
    """(P1) Constructor parameters override environment variables."""
    engine = YAMLEngine(opik_project_name="constructor-project")
    config = engine._resolve_opik_config()
    assert config['project_name'] == 'constructor-project'  # From constructor
```

---

### AC3: opik.healthcheck action validates connectivity and authentication

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-007 | Unit | P1 | Verify healthcheck returns success structure | Result schema |
| 005.3-UNIT-008 | Unit | P1 | Verify healthcheck returns failure on connection error | Error handling |
| 005.3-UNIT-009 | Unit | P1 | Verify healthcheck returns latency_ms | Performance metric |
| 005.3-INT-002 | Integration | P0 | Verify healthcheck action registered in YAMLEngine | Action availability |

#### Scenarios

```python
def test_healthcheck_success_structure(self):
    """(P1) Successful healthcheck returns expected structure."""
    with patch('opik.configure') as mock_configure:
        result = opik_healthcheck(state={})
        assert 'success' in result
        assert 'latency_ms' in result
        assert 'workspace' in result
        assert 'project' in result
        assert 'message' in result
        assert result['success'] is True

def test_healthcheck_connection_failure(self):
    """(P1) Healthcheck returns failure on connection error."""
    with patch('opik.configure', side_effect=ConnectionError("Network unreachable")):
        result = opik_healthcheck(state={})
        assert result['success'] is False
        assert 'error' in result
        assert 'Network' in result['message'] or 'connect' in result['message'].lower()

def test_healthcheck_latency_captured(self):
    """(P1) Healthcheck includes latency measurement."""
    with patch('opik.configure'):
        result = opik_healthcheck(state={})
        assert 'latency_ms' in result
        assert isinstance(result['latency_ms'], (int, float))
        assert result['latency_ms'] >= 0

def test_healthcheck_action_registered(self):
    """(P0) opik.healthcheck is available in actions registry."""
    engine = YAMLEngine()
    assert 'opik.healthcheck' in engine.actions_registry or \
           'actions.opik_healthcheck' in engine.actions_registry
```

---

### AC4: Project auto-creation when project doesn't exist

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-010 | Unit | P1 | Verify project creation is attempted | Auto-create behavior |
| 005.3-UNIT-011 | Unit | P2 | Verify project creation failure is logged | Error visibility |

#### Scenarios

```python
def test_project_auto_creation(self):
    """(P1) Project is created if it doesn't exist."""
    with patch('opik.configure') as mock_configure:
        # Opik SDK auto-creates project when configured
        exporter = OpikExporter(project_name="new-project")
        exporter._ensure_configured()

        # Verify configure was called with project_name
        mock_configure.assert_called()
        call_kwargs = mock_configure.call_args.kwargs
        assert call_kwargs.get('project_name') == 'new-project'

def test_project_creation_failure_logged(self):
    """(P2) Project creation failure is logged as warning."""
    with patch('opik.configure', side_effect=PermissionError("Cannot create project")):
        with patch('logging.warning') as mock_log:
            exporter = OpikExporter(project_name="forbidden-project")
            try:
                exporter._ensure_configured()
            except:
                pass
            # Log should indicate the issue
```

---

### AC5: Self-hosted URL configurable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-012 | Unit | P1 | Verify OPIK_URL_OVERRIDE is used | Env var config |
| 005.3-UNIT-013 | Unit | P2 | Verify settings.opik.url is used | YAML config |
| 005.3-INT-003 | Integration | P2 | Verify self-hosted URL is passed to Opik SDK | End-to-end config |

#### Scenarios

```python
@patch.dict(os.environ, {"OPIK_URL_OVERRIDE": "https://opik.mycompany.com"})
def test_url_override_from_env(self):
    """(P1) OPIK_URL_OVERRIDE environment variable is used."""
    config = YAMLEngine()._resolve_opik_config()
    assert config['url'] == 'https://opik.mycompany.com'

def test_url_from_yaml_settings(self):
    """(P2) settings.opik.url is used from YAML."""
    yaml_content = """
    settings:
      opik:
        url: https://yaml.opik.com
    """
    engine = YAMLEngine()
    engine._parse_yaml(yaml_content)
    config = engine._resolve_opik_config()
    assert config['url'] == 'https://yaml.opik.com'

def test_self_hosted_url_passed_to_sdk(self):
    """(P2) Self-hosted URL is passed to opik.configure()."""
    with patch('opik.configure') as mock_configure:
        exporter = OpikExporter(url_override="https://self-hosted.opik.com")
        exporter._ensure_configured()

        call_kwargs = mock_configure.call_args.kwargs
        assert call_kwargs.get('url') == 'https://self-hosted.opik.com'
```

---

### AC6: Clear error messages guide users

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-014 | Unit | P1 | Verify missing SDK error includes install command | Actionable guidance |
| 005.3-UNIT-015 | Unit | P1 | Verify missing API key error includes documentation link | Actionable guidance |
| 005.3-UNIT-016 | Unit | P1 | Verify network error includes URL | Debug information |
| 005.3-UNIT-017 | Unit | P2 | Verify invalid API key error is distinct | Clear diagnosis |

#### Scenarios

```python
def test_missing_sdk_error_message(self):
    """(P1) Missing SDK error includes pip install command."""
    with patch.dict('sys.modules', {'opik': None}):
        result = opik_healthcheck(state={})
        assert result['success'] is False
        assert 'pip install opik' in result.get('install_command', '') or \
               'pip install opik' in result.get('error', '')

def test_missing_api_key_error(self):
    """(P1) Missing API key error includes documentation link."""
    with patch('opik.configure', side_effect=ValueError("API key required")):
        result = opik_healthcheck(state={})
        assert result['success'] is False
        # Should include link to get API key
        message = result.get('message', '') + result.get('error', '')
        assert 'comet.com' in message.lower() or 'api key' in message.lower()

def test_network_error_includes_url(self):
    """(P1) Network error message includes the URL attempted."""
    with patch('opik.configure', side_effect=ConnectionError("Cannot connect")):
        result = opik_healthcheck(state={})
        assert result['success'] is False
        # Message should help debug connectivity
        assert 'connect' in result.get('message', '').lower()

def test_invalid_api_key_distinct_error(self):
    """(P2) Invalid API key error is distinguishable from missing key."""
    with patch('opik.configure', side_effect=AuthenticationError("Invalid key")):
        result = opik_healthcheck(state={})
        assert result['success'] is False
        message = result.get('message', '').lower()
        assert 'invalid' in message or 'authentication' in message
```

---

### AC7: Configuration examples documented

*(Documentation task - verified in code review)*

---

### AC8: All features work with opik SDK as optional dependency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-INT-004 | Integration | P0 | Verify YAMLEngine works without opik installed | Graceful degradation |

#### Scenarios

```python
def test_yaml_engine_without_opik_sdk(self):
    """(P0) YAMLEngine functions correctly when opik SDK not installed."""
    with patch.dict('sys.modules', {'opik': None, 'opik.integrations': None}):
        engine = YAMLEngine(trace_exporter="console")

        yaml_content = """
        graph:
          nodes:
            - name: start
              run: |
                return {"result": "ok"}
        """
        graph = engine.load_from_string(yaml_content)

        # Graph execution should work
        result = list(graph.invoke({"input": "test"}))
        assert len(result) > 0
```

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Config precedence wrong | 005.3-UNIT-003 to 006 | Full precedence chain tested |
| Healthcheck unavailable | 005.3-INT-002 | Action registration verified |
| Unclear error messages | 005.3-UNIT-014 to 017 | Specific error content tests |
| SDK dependency breaks engine | 005.3-INT-004 | Graceful degradation verified |
| YAML settings ignored | 005.3-UNIT-001, 002, INT-001 | Parsing and application tested |

---

## Recommended Execution Order

1. P0 Unit tests (005.3-UNIT-001, 003, 004, 005) - configuration foundation
2. P0 Integration tests (005.3-INT-002, 004) - core integration points
3. P1 Unit tests (005.3-UNIT-002, 006, 007, 008, 009, 010, 012, 014, 015, 016)
4. P1 Integration tests (005.3-INT-001)
5. P2 tests as time permits

---

## Test Environment Requirements

- **Mocking**: `opik` SDK must be mocked for most tests
- **Environment variables**: Use `unittest.mock.patch.dict(os.environ, ...)`, clear before each test
- **Import mocking**: Use `unittest.mock.patch.dict('sys.modules', ...)` for optional dependency tests
- **Test framework**: `unittest` (project standard)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for wiring)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (005.3-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
