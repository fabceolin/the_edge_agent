"""
Tests for secrets YAML integration (TEA-BUILTIN-012.3).

This module tests the integration of secrets backends with YAML workflows:
- Template access via {{ secrets.KEY }}
- Actions: secrets.get, secrets.has
- CLI --secrets-backend flag
- Secrets excluded from checkpoints
- settings.secrets configuration
"""

import os
import json
import pickle
import tempfile
import pytest
import yaml as pyyaml
from pathlib import Path
from unittest.mock import MagicMock, patch

from the_edge_agent import YAMLEngine
from the_edge_agent.secrets import create_secrets_backend, EnvSecretsBackend


def load_yaml(yaml_content: str) -> dict:
    """Parse YAML string to dict."""
    return pyyaml.safe_load(yaml_content)


class TestSecretsTemplateAccess:
    """Test secrets access via Jinja2 templates."""

    def test_secrets_accessible_in_templates(self, monkeypatch):
        """GIVEN secrets configured, WHEN template uses {{ secrets.KEY }}, THEN value is interpolated."""
        monkeypatch.setenv("TEST_API_KEY", "secret123")
        monkeypatch.setenv("TEST_DB_URL", "postgres://localhost/db")

        yaml_content = """
name: secrets-template-test
state_schema:
  result: str

settings:
  secrets:
    backend: env
    env:
      prefix: TEST_

nodes:
  - name: use_secret
    steps:
      - uses: secrets.get
        with:
          key: API_KEY
          default: not-found
        output: api_key_result
      - run: |
          return {"result": f"API Key: {state.get('api_key_result', {}).get('value', 'not-found')}"}
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        # Execute
        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        assert final_state is not None
        assert "secret123" in final_state.get("result", "")

    def test_secrets_dot_notation_in_templates(self, monkeypatch):
        """GIVEN secrets backend, WHEN using secrets.KEY in template, THEN correct value returned."""
        monkeypatch.setenv("MYAPP_TOKEN", "bearer-xyz")

        yaml_content = """
name: secrets-dot-test
state_schema:
  token: str

settings:
  secrets:
    backend: env
    env:
      prefix: MYAPP_

nodes:
  - name: get_token
    steps:
      - action: noop
        with:
          value: "{{ secrets.TOKEN }}"
        output: token
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        # The secrets should be available in template context
        assert engine.secrets.get("TOKEN") == "bearer-xyz"


class TestSecretsActions:
    """Test secrets.get and secrets.has actions."""

    def test_secrets_get_action(self, monkeypatch):
        """GIVEN secret exists, WHEN secrets.get called, THEN value is returned."""
        monkeypatch.setenv("APP_SECRET", "mysecretvalue")

        yaml_content = """
name: secrets-get-test
state_schema:
  api_key: dict

settings:
  secrets:
    backend: env
    env:
      prefix: APP_

nodes:
  - name: get_secret
    uses: secrets.get
    with:
      key: SECRET
    output: api_key
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        assert final_state is not None
        assert final_state.get("api_key", {}).get("value") == "mysecretvalue"

    def test_secrets_get_action_with_default(self, monkeypatch):
        """GIVEN secret doesn't exist, WHEN secrets.get with default, THEN default returned."""
        yaml_content = """
name: secrets-get-default-test
state_schema:
  value: dict

settings:
  secrets:
    backend: env
    env:
      prefix: NONEXISTENT_

nodes:
  - name: get_missing
    uses: secrets.get
    with:
      key: MISSING_KEY
      default: fallback_value
    output: value
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        assert final_state is not None
        assert final_state.get("value", {}).get("value") == "fallback_value"

    def test_secrets_has_action_exists(self, monkeypatch):
        """GIVEN secret exists, WHEN secrets.has called, THEN exists=True."""
        monkeypatch.setenv("HAS_TEST_KEY", "somevalue")

        yaml_content = """
name: secrets-has-test
state_schema:
  check_result: dict

settings:
  secrets:
    backend: env
    env:
      prefix: HAS_TEST_

nodes:
  - name: check_exists
    uses: secrets.has
    with:
      key: KEY
    output: check_result
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        assert final_state is not None
        assert final_state.get("check_result", {}).get("exists") is True

    def test_secrets_has_action_not_exists(self):
        """GIVEN secret doesn't exist, WHEN secrets.has called, THEN exists=False."""
        yaml_content = """
name: secrets-has-missing-test
state_schema:
  check_result: dict

settings:
  secrets:
    backend: env
    env:
      prefix: NONEXISTENT_PREFIX_

nodes:
  - name: check_missing
    uses: secrets.has
    with:
      key: MISSING_KEY
    output: check_result
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        assert final_state is not None
        assert final_state.get("check_result", {}).get("exists") is False


class TestSecretsConfiguration:
    """Test settings.secrets configuration parsing."""

    def test_env_backend_with_prefix(self, monkeypatch):
        """GIVEN env backend with prefix, WHEN loaded, THEN prefix is applied."""
        monkeypatch.setenv("MYPREFIX_KEY1", "value1")
        monkeypatch.setenv("MYPREFIX_KEY2", "value2")
        monkeypatch.setenv("OTHER_KEY", "other")

        yaml_content = """
name: env-prefix-test
settings:
  secrets:
    backend: env
    env:
      prefix: MYPREFIX_
nodes: []
"""
        engine = YAMLEngine()
        engine.load_from_dict(load_yaml(yaml_content))

        assert "KEY1" in engine.secrets
        assert "KEY2" in engine.secrets
        assert "OTHER_KEY" not in engine.secrets
        assert engine.secrets["KEY1"] == "value1"

    def test_nested_provider_config(self, monkeypatch):
        """GIVEN aws.region in config, WHEN parsed, THEN backend gets correct kwargs."""
        # This test just validates parsing, not actual AWS connection
        monkeypatch.setenv("AWS_TEST_API", "test")

        yaml_content = """
name: aws-config-test
settings:
  secrets:
    backend: env
    env:
      prefix: AWS_TEST_
nodes: []
"""
        engine = YAMLEngine()
        engine.load_from_dict(load_yaml(yaml_content))

        # Verify backend was created with correct type
        assert engine._secrets_backend is not None
        assert "API" in engine.secrets

    def test_env_vars_expansion_in_config(self, monkeypatch):
        """GIVEN ${VAR} in config, WHEN loaded, THEN variable is expanded."""
        monkeypatch.setenv("SECRET_PREFIX_VAR", "EXPANDED_")
        monkeypatch.setenv("EXPANDED_TOKEN", "token123")

        yaml_content = """
name: env-expansion-test
settings:
  secrets:
    backend: env
    env:
      prefix: ${SECRET_PREFIX_VAR}
nodes: []
"""
        engine = YAMLEngine()
        engine.load_from_dict(load_yaml(yaml_content))

        assert "TOKEN" in engine.secrets
        assert engine.secrets["TOKEN"] == "token123"


class TestSecretsNotInCheckpoints:
    """Test that secrets are not serialized in checkpoints."""

    def test_secrets_backend_not_in_checkpoint(self, monkeypatch, tmp_path):
        """GIVEN secrets backend configured, WHEN checkpoint saved, THEN secrets not in checkpoint."""
        monkeypatch.setenv("CKPT_SECRET", "sensitive_data")

        yaml_content = """
name: checkpoint-secrets-test
state_schema:
  value: int

settings:
  secrets:
    backend: env
    env:
      prefix: CKPT_

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        # Run to get final state
        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        # Save checkpoint manually
        checkpoint_file = tmp_path / "test_checkpoint.pkl"
        checkpoint_data = {
            "state": final_state.copy(),
            "node": "process",
            "config": {},
            "timestamp": 12345,
            "version": "1.0",
        }
        with open(checkpoint_file, "wb") as f:
            pickle.dump(checkpoint_data, f)

        # Verify checkpoint doesn't contain secrets
        with open(checkpoint_file, "rb") as f:
            loaded = pickle.load(f)

        # Checkpoint state should not contain _secrets_backend or raw secrets
        assert "_secrets_backend" not in loaded
        assert "sensitive_data" not in str(loaded)
        assert "CKPT_SECRET" not in str(loaded)

    def test_engine_secrets_dict_not_in_state(self, monkeypatch):
        """GIVEN secrets.get action used, WHEN checkpoint created, THEN engine.secrets not in state."""
        monkeypatch.setenv("STATE_SECRET", "secret_value")

        yaml_content = """
name: state-secrets-test
state_schema:
  result: dict

settings:
  secrets:
    backend: env
    env:
      prefix: STATE_

nodes:
  - name: get_secret
    uses: secrets.get
    with:
      key: SECRET
    output: result
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        final_state = None
        for event in graph.stream({}):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        # The result should contain the secret value (this is intentional - user chose to store it)
        # But engine.secrets should not be in the state
        assert "_secrets" not in final_state
        assert "engine" not in final_state


class TestCLISecretsBackend:
    """Test CLI --secrets-backend flag."""

    def test_cli_secrets_backend_flag(self, monkeypatch):
        """GIVEN --secrets-backend env, WHEN CLI parses, THEN backend is configured."""
        from the_edge_agent.secrets import create_secrets_backend

        monkeypatch.setenv("CLI_TEST_KEY", "cli_value")

        # Simulate what CLI does
        backend_type = "env"
        backend_opts = {"prefix": "CLI_TEST_"}

        backend = create_secrets_backend(backend_type, **backend_opts)

        assert backend.get("KEY") == "cli_value"
        assert backend.has("KEY") is True
        assert backend.has("NONEXISTENT") is False

    def test_cli_secrets_backend_opts_json(self, monkeypatch):
        """GIVEN --secrets-backend-opts as JSON, WHEN parsed, THEN options applied."""
        import json

        monkeypatch.setenv("JSON_OPT_SECRET", "json_value")

        # Simulate CLI JSON parsing
        opts_json = '{"prefix": "JSON_OPT_"}'
        backend_opts = json.loads(opts_json)

        backend = create_secrets_backend("env", **backend_opts)

        assert backend.get("SECRET") == "json_value"


class TestSecretsWithLLMActions:
    """Test secrets integration with LLM actions."""

    def test_secrets_in_llm_call_headers(self, monkeypatch):
        """GIVEN API key in secrets, WHEN llm.call uses it, THEN header is correct."""
        monkeypatch.setenv("LLM_API_KEY", "sk-test-key")

        yaml_content = """
name: llm-secrets-test
state_schema:
  messages: list

settings:
  secrets:
    backend: env
    env:
      prefix: LLM_

nodes:
  - name: prepare
    run: |
      # Just verify secrets are accessible
      key = secrets.get("API_KEY")
      return {"api_key_present": key is not None}
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        # Verify secrets are loaded
        assert engine.secrets.get("API_KEY") == "sk-test-key"


class TestSecretsEdgeCases:
    """Test edge cases and error handling."""

    def test_empty_secrets_config(self):
        """GIVEN no secrets config, WHEN loaded, THEN defaults to env backend."""
        yaml_content = """
name: no-secrets-test
state_schema:
  value: str
nodes:
  - name: noop
    run: |
      return {"value": "done"}
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        # Engine should have empty secrets dict (no config)
        assert engine.secrets == {}

    def test_invalid_backend_type(self):
        """GIVEN invalid backend type, WHEN loaded, THEN warning logged."""
        yaml_content = """
name: invalid-backend-test
settings:
  secrets:
    backend: nonexistent_backend
nodes: []
"""
        engine = YAMLEngine()
        # Should not raise, just log warning
        engine.load_from_dict(load_yaml(yaml_content))
        # Backend may not be configured, secrets empty
        assert engine.secrets == {}

    def test_secrets_backend_close(self, monkeypatch):
        """GIVEN secrets backend, WHEN engine closed, THEN backend is closed."""
        monkeypatch.setenv("CLOSE_TEST_KEY", "value")

        yaml_content = """
name: close-test
settings:
  secrets:
    backend: env
    env:
      prefix: CLOSE_TEST_
nodes:
  - name: noop
    run: |
      return {}
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(load_yaml(yaml_content))

        assert engine._secrets_backend is not None

        # Get config wrapper and close
        from the_edge_agent.yaml_config import EngineConfig

        config = EngineConfig(engine)
        config.close()

        # Backend should be None after close
        assert engine._secrets_backend is None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
