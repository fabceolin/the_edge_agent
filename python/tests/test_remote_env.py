"""
Tests for remote environment variable handling (TEA-PARALLEL-001.4).

This module tests the RemoteEnvHandler class and EnvVarsConfig dataclass
that provide secure environment variable transfer to remote hosts.

Test Coverage:
- AC1: Environment variable transfer via env_vars.include whitelist
- AC2: Pattern exclusion via env_vars.exclude_patterns
- AC3: Transfer mode: ssh_env generates -o SendEnv flags
- AC4: Transfer mode: export_file generates sourced script
- AC5: Transfer mode: none skips env transfer

Test IDs:
- 001.4-UNIT-001: EnvVarsConfig validates fields
- 001.4-UNIT-002: Security - Only whitelisted vars returned
- 001.4-UNIT-003: Security - Exclude patterns filter
- 001.4-UNIT-004: SSH -o SendEnv options generated
- 001.4-UNIT-005: Export script generation
- 001.4-INT-001: ssh_env mode works end-to-end
- 001.4-INT-002: export_file mode transfers and sources
- 001.4-INT-003: none mode skips env transfer
- 001.4-INT-004: Missing env var handled gracefully
- 001.4-INT-005: YAML parsing for env_vars config
"""

import os
import tempfile
import pytest
from unittest.mock import patch, MagicMock

from the_edge_agent.remote_env import (
    EnvVarsConfig,
    RemoteEnvHandler,
    parse_env_vars_config,
)


class TestEnvVarsConfig:
    """Test suite for EnvVarsConfig dataclass (001.4-UNIT-001)."""

    def test_default_values(self):
        """Test that EnvVarsConfig has sensible defaults."""
        config = EnvVarsConfig()
        assert config.include == []
        assert config.exclude_patterns == []
        assert config.mode == "ssh_env"

    def test_valid_modes(self):
        """Test that all valid modes are accepted."""
        for mode in ("ssh_env", "export_file", "none"):
            config = EnvVarsConfig(mode=mode)
            assert config.mode == mode

    def test_invalid_mode_raises(self):
        """Test that invalid mode raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            EnvVarsConfig(mode="invalid")
        assert "Invalid mode" in str(exc_info.value)

    def test_include_must_be_list(self):
        """Test that include must be a list."""
        with pytest.raises(TypeError) as exc_info:
            EnvVarsConfig(include="not_a_list")
        assert "include must be a list" in str(exc_info.value)

    def test_exclude_patterns_must_be_list(self):
        """Test that exclude_patterns must be a list."""
        with pytest.raises(TypeError) as exc_info:
            EnvVarsConfig(exclude_patterns="not_a_list")
        assert "exclude_patterns must be a list" in str(exc_info.value)

    def test_full_config(self):
        """Test creating config with all options."""
        config = EnvVarsConfig(
            include=["OPENAI_API_KEY", "LOG_LEVEL"],
            exclude_patterns=["*_SECRET", "AWS_*"],
            mode="export_file",
        )
        assert config.include == ["OPENAI_API_KEY", "LOG_LEVEL"]
        assert config.exclude_patterns == ["*_SECRET", "AWS_*"]
        assert config.mode == "export_file"


class TestRemoteEnvHandlerFiltering:
    """Test suite for env var filtering (001.4-UNIT-002, 001.4-UNIT-003)."""

    def test_whitelist_only_returns_included_vars(self, monkeypatch):
        """SECURITY: Only variables in include list are returned."""
        # Set up environment
        monkeypatch.setenv("OPENAI_API_KEY", "sk-test-key")
        monkeypatch.setenv("LOG_LEVEL", "DEBUG")
        monkeypatch.setenv("SECRET_PASSWORD", "should-not-appear")
        monkeypatch.setenv("ANOTHER_VAR", "also-hidden")

        config = EnvVarsConfig(include=["OPENAI_API_KEY", "LOG_LEVEL"])
        handler = RemoteEnvHandler(config)

        result = handler.get_filtered_env_vars()

        assert "OPENAI_API_KEY" in result
        assert "LOG_LEVEL" in result
        assert "SECRET_PASSWORD" not in result
        assert "ANOTHER_VAR" not in result
        assert result["OPENAI_API_KEY"] == "sk-test-key"
        assert result["LOG_LEVEL"] == "DEBUG"

    def test_exclude_patterns_filter_after_include(self, monkeypatch):
        """SECURITY: Exclude patterns remove vars even if in include list."""
        monkeypatch.setenv("API_KEY", "key-value")
        monkeypatch.setenv("API_SECRET", "secret-value")
        monkeypatch.setenv("DB_PASSWORD", "db-pass")

        config = EnvVarsConfig(
            include=["API_KEY", "API_SECRET", "DB_PASSWORD"],
            exclude_patterns=["*_SECRET", "*_PASSWORD"],
        )
        handler = RemoteEnvHandler(config)

        result = handler.get_filtered_env_vars()

        assert "API_KEY" in result
        assert "API_SECRET" not in result  # Excluded by *_SECRET
        assert "DB_PASSWORD" not in result  # Excluded by *_PASSWORD

    def test_empty_include_returns_empty(self, monkeypatch):
        """Test that empty include list returns no vars."""
        monkeypatch.setenv("SOME_VAR", "value")

        config = EnvVarsConfig(include=[])
        handler = RemoteEnvHandler(config)

        result = handler.get_filtered_env_vars()
        assert result == {}

    def test_missing_var_gracefully_skipped(self, monkeypatch):
        """Test that missing env vars are skipped without error."""
        monkeypatch.setenv("EXISTING_VAR", "value")
        # NONEXISTENT_VAR is not set

        config = EnvVarsConfig(include=["EXISTING_VAR", "NONEXISTENT_VAR"])
        handler = RemoteEnvHandler(config)

        result = handler.get_filtered_env_vars()

        assert "EXISTING_VAR" in result
        assert "NONEXISTENT_VAR" not in result

    def test_empty_exclude_patterns_no_filtering(self, monkeypatch):
        """Test that empty exclude_patterns doesn't filter anything."""
        monkeypatch.setenv("VAR1", "value1")
        monkeypatch.setenv("VAR2_SECRET", "value2")

        config = EnvVarsConfig(
            include=["VAR1", "VAR2_SECRET"],
            exclude_patterns=[],  # Empty - no filtering
        )
        handler = RemoteEnvHandler(config)

        result = handler.get_filtered_env_vars()

        assert "VAR1" in result
        assert "VAR2_SECRET" in result  # Not excluded

    def test_aws_credentials_excluded_by_pattern(self, monkeypatch):
        """SECURITY: Cloud credentials can be excluded with patterns."""
        monkeypatch.setenv("AWS_ACCESS_KEY_ID", "AKIA...")
        monkeypatch.setenv("AWS_SECRET_ACCESS_KEY", "secret")
        monkeypatch.setenv("CUSTOM_API_KEY", "my-key")

        config = EnvVarsConfig(
            include=["AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "CUSTOM_API_KEY"],
            exclude_patterns=["AWS_*"],
        )
        handler = RemoteEnvHandler(config)

        result = handler.get_filtered_env_vars()

        assert "AWS_ACCESS_KEY_ID" not in result
        assert "AWS_SECRET_ACCESS_KEY" not in result
        assert "CUSTOM_API_KEY" in result


class TestSSHEnvMode:
    """Test suite for ssh_env mode (001.4-UNIT-004)."""

    def test_build_ssh_options_generates_send_env(self, monkeypatch):
        """Test SSH -o SendEnv options are generated correctly."""
        monkeypatch.setenv("API_KEY", "value1")
        monkeypatch.setenv("LOG_LEVEL", "DEBUG")

        config = EnvVarsConfig(
            include=["API_KEY", "LOG_LEVEL"],
            mode="ssh_env",
        )
        handler = RemoteEnvHandler(config)

        options = handler.build_ssh_options()

        assert "-o" in options
        assert "SendEnv=API_KEY" in options
        assert "SendEnv=LOG_LEVEL" in options
        # Verify format: ["-o", "SendEnv=VAR1", "-o", "SendEnv=VAR2"]
        assert options.count("-o") == 2

    def test_ssh_options_empty_for_other_modes(self, monkeypatch):
        """Test SSH options are empty for non-ssh_env modes."""
        monkeypatch.setenv("API_KEY", "value")

        for mode in ("export_file", "none"):
            config = EnvVarsConfig(include=["API_KEY"], mode=mode)
            handler = RemoteEnvHandler(config)
            assert handler.build_ssh_options() == []

    def test_ssh_options_empty_when_no_vars(self):
        """Test SSH options are empty when no vars match."""
        config = EnvVarsConfig(include=[], mode="ssh_env")
        handler = RemoteEnvHandler(config)
        assert handler.build_ssh_options() == []


class TestExportFileMode:
    """Test suite for export_file mode (001.4-UNIT-005)."""

    def test_generate_export_script_creates_file(self, monkeypatch, tmp_path):
        """Test export script is created with correct content."""
        monkeypatch.setenv("API_KEY", "my-api-key")
        monkeypatch.setenv("LOG_LEVEL", "INFO")

        config = EnvVarsConfig(
            include=["API_KEY", "LOG_LEVEL"],
            mode="export_file",
        )
        handler = RemoteEnvHandler(config)

        script_path = tmp_path / "env.sh"
        result = handler.generate_export_script(str(script_path))

        assert result == str(script_path)
        assert script_path.exists()

        content = script_path.read_text()
        assert "#!/bin/bash" in content
        assert "export API_KEY='my-api-key'" in content
        assert "export LOG_LEVEL='INFO'" in content

    def test_export_script_escapes_single_quotes(self, monkeypatch, tmp_path):
        """Test that single quotes in values are properly escaped."""
        monkeypatch.setenv("QUERY", "SELECT * FROM users WHERE name='test'")

        config = EnvVarsConfig(include=["QUERY"], mode="export_file")
        handler = RemoteEnvHandler(config)

        script_path = tmp_path / "env.sh"
        handler.generate_export_script(str(script_path))

        content = script_path.read_text()
        # Single quotes should be escaped as: '\"'\"'
        assert "QUERY=" in content
        # The value should be properly escaped
        assert "'\"'\"'" in content

    def test_export_script_returns_none_for_other_modes(self, monkeypatch, tmp_path):
        """Test export script returns None for non-export_file modes."""
        monkeypatch.setenv("API_KEY", "value")

        for mode in ("ssh_env", "none"):
            config = EnvVarsConfig(include=["API_KEY"], mode=mode)
            handler = RemoteEnvHandler(config)
            result = handler.generate_export_script(str(tmp_path / "env.sh"))
            assert result is None

    def test_get_source_command(self, monkeypatch):
        """Test source command is generated correctly."""
        config = EnvVarsConfig(mode="export_file")
        handler = RemoteEnvHandler(config)

        cmd = handler.get_source_command("/tmp/tea-jobs/env.sh")
        # shlex.quote adds quotes, but simple paths may not need them
        assert "source " in cmd
        assert "/tmp/tea-jobs/env.sh" in cmd
        assert cmd.endswith(" && ")

    def test_get_source_command_returns_empty_for_other_modes(self):
        """Test source command is empty for non-export_file modes."""
        for mode in ("ssh_env", "none"):
            config = EnvVarsConfig(mode=mode)
            handler = RemoteEnvHandler(config)
            assert handler.get_source_command("/path/to/env.sh") == ""


class TestNoneMode:
    """Test suite for none mode (001.4-INT-003)."""

    def test_none_mode_skips_env_transfer(self, monkeypatch):
        """Test that none mode doesn't transfer any env vars."""
        monkeypatch.setenv("API_KEY", "value")

        config = EnvVarsConfig(
            include=["API_KEY"],
            mode="none",
        )
        handler = RemoteEnvHandler(config)

        # SSH options should be empty
        assert handler.build_ssh_options() == []

        # Export script should not be generated
        assert handler.generate_export_script("/tmp/env.sh") is None

        # Source command should be empty
        assert handler.get_source_command("/tmp/env.sh") == ""

    def test_get_env_for_subprocess_returns_none_for_none_mode(self):
        """Test subprocess env is None for none mode."""
        config = EnvVarsConfig(mode="none")
        handler = RemoteEnvHandler(config)
        assert handler.get_env_for_subprocess() is None


class TestParseEnvVarsConfig:
    """Test suite for YAML parsing (001.4-INT-005)."""

    def test_parse_none_returns_defaults(self):
        """Test parsing None returns default config."""
        config = parse_env_vars_config(None)
        assert config.include == []
        assert config.exclude_patterns == []
        assert config.mode == "ssh_env"

    def test_parse_empty_dict_returns_defaults(self):
        """Test parsing empty dict returns default config."""
        config = parse_env_vars_config({})
        assert config.include == []
        assert config.exclude_patterns == []
        assert config.mode == "ssh_env"

    def test_parse_full_config(self):
        """Test parsing full configuration dict."""
        config = parse_env_vars_config(
            {
                "include": ["API_KEY", "LOG_LEVEL"],
                "exclude_patterns": ["*_SECRET"],
                "mode": "export_file",
            }
        )
        assert config.include == ["API_KEY", "LOG_LEVEL"]
        assert config.exclude_patterns == ["*_SECRET"]
        assert config.mode == "export_file"

    def test_parse_partial_config(self):
        """Test parsing partial configuration uses defaults for missing keys."""
        config = parse_env_vars_config(
            {
                "include": ["MY_VAR"],
            }
        )
        assert config.include == ["MY_VAR"]
        assert config.exclude_patterns == []  # Default
        assert config.mode == "ssh_env"  # Default


class TestIntegration:
    """Integration tests for end-to-end scenarios."""

    def test_ssh_env_mode_end_to_end(self, monkeypatch):
        """001.4-INT-001: ssh_env mode works end-to-end."""
        monkeypatch.setenv("OPENAI_API_KEY", "sk-xxx")
        monkeypatch.setenv("DEBUG", "true")
        monkeypatch.setenv("SECRET_TOKEN", "should-be-excluded")

        config_dict = {
            "include": ["OPENAI_API_KEY", "DEBUG", "SECRET_TOKEN"],
            "exclude_patterns": ["*_TOKEN"],
            "mode": "ssh_env",
        }

        config = parse_env_vars_config(config_dict)
        handler = RemoteEnvHandler(config)

        # Get filtered vars
        vars = handler.get_filtered_env_vars()
        assert "OPENAI_API_KEY" in vars
        assert "DEBUG" in vars
        assert "SECRET_TOKEN" not in vars

        # Get SSH options
        opts = handler.build_ssh_options()
        assert "-o" in opts
        assert "SendEnv=OPENAI_API_KEY" in opts
        assert "SendEnv=DEBUG" in opts

    def test_export_file_mode_end_to_end(self, monkeypatch, tmp_path):
        """001.4-INT-002: export_file mode transfers and sources."""
        monkeypatch.setenv("API_KEY", "my-key")
        monkeypatch.setenv("DB_PASSWORD", "secret")

        config_dict = {
            "include": ["API_KEY", "DB_PASSWORD"],
            "exclude_patterns": ["*_PASSWORD"],
            "mode": "export_file",
        }

        config = parse_env_vars_config(config_dict)
        handler = RemoteEnvHandler(config)

        # Generate script
        script_path = tmp_path / "env.sh"
        handler.generate_export_script(str(script_path))

        content = script_path.read_text()
        assert "export API_KEY='my-key'" in content
        assert "DB_PASSWORD" not in content

        # Get source command
        source_cmd = handler.get_source_command("/remote/env.sh")
        assert source_cmd.startswith("source ")
        assert source_cmd.endswith(" && ")

    def test_yaml_config_parsing_integration(self, monkeypatch):
        """001.4-INT-005: Full YAML config parsing integration."""
        monkeypatch.setenv("ANTHROPIC_API_KEY", "claude-key")

        # Simulate what yaml_engine.py does
        yaml_config = {
            "settings": {
                "parallel": {
                    "strategy": "remote",
                    "remote": {
                        "hosts": ["user@server1", "user@server2"],
                        "env_vars": {
                            "include": ["ANTHROPIC_API_KEY"],
                            "exclude_patterns": [],
                            "mode": "ssh_env",
                        },
                    },
                },
            },
        }

        env_vars_dict = yaml_config["settings"]["parallel"]["remote"]["env_vars"]
        config = parse_env_vars_config(env_vars_dict)

        assert config.include == ["ANTHROPIC_API_KEY"]
        assert config.mode == "ssh_env"

        handler = RemoteEnvHandler(config)
        vars = handler.get_filtered_env_vars()
        assert vars["ANTHROPIC_API_KEY"] == "claude-key"
