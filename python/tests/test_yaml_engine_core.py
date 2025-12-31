"""
Tests for YAML Engine - Core Tests

Core YAML Engine tests including:
- YAMLEngine initialization
- File and dict loading
- DotDict helper
- Package integration
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END, MemoryCheckpointer
from the_edge_agent.yaml_engine import DotDict


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def minimal_yaml_config():
    """Minimal valid YAML configuration."""
    return {
        "name": "test-agent",
        "state_schema": {"value": "int"},
        "nodes": [{"name": "start", "run": 'return {"value": 1}'}],
        "edges": [
            {"from": "__start__", "to": "start"},
            {"from": "start", "to": "__end__"},
        ],
    }


@pytest.fixture
def temp_yaml_file(minimal_yaml_config):
    """Create a temporary YAML file."""
    import yaml

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
        yaml.dump(minimal_yaml_config, f)
        yield f.name
    os.unlink(f.name)


# =============================================================================
# AC1: Core YAML Engine - YAMLEngine Class Creation
# =============================================================================


class TestYAMLEngineInit:
    """Tests for YAMLEngine initialization."""

    def test_unit_001_init_creates_valid_instance(self, engine):
        """YAML-001-UNIT-001: Constructor creates valid instance with default actions."""
        assert isinstance(engine, YAMLEngine)
        assert "llm.call" in engine.actions_registry
        assert "http.get" in engine.actions_registry
        assert "http.post" in engine.actions_registry
        assert "file.write" in engine.actions_registry
        assert "file.read" in engine.actions_registry
        assert "actions.notify" in engine.actions_registry

    def test_unit_002_init_accepts_custom_actions(self):
        """YAML-001-UNIT-002: Constructor accepts and merges custom actions_registry."""
        custom_action = lambda state, **kwargs: {"custom": True}
        engine = YAMLEngine(actions_registry={"custom.action": custom_action})

        assert "custom.action" in engine.actions_registry
        # Built-in actions still present
        assert "llm.call" in engine.actions_registry

    def test_unit_003_init_empty_dicts(self, engine):
        """YAML-001-UNIT-003: Initializes empty variables and secrets dicts."""
        assert engine.variables == {}
        assert engine.secrets == {}


# =============================================================================
# AC2: Load from File
# =============================================================================


class TestLoadFromFile:
    """Tests for load_from_file functionality."""

    def test_int_001_load_from_file_returns_stategraph(self, engine, temp_yaml_file):
        """YAML-001-INT-001: load_from_file parses YAML and returns compiled StateGraph."""
        graph = engine.load_from_file(temp_yaml_file)
        assert isinstance(graph, StateGraph)

    def test_int_002_load_from_file_nonexistent_raises(self, engine):
        """YAML-001-INT-002: load_from_file raises error for non-existent file."""
        with pytest.raises(FileNotFoundError):
            engine.load_from_file("/nonexistent/path/file.yaml")

    def test_unit_004_load_from_file_invalid_yaml(self, engine):
        """YAML-001-UNIT-004: load_from_file handles YAML syntax errors."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("invalid: yaml: syntax: [[[")
            f.flush()

            with pytest.raises(Exception):  # yaml.scanner.ScannerError
                engine.load_from_file(f.name)

            os.unlink(f.name)

    def test_int_003_load_from_file_empty(self, engine):
        """YAML-001-INT-003: load_from_file handles empty YAML file."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("")
            f.flush()

            # Empty YAML returns None, which should be handled
            with pytest.raises((TypeError, AttributeError)):
                engine.load_from_file(f.name)

            os.unlink(f.name)


# =============================================================================
# AC3: Load from Dictionary
# =============================================================================


class TestLoadFromDict:
    """Tests for load_from_dict functionality."""

    def test_unit_005_load_from_dict_minimal(self, engine, minimal_yaml_config):
        """YAML-001-UNIT-005: load_from_dict with minimal config returns StateGraph."""
        graph = engine.load_from_dict(minimal_yaml_config)
        assert isinstance(graph, StateGraph)

    def test_unit_006_load_from_dict_extracts_variables(self, engine):
        """YAML-001-UNIT-006: load_from_dict extracts and stores global variables."""
        config = {
            "variables": {"api_key": "test123", "timeout": 30},
            "nodes": [],
            "edges": [],
        }
        engine.load_from_dict(config)

        assert engine.variables == {"api_key": "test123", "timeout": 30}

    def test_unit_007_load_from_dict_applies_state_schema(self, engine):
        """YAML-001-UNIT-007: load_from_dict applies state_schema to graph."""
        config = {
            "state_schema": {"query": str, "count": int},
            "nodes": [],
            "edges": [],
        }
        graph = engine.load_from_dict(config)
        assert graph.state_schema == {"query": str, "count": int}

    def test_unit_008_load_from_dict_applies_raise_exceptions(self, engine):
        """YAML-001-UNIT-008: load_from_dict applies config.raise_exceptions."""
        config = {"config": {"raise_exceptions": True}, "nodes": [], "edges": []}
        graph = engine.load_from_dict(config)
        assert graph.raise_exceptions is True

    def test_unit_009_load_from_dict_applies_interrupts(self, engine):
        """YAML-001-UNIT-009: load_from_dict applies interrupt_before/after."""
        config = {
            "config": {"interrupt_before": ["node_a"], "interrupt_after": ["node_b"]},
            "nodes": [
                {"name": "node_a", "run": "result = {}"},
                {"name": "node_b", "run": "result = {}"},
            ],
            "edges": [
                {"from": "__start__", "to": "node_a"},
                {"from": "node_a", "to": "node_b"},
                {"from": "node_b", "to": "__end__"},
            ],
        }
        # Interrupts require a checkpointer
        graph = engine.load_from_dict(config, checkpointer=MemoryCheckpointer())
        assert "node_a" in graph.interrupt_before
        assert "node_b" in graph.interrupt_after


# =============================================================================
# DotDict Helper Tests
# =============================================================================


class TestDotDict:
    """Tests for DotDict helper class."""

    def test_dotdict_attribute_access(self):
        """DotDict allows attribute-style access to dict keys."""
        d = DotDict({"name": "test", "count": 42})
        assert d.name == "test"
        assert d.count == 42

    def test_dotdict_nested_access(self):
        """DotDict handles nested dicts."""
        d = DotDict({"outer": {"inner": "value"}})
        assert d.outer.inner == "value"

    def test_dotdict_missing_key_returns_none(self):
        """DotDict returns None for missing keys (allows safe Jinja2 access)."""
        d = DotDict({"a": 1})
        # Missing keys return None instead of raising AttributeError
        # This allows safe access in Jinja2 templates like {% if state.x %}
        assert d.nonexistent is None


# =============================================================================
# AC16: Package Integration
# =============================================================================


class TestPackageIntegration:
    """Tests for package exports."""

    def test_int_013_import_yamlengine(self):
        """YAML-001-INT-013: from the_edge_agent import YAMLEngine works."""
        from the_edge_agent import YAMLEngine

        assert YAMLEngine is not None

    def test_int_014_yamlengine_in_all(self):
        """YAML-001-INT-014: YAMLEngine in __all__ exports."""
        from the_edge_agent import __all__

        assert "YAMLEngine" in __all__


# =============================================================================
# Run tests
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
