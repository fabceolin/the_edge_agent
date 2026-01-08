"""
Tests for YAML parallel_strategy parsing (TEA-PARALLEL-001.1).

This module tests the YAML parsing of parallel execution settings:
- settings.parallel.strategy
- settings.parallel.max_workers

Test Coverage:
- AC6-1: Parse strategy: thread
- AC6-2: Parse strategy: process
- AC6-3: Default strategy is thread
- AC6-4: Parse max_workers
- AC6-5: Invalid strategy warning

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

import logging
import pytest
import yaml as yaml_lib
from unittest.mock import patch

from the_edge_agent import YAMLEngine


def create_engine_from_yaml(yaml_content: str) -> YAMLEngine:
    """Helper to create YAMLEngine from YAML content string."""
    engine = YAMLEngine()
    config = yaml_lib.safe_load(yaml_content)
    engine.load_from_dict(config)
    return engine


class TestYAMLParallelStrategyParsing:
    """Test YAML parsing of parallel execution settings (AC6)."""

    def test_001_1_yaml_001_parse_strategy_thread(self):
        """001.1-YAML-001: Parse settings.parallel.strategy: thread."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

settings:
  parallel:
    strategy: thread

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = create_engine_from_yaml(yaml_content)

        # Check that the graph has thread strategy in parallel_config
        assert engine._current_graph is not None
        assert engine._current_graph.parallel_config.strategy == "thread"

    def test_001_1_yaml_002_parse_strategy_process(self):
        """001.1-YAML-002: Parse settings.parallel.strategy: process."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

settings:
  parallel:
    strategy: process

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = create_engine_from_yaml(yaml_content)

        # Check that the graph has process strategy in parallel_config
        assert engine._current_graph is not None
        assert engine._current_graph.parallel_config.strategy == "process"

    def test_001_1_yaml_003_default_strategy_thread(self):
        """001.1-YAML-003: Default strategy is thread when not specified."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = create_engine_from_yaml(yaml_content)

        # Default should be thread
        assert engine._current_graph is not None
        assert engine._current_graph.parallel_config.strategy == "thread"

    def test_001_1_yaml_004_parse_max_workers(self):
        """001.1-YAML-004: Parse settings.parallel.max_workers."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

settings:
  parallel:
    strategy: thread
    max_workers: 4

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = create_engine_from_yaml(yaml_content)

        # Check max_workers is set
        assert engine._current_graph is not None
        assert engine._current_graph.max_workers == 4
        assert engine._current_graph.parallel_config.max_workers == 4

    def test_001_1_yaml_005_invalid_strategy_warning(self, caplog):
        """001.1-YAML-005: Invalid strategy logs warning and defaults to thread."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

settings:
  parallel:
    strategy: invalid_strategy

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        with caplog.at_level(logging.WARNING):
            engine = create_engine_from_yaml(yaml_content)

        # Should log a warning
        assert "Invalid parallel strategy" in caplog.text
        assert "invalid_strategy" in caplog.text

        # Should fall back to thread
        assert engine._current_graph is not None
        assert engine._current_graph.parallel_config.strategy == "thread"

    def test_001_1_yaml_006_empty_parallel_settings(self):
        """Empty parallel settings should use defaults."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

settings:
  parallel: {}

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = create_engine_from_yaml(yaml_content)

        # Default values
        assert engine._current_graph is not None
        assert engine._current_graph.parallel_config.strategy == "thread"
        assert engine._current_graph.max_workers is None

    def test_001_1_yaml_007_max_workers_null(self):
        """max_workers: null should use default (None)."""
        yaml_content = """
name: test-agent
state_schema:
  value: int

settings:
  parallel:
    strategy: thread
    max_workers: null

nodes:
  - name: process
    run: |
      return {"value": 42}
"""
        engine = create_engine_from_yaml(yaml_content)

        # max_workers should be None
        assert engine._current_graph is not None
        assert engine._current_graph.max_workers is None


class TestYAMLParallelExecution:
    """Test that YAML parallel settings are used during execution."""

    def test_yaml_parallel_config_from_settings(self):
        """Parallel config from YAML settings is passed to graph."""
        config = {
            "name": "parallel-test",
            "state_schema": {
                "input": "str",
            },
            "settings": {
                "parallel": {
                    "strategy": "thread",
                    "max_workers": 2,
                }
            },
            "nodes": [
                {
                    "name": "start",
                    "run": "return {}",
                },
            ],
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Verify the parallel config is set correctly
        assert graph.parallel_config.strategy == "thread"
        assert graph.parallel_config.max_workers == 2
        assert graph.max_workers == 2

    def test_yaml_parallel_config_process_strategy(self):
        """Process strategy from YAML settings is passed to graph."""
        config = {
            "name": "process-test",
            "state_schema": {
                "input": "str",
            },
            "settings": {
                "parallel": {
                    "strategy": "process",
                    "max_workers": 4,
                }
            },
            "nodes": [
                {
                    "name": "start",
                    "run": "return {}",
                },
            ],
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Verify the parallel config is set to process
        assert graph.parallel_config.strategy == "process"
        assert graph.parallel_config.max_workers == 4
        assert graph.max_workers == 4


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
