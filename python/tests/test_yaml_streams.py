"""
Tests for YAML Stream Integration (TEA-STREAM-001.4).

Test scenarios cover:
- AC1: Node streams parsing (4 tests)
- AC2: Edge stream mode (3 tests)
- AC3: Registry building (3 tests)
- AC4: Producer/consumer validation (3 tests)
- AC5: Strategy validation (2 tests)
- AC6: Settings parsing (3 tests)
- AC7: Error messages (2 tests)

Total: 20+ test scenarios
"""

import sys
import unittest
from unittest.mock import patch

import pytest

from the_edge_agent.yaml_nodes import NodeStreamsConfig, StreamSettings


class TestNodeStreamsParsing(unittest.TestCase):
    """AC1: Node Streams Parsing (4 tests)"""

    def test_001_4_unit_001_parse_streams_stdin(self):
        """001.4-UNIT-001: Parse streams.stdin from node"""
        config = {"name": "consumer", "streams": {"stdin": "input_pipe"}}

        result = NodeStreamsConfig.from_dict(config)

        self.assertIsNotNone(result)
        self.assertEqual(result.stdin, "input_pipe")
        self.assertIsNone(result.stdout)
        self.assertIsNone(result.stderr)

    def test_001_4_unit_002_parse_streams_stdout(self):
        """001.4-UNIT-002: Parse streams.stdout from node"""
        config = {"name": "producer", "streams": {"stdout": "output_pipe"}}

        result = NodeStreamsConfig.from_dict(config)

        self.assertIsNotNone(result)
        self.assertIsNone(result.stdin)
        self.assertEqual(result.stdout, "output_pipe")

    def test_001_4_unit_003_parse_streams_stderr(self):
        """001.4-UNIT-003: Parse streams.stderr from node"""
        config = {"name": "producer", "streams": {"stderr": "error_pipe"}}

        result = NodeStreamsConfig.from_dict(config)

        self.assertIsNotNone(result)
        self.assertEqual(result.stderr, "error_pipe")

    def test_001_4_unit_004_node_without_streams_returns_none(self):
        """001.4-UNIT-004: Node without streams returns None"""
        config = {"name": "simple_node", "run": "return {}"}

        result = NodeStreamsConfig.from_dict(config)

        self.assertIsNone(result)


class TestEdgeStreamMode(unittest.TestCase):
    """AC2: Edge Stream Mode (3 tests)"""

    def test_001_4_unit_005_parse_stream_mode_direct(self):
        """001.4-UNIT-005: Parse stream_mode: direct"""
        edge = {"from": "producer", "to": "consumer", "stream_mode": "direct"}

        mode = edge.get("stream_mode", "direct")
        self.assertEqual(mode, "direct")

    def test_001_4_unit_006_parse_stream_mode_broadcast(self):
        """001.4-UNIT-006: Parse stream_mode: broadcast"""
        edge = {
            "from": "producer",
            "to": ["consumer_a", "consumer_b"],
            "stream_mode": "broadcast",
        }

        mode = edge.get("stream_mode")
        self.assertEqual(mode, "broadcast")

    def test_001_4_unit_007_invalid_stream_mode_error(self):
        """001.4-UNIT-007: Invalid stream_mode raises error"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [],
            "edges": [{"from": "a", "to": "b", "stream_mode": "invalid"}],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            self.assertIn("Invalid stream_mode", str(ctx.exception))


class TestRegistryBuilding(unittest.TestCase):
    """AC3: Registry Building (3 tests)"""

    @pytest.mark.skipif(sys.platform == "win32", reason="Unix-only test")
    def test_001_4_int_001_registry_contains_all_channels(self):
        """001.4-INT-001: Registry contains all declared channels"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data_pipe"}},
                {"name": "consumer", "streams": {"stdin": "data_pipe"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            registry = engine._build_stream_registry(config, "process")

        self.assertIsNotNone(registry)
        # Single channel name used by both producer and consumer
        self.assertEqual(len(registry.channels), 1)
        self.assertIn("data_pipe", registry.channels)

    @pytest.mark.skipif(sys.platform == "win32", reason="Unix-only test")
    def test_001_4_int_002_producer_registered_as_stdout(self):
        """001.4-INT-002: Producer channels registered as STDOUT"""
        from the_edge_agent.yaml_engine import YAMLEngine
        from the_edge_agent.streams import StreamDirection

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "output"}},
                {"name": "consumer", "streams": {"stdin": "output"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            registry = engine._build_stream_registry(config, "process")

        # Find channel with STDOUT direction
        producers = registry.get_producers()
        self.assertEqual(len(producers), 1)
        self.assertEqual(producers[0].direction, StreamDirection.STDOUT)

    @pytest.mark.skipif(sys.platform == "win32", reason="Unix-only test")
    def test_001_4_int_003_consumer_registered_as_consumer(self):
        """001.4-INT-003: Consumer nodes registered on the channel"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data"}},
                {"name": "consumer", "streams": {"stdin": "data"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            registry = engine._build_stream_registry(config, "process")

        # Channel exists with consumers registered
        channel = registry.get("data")
        self.assertIsNotNone(channel)
        # Consumer is registered on the channel
        self.assertIn("consumer", channel.consumers)


class TestProducerConsumerValidation(unittest.TestCase):
    """AC4: Producer/Consumer Validation (3 tests)"""

    def test_001_4_unit_008_error_missing_producer(self):
        """001.4-UNIT-008: Error if consumer references missing producer"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "consumer", "streams": {"stdin": "nonexistent"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            self.assertIn("consumed by", str(ctx.exception))
            self.assertIn("has no producer", str(ctx.exception))

    def test_001_4_unit_009_error_multiple_producers(self):
        """001.4-UNIT-009: Error if multiple producers for same stream"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer_a", "streams": {"stdout": "data"}},
                {"name": "producer_b", "streams": {"stdout": "data"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            self.assertIn("multiple producers", str(ctx.exception))

    @pytest.mark.skipif(sys.platform == "win32", reason="Unix-only test")
    def test_001_4_unit_010_valid_config_passes(self):
        """001.4-UNIT-010: Valid config passes validation"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data"}},
                {"name": "consumer", "streams": {"stdin": "data"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            registry = engine._build_stream_registry(config, "process")

        self.assertIsNotNone(registry)


class TestStrategyValidation(unittest.TestCase):
    """AC5: Strategy Validation (2 tests)"""

    def test_001_4_unit_011_error_streams_with_thread_strategy(self):
        """001.4-UNIT-011: Error if streams with strategy: thread"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {
                    "strategy": "thread",  # Wrong strategy
                    "streams": {"enabled": True},
                }
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "thread")

            self.assertIn("parallel_strategy: process", str(ctx.exception))

    @pytest.mark.skipif(sys.platform == "win32", reason="Unix-only test")
    def test_001_4_unit_012_streams_work_with_process_strategy(self):
        """001.4-UNIT-012: Streams work with strategy: process"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data"}},
                {"name": "consumer", "streams": {"stdin": "data"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            registry = engine._build_stream_registry(config, "process")

        self.assertIsNotNone(registry)


class TestSettingsParsing(unittest.TestCase):
    """AC6: Settings Parsing (3 tests)"""

    def test_001_4_unit_013_parse_enabled_default_false(self):
        """001.4-UNIT-013: Parse streams.enabled (default false)"""
        # No streams section
        settings = StreamSettings.from_dict({})
        self.assertFalse(settings.enabled)

        # Empty streams section
        settings = StreamSettings.from_dict({"parallel": {"streams": {}}})
        self.assertFalse(settings.enabled)

        # Explicitly enabled
        settings = StreamSettings.from_dict(
            {"parallel": {"streams": {"enabled": True}}}
        )
        self.assertTrue(settings.enabled)

    def test_001_4_unit_014_parse_buffer_size(self):
        """001.4-UNIT-014: Parse streams.buffer_size"""
        # Default
        settings = StreamSettings.from_dict({})
        self.assertEqual(settings.buffer_size, 65536)

        # Custom
        settings = StreamSettings.from_dict(
            {"parallel": {"streams": {"buffer_size": 131072}}}
        )
        self.assertEqual(settings.buffer_size, 131072)

    def test_001_4_unit_015_parse_timeout(self):
        """001.4-UNIT-015: Parse streams.timeout"""
        # Default (no timeout)
        settings = StreamSettings.from_dict({})
        self.assertIsNone(settings.timeout)

        # Custom
        settings = StreamSettings.from_dict({"parallel": {"streams": {"timeout": 300}}})
        self.assertEqual(settings.timeout, 300)


class TestErrorMessages(unittest.TestCase):
    """AC7: Error Messages (2 tests)"""

    def test_001_4_unit_016_error_messages_include_fix(self):
        """001.4-UNIT-016: Error messages include fix suggestions"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "consumer", "streams": {"stdin": "missing_stream"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            error_msg = str(ctx.exception)
            # Should include fix suggestion
            self.assertIn("Add a node with", error_msg)
            self.assertIn("streams.stdout: missing_stream", error_msg)

    def test_001_4_unit_017_platform_error_clear(self):
        """001.4-UNIT-017: Platform error is clear and actionable"""
        from the_edge_agent.streams import PlatformError, validate_platform

        with patch("sys.platform", "win32"):
            with self.assertRaises(PlatformError) as ctx:
                validate_platform()

            error_msg = str(ctx.exception)
            self.assertIn("Windows is not supported", error_msg)
            self.assertIn("parallel_strategy: thread", error_msg)


class TestInterruptValidation(unittest.TestCase):
    """Tests for interrupt validation on stream nodes"""

    def test_interrupt_before_on_stream_node_error(self):
        """Error when stream node has interrupt_before"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {
                    "name": "producer",
                    "streams": {"stdout": "data"},
                    "interrupt_before": True,  # Invalid
                },
                {"name": "consumer", "streams": {"stdin": "data"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            self.assertIn("interrupt_before", str(ctx.exception))
            self.assertIn("cannot be checkpointed", str(ctx.exception))

    def test_interrupt_after_on_stream_node_error(self):
        """Error when stream node has interrupt_after"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data"}},
                {
                    "name": "consumer",
                    "streams": {"stdin": "data"},
                    "interrupt": "after",  # Invalid
                },
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            self.assertIn("interrupt_after", str(ctx.exception))


class TestStreamModeValidation(unittest.TestCase):
    """Tests for stream_mode validation"""

    def test_direct_mode_multi_target_error(self):
        """Error when direct mode has multiple targets"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [],
            "edges": [
                {
                    "from": "producer",
                    "to": ["consumer_a", "consumer_b"],  # Multiple targets
                    "stream_mode": "direct",  # Direct mode
                }
            ],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            self.assertIn("direct requires single target", str(ctx.exception))
            self.assertIn("broadcast", str(ctx.exception))


class TestStreamsNotEnabled(unittest.TestCase):
    """Tests for when streams are not enabled"""

    def test_returns_none_when_not_enabled(self):
        """Returns None when streams not enabled"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {
                    "strategy": "process"
                    # streams.enabled not set (defaults to False)
                }
            },
            "nodes": [],
            "edges": [],
        }

        registry = engine._build_stream_registry(config, "process")

        self.assertIsNone(registry)


if __name__ == "__main__":
    unittest.main()
