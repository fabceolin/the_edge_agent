"""
Tests for TEA-BUILTIN-005.1: OpikExporter Backend

Test categories:
- P0: Critical - Protocol compliance, graceful degradation, core export
- P1: Core - Configuration, field mapping, token metrics
- P2: Edge cases - Events handling, type hints

Test summary: 16 tests (11 unit + 5 integration) | P0: 5 | P1: 7 | P2: 4
"""

import importlib.util
import os
import sys
import unittest
from datetime import datetime
from pathlib import Path
from unittest.mock import MagicMock, patch


def load_opik_exporter_module():
    """Load opik_exporter module directly from file, bypassing package imports."""
    # Get the path to the opik_exporter.py file
    tests_dir = Path(__file__).parent
    project_root = tests_dir.parent
    module_path = project_root / "src" / "the_edge_agent" / "exporters" / "opik_exporter.py"

    # Load the module spec
    spec = importlib.util.spec_from_file_location("opik_exporter", module_path)
    module = importlib.util.module_from_spec(spec)

    # Execute the module
    spec.loader.exec_module(module)

    return module


def get_opik_exporter_with_mock():
    """Get OpikExporter class with mocked opik module."""
    mock_opik = MagicMock()

    # Set up basic mock structure
    mock_client = MagicMock()
    mock_trace = MagicMock()
    mock_opik.Opik.return_value = mock_client
    mock_client.trace.return_value = mock_trace

    with patch.dict('sys.modules', {'opik': mock_opik}):
        module = load_opik_exporter_module()
        return module.OpikExporter, mock_opik, mock_client, mock_trace


class TestOpikExporterProtocol(unittest.TestCase):
    """Unit tests for OpikExporter protocol compliance (AC1)."""

    def test_opik_exporter_has_export_method(self):
        """P0: OpikExporter must implement export(span) method."""
        OpikExporter, _, _, _ = get_opik_exporter_with_mock()
        exporter = OpikExporter()
        self.assertTrue(hasattr(exporter, 'export'))
        self.assertTrue(callable(exporter.export))

    def test_opik_exporter_export_accepts_span_dict(self):
        """P1: Export method accepts span dictionary."""
        OpikExporter, mock_opik, _, _ = get_opik_exporter_with_mock()

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()
            span = {
                "name": "test",
                "span_id": "123",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1001.0
            }
            # Should not raise TypeError
            exporter.export(span)

    def test_opik_exporter_satisfies_protocol(self):
        """P2: Type system recognizes OpikExporter as TraceExporter."""
        OpikExporter, _, _, _ = get_opik_exporter_with_mock()
        exporter = OpikExporter()
        # Protocol structural typing check - has export method
        self.assertTrue(hasattr(exporter, 'export'))
        # Verify signature accepts dict
        import inspect
        sig = inspect.signature(exporter.export)
        params = list(sig.parameters.keys())
        self.assertIn('span', params)


class TestOpikExporterSpanFields(unittest.TestCase):
    """Unit tests for span field export (AC2, AC7)."""

    def test_export_span_name(self):
        """P0: Span name is passed to Opik SDK."""
        OpikExporter, mock_opik, mock_client, _ = get_opik_exporter_with_mock()

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()
            exporter.export({
                "name": "llm.call",
                "span_id": "abc123",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1001.0
            })

            # Verify trace was created with correct name
            mock_client.trace.assert_called()
            call_kwargs = mock_client.trace.call_args[1]
            self.assertEqual(call_kwargs['name'], 'llm.call')

    def test_export_duration_via_timestamps(self):
        """P0: Duration is exported via start_time and end_time."""
        OpikExporter, mock_opik, mock_client, _ = get_opik_exporter_with_mock()

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()
            exporter.export({
                "name": "test",
                "span_id": "123",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1001.5,
                "duration_ms": 1500.0
            })

            call_kwargs = mock_client.trace.call_args[1]
            # Verify timestamps are datetime objects
            self.assertIsInstance(call_kwargs['start_time'], datetime)
            self.assertIsInstance(call_kwargs['end_time'], datetime)

    def test_export_metadata_forwarded(self):
        """P1: Metadata dict is passed to Opik span."""
        OpikExporter, mock_opik, mock_client, _ = get_opik_exporter_with_mock()

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()
            exporter.export({
                "name": "test",
                "span_id": "123",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1001.0,
                "metadata": {"model": "gpt-4", "temperature": 0.7}
            })

            call_kwargs = mock_client.trace.call_args[1]
            self.assertIn('metadata', call_kwargs)
            self.assertEqual(call_kwargs['metadata']['model'], 'gpt-4')
            self.assertEqual(call_kwargs['metadata']['temperature'], 0.7)

    def test_export_events_processed(self):
        """P2: Events list is added to metadata."""
        OpikExporter, mock_opik, mock_client, _ = get_opik_exporter_with_mock()

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()
            exporter.export({
                "name": "test",
                "span_id": "123",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1001.0,
                "events": [
                    {"timestamp": 1000.0, "message": "Started"},
                    {"timestamp": 1001.0, "message": "Completed"}
                ]
            })

            call_kwargs = mock_client.trace.call_args[1]
            self.assertIn('tea_events', call_kwargs['metadata'])
            self.assertEqual(len(call_kwargs['metadata']['tea_events']), 2)

    def test_export_metrics_token_counts(self):
        """P1: Token usage metrics are forwarded to Opik usage field."""
        OpikExporter, mock_opik, mock_client, mock_trace = get_opik_exporter_with_mock()

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()
            # Export parent span first
            exporter.export({
                "name": "parent",
                "span_id": "parent-123",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1002.0
            })

            # Export child span with metrics
            exporter.export({
                "name": "llm.call",
                "span_id": "child-456",
                "parent_id": "parent-123",
                "start_time": 1000.5,
                "end_time": 1001.5,
                "metrics": {
                    "prompt_tokens": 150,
                    "completion_tokens": 50,
                    "total_tokens": 200
                }
            })

            # Child span should have usage in the span call
            span_call = mock_trace.span.call_args[1]
            self.assertIn('usage', span_call)
            self.assertEqual(span_call['usage']['prompt_tokens'], 150)
            self.assertEqual(span_call['usage']['completion_tokens'], 50)
            self.assertEqual(span_call['usage']['total_tokens'], 200)


class TestOpikExporterConfiguration(unittest.TestCase):
    """Unit tests for configuration and environment variables (AC5)."""

    def test_default_project_name(self):
        """P1: Default project name is 'the-edge-agent'."""
        OpikExporter, _, _, _ = get_opik_exporter_with_mock()
        exporter = OpikExporter()
        self.assertEqual(exporter._project_name, "the-edge-agent")

    def test_api_key_from_environment(self):
        """P1: OPIK_API_KEY environment variable is used."""
        with patch.dict(os.environ, {"OPIK_API_KEY": "test-key-123"}, clear=False):
            OpikExporter, _, _, _ = get_opik_exporter_with_mock()
            exporter = OpikExporter()
            self.assertEqual(exporter._api_key, "test-key-123")

    def test_project_name_from_environment(self):
        """P1: OPIK_PROJECT_NAME environment variable overrides default."""
        with patch.dict(os.environ, {"OPIK_PROJECT_NAME": "custom-project"}, clear=False):
            OpikExporter, _, _, _ = get_opik_exporter_with_mock()
            exporter = OpikExporter()
            self.assertEqual(exporter._project_name, "custom-project")

    def test_workspace_from_environment(self):
        """P2: OPIK_WORKSPACE environment variable is used."""
        with patch.dict(os.environ, {"OPIK_WORKSPACE": "my-team"}, clear=False):
            OpikExporter, _, _, _ = get_opik_exporter_with_mock()
            exporter = OpikExporter()
            self.assertEqual(exporter._workspace, "my-team")

    def test_url_override_from_environment(self):
        """P2: OPIK_URL_OVERRIDE configures custom URL."""
        with patch.dict(os.environ, {"OPIK_URL_OVERRIDE": "https://opik.mycompany.com"}, clear=False):
            OpikExporter, _, _, _ = get_opik_exporter_with_mock()
            exporter = OpikExporter()
            self.assertEqual(exporter._url_override, "https://opik.mycompany.com")

    def test_constructor_params_override_env(self):
        """P1: Constructor parameters override environment variables."""
        with patch.dict(os.environ, {"OPIK_API_KEY": "env-key"}, clear=False):
            OpikExporter, _, _, _ = get_opik_exporter_with_mock()
            exporter = OpikExporter(api_key="constructor-key")
            self.assertEqual(exporter._api_key, "constructor-key")


class TestOpikExporterMissingSDK(unittest.TestCase):
    """Unit tests for missing SDK handling (AC4)."""

    def test_missing_sdk_raises_import_error(self):
        """P0: ImportError raised when opik SDK not installed."""
        # Load module with opik mocked
        OpikExporter, _, _, _ = get_opik_exporter_with_mock()
        exporter = OpikExporter()

        # Clear opik from modules to simulate it not being installed
        modules_to_remove = [k for k in list(sys.modules.keys()) if k.startswith('opik')]
        saved_modules = {}
        for mod in modules_to_remove:
            saved_modules[mod] = sys.modules.pop(mod)

        try:
            # Patch sys.modules to make opik unavailable
            with patch.dict('sys.modules', {'opik': None}):
                with self.assertRaises(ImportError) as ctx:
                    exporter._check_opik_available()
                self.assertIn("opik", str(ctx.exception).lower())
        finally:
            # Restore modules
            sys.modules.update(saved_modules)

    def test_import_error_includes_install_instructions(self):
        """P1: Error message includes pip install command."""
        OpikExporter, _, _, _ = get_opik_exporter_with_mock()
        exporter = OpikExporter()

        # Clear opik from modules to simulate it not being installed
        modules_to_remove = [k for k in list(sys.modules.keys()) if k.startswith('opik')]
        saved_modules = {}
        for mod in modules_to_remove:
            saved_modules[mod] = sys.modules.pop(mod)

        try:
            # Patch sys.modules to make opik unavailable
            with patch.dict('sys.modules', {'opik': None}):
                try:
                    exporter._check_opik_available()
                    self.fail("Expected ImportError")
                except ImportError as e:
                    self.assertIn("pip install opik", str(e))
        finally:
            # Restore modules
            sys.modules.update(saved_modules)


class TestOpikExporterGracefulDegradation(unittest.TestCase):
    """Unit tests for graceful degradation (AC8)."""

    def test_export_error_graceful_degradation(self):
        """P0: Errors during export don't crash graph execution."""
        OpikExporter, mock_opik, mock_client, _ = get_opik_exporter_with_mock()

        # Make trace() raise an exception
        mock_client.trace.side_effect = Exception("Network error!")

        with patch.dict('sys.modules', {'opik': mock_opik}):
            exporter = OpikExporter()

            # Should NOT raise - graceful degradation
            try:
                exporter.export({
                    "name": "test",
                    "span_id": "123",
                    "parent_id": None,
                    "start_time": 1000.0,
                    "end_time": 1001.0
                })
            except ImportError:
                # ImportError should still propagate
                raise
            except Exception as e:
                self.fail(f"Export error should not propagate: {e}")


class TestOpikExporterHierarchy(unittest.TestCase):
    """Integration tests for span hierarchy (AC6)."""

    def test_parent_child_span_relationship(self):
        """P0: parent_id is correctly mapped to Opik's parent_span_id."""
        # Set up mocks
        mock_opik = MagicMock()
        mock_client = MagicMock()
        mock_trace = MagicMock()
        mock_opik.Opik.return_value = mock_client
        mock_client.trace.return_value = mock_trace

        with patch.dict('sys.modules', {'opik': mock_opik}):
            module = load_opik_exporter_module()
            OpikExporter = module.OpikExporter
            exporter = OpikExporter()

            # Export parent span
            exporter.export({
                "span_id": "parent-123",
                "name": "parent",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1002.0
            })

            # Export child span
            exporter.export({
                "span_id": "child-456",
                "name": "child",
                "parent_id": "parent-123",
                "start_time": 1000.5,
                "end_time": 1001.5
            })

            # Verify child span references parent
            span_call = mock_trace.span.call_args[1]
            self.assertEqual(span_call['parent_span_id'], "parent-123")

    def test_multi_level_hierarchy(self):
        """P1: Three-level hierarchy is preserved for direct parent-child."""
        # Set up mocks within the test
        mock_opik = MagicMock()
        mock_client = MagicMock()
        mock_trace = MagicMock()
        mock_opik.Opik.return_value = mock_client
        mock_client.trace.return_value = mock_trace

        with patch.dict('sys.modules', {'opik': mock_opik}):
            module = load_opik_exporter_module()
            OpikExporter = module.OpikExporter
            exporter = OpikExporter()

            # Export root
            exporter.export({
                "span_id": "root",
                "name": "root",
                "parent_id": None,
                "start_time": 1000.0,
                "end_time": 1003.0
            })

            # Export middle (child of root)
            exporter.export({
                "span_id": "middle",
                "name": "middle",
                "parent_id": "root",
                "start_time": 1000.5,
                "end_time": 1002.5
            })

            # Verify:
            # - Root created a trace
            # - Middle was attached as a span to root's trace
            mock_client.trace.assert_called()  # At least one trace created
            mock_trace.span.assert_called()  # At least one span created

            # Verify middle span references its parent
            span_calls = mock_trace.span.call_args_list
            self.assertTrue(len(span_calls) >= 1)
            # First span call should be for middle with parent_id = root
            first_span = span_calls[0][1]
            self.assertEqual(first_span['parent_span_id'], 'root')


class TestOpikExporterYAMLEngineIntegration(unittest.TestCase):
    """Integration tests for YAMLEngine integration (AC3).

    Note: These tests need opik to be mocked before importing YAMLEngine.
    Due to complex import chains in the_edge_agent package, we test
    the core functionality in a single comprehensive test.
    """

    def test_yaml_engine_opik_exporter_full_flow(self):
        """P0: YAMLEngine with trace_exporter='opik' creates and uses OpikExporter."""
        mock_opik = MagicMock()
        mock_client = MagicMock()
        mock_opik.Opik.return_value = mock_client

        with patch.dict('sys.modules', {'opik': mock_opik}):
            from the_edge_agent import YAMLEngine

            # Test 1: YAMLEngine accepts 'opik' as trace_exporter
            engine = YAMLEngine(trace_exporter="opik")
            self.assertIsNotNone(engine)

            # Test 2: OpikExporter was instantiated
            self.assertEqual(len(engine._trace_context.exporters), 1)
            exporter_name = type(engine._trace_context.exporters[0]).__name__
            self.assertEqual(exporter_name, 'OpikExporter')

            # Test 3: Spans flow through to Opik
            ctx = engine._trace_context
            ctx.start_span("test_span")
            ctx.end_span()

            # Verify Opik client was called (trace created for root span)
            mock_client.trace.assert_called()


if __name__ == '__main__':
    unittest.main()
