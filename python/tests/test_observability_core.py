"""
Tests for ObservabilityContext Core Infrastructure (TEA-OBS-001.1).

This module tests the flow-scoped observability layer that wraps TraceContext
to provide structured logging, event buffering, and flow log retrieval.

Test Coverage:
- EventStream append and get_all
- EventStream ring buffer max size
- EventStream query filtering
- EventStream thread safety
- ObservabilityContext flow_id injection
- ObservabilityContext start_end_span
- ObservabilityContext log events
- ObservabilityContext shared schema compliance
- ObservabilityContext composition pattern
- ConsoleHandler formatting
- FileHandler JSON lines
- CallbackHandler error resilience
- YAMLEngine observability config parsing
- YAMLEngine flow_id in state
- YAMLEngine node auto-instrumentation
- YAMLEngine observability disabled
- YAMLEngine multiple handlers
- get_flow_log complete trace
- get_flow_log aggregate metrics
- obs.get_flow_log action
"""

import json
import os
import tempfile
import threading
import time
import unittest
from concurrent.futures import ThreadPoolExecutor

import the_edge_agent as tea
from the_edge_agent.observability import (
    EventStream,
    ObservabilityContext,
    ConsoleHandler,
    FileHandler,
    CallbackHandler,
)
from the_edge_agent.tracing import TraceContext


class TestEventStreamAppendAndGetAll(unittest.TestCase):
    """AC2: Test EventStream append and get_all functionality."""

    def test_event_stream_append_and_get_all(self):
        """Test appending events and retrieving all events."""
        stream = EventStream(max_size=100)

        # Append events
        stream.append({"node": "a", "level": "info", "event_type": "entry"})
        stream.append({"node": "b", "level": "info", "event_type": "entry"})
        stream.append({"node": "a", "level": "info", "event_type": "exit"})

        events = stream.get_all()
        self.assertEqual(len(events), 3)
        self.assertEqual(events[0]["node"], "a")
        self.assertEqual(events[1]["node"], "b")
        self.assertEqual(events[2]["node"], "a")

    def test_event_stream_empty(self):
        """Test empty event stream returns empty list."""
        stream = EventStream()
        events = stream.get_all()
        self.assertEqual(events, [])


class TestEventStreamRingBuffer(unittest.TestCase):
    """AC2: Test EventStream ring buffer max size behavior."""

    def test_event_stream_ring_buffer_max_size(self):
        """Test ring buffer evicts oldest events when capacity reached."""
        stream = EventStream(max_size=3)

        # Add 5 events to 3-slot buffer
        for i in range(5):
            stream.append({"node": f"node_{i}", "value": i})

        events = stream.get_all()
        self.assertEqual(len(events), 3)
        # Should have nodes 2, 3, 4 (oldest 0, 1 evicted)
        self.assertEqual(events[0]["value"], 2)
        self.assertEqual(events[1]["value"], 3)
        self.assertEqual(events[2]["value"], 4)

    def test_default_buffer_size(self):
        """Test default buffer size is 1000."""
        stream = EventStream()
        self.assertEqual(stream.max_size, 1000)


class TestEventStreamQuery(unittest.TestCase):
    """AC2: Test EventStream query filtering."""

    def setUp(self):
        self.stream = EventStream(max_size=100)
        self.stream.append(
            {
                "node": "llm.call",
                "level": "info",
                "event_type": "entry",
                "timestamp": 1.0,
            }
        )
        self.stream.append(
            {
                "node": "llm.call",
                "level": "info",
                "event_type": "exit",
                "timestamp": 2.0,
            }
        )
        self.stream.append(
            {
                "node": "http.get",
                "level": "error",
                "event_type": "error",
                "timestamp": 3.0,
            }
        )
        self.stream.append(
            {
                "node": "llm.stream",
                "level": "debug",
                "event_type": "metric",
                "timestamp": 4.0,
            }
        )

    def test_query_by_node_glob(self):
        """Test filtering by node glob pattern."""
        results = self.stream.query({"node": "llm.*"})
        self.assertEqual(len(results), 3)

    def test_query_by_level(self):
        """Test filtering by log level."""
        results = self.stream.query({"level": "error"})
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0]["node"], "http.get")

    def test_query_by_event_type(self):
        """Test filtering by event type."""
        results = self.stream.query({"event_type": "entry"})
        self.assertEqual(len(results), 1)

    def test_query_by_time_range(self):
        """Test filtering by time range."""
        results = self.stream.query({"start_time": 2.0, "end_time": 3.5})
        self.assertEqual(len(results), 2)  # timestamps 2.0 and 3.0

    def test_query_combined_filters(self):
        """Test combining multiple filters."""
        results = self.stream.query({"node": "llm.*", "level": "info"})
        self.assertEqual(len(results), 2)


class TestEventStreamThreadSafety(unittest.TestCase):
    """AC2: Test EventStream thread safety."""

    def test_concurrent_append_and_read(self):
        """Test thread-safe concurrent append and get_all."""
        stream = EventStream(max_size=1000)
        errors = []

        def writer(thread_id):
            for i in range(100):
                try:
                    stream.append({"thread": thread_id, "index": i})
                except Exception as e:
                    errors.append(str(e))

        def reader():
            for _ in range(100):
                try:
                    events = stream.get_all()
                    # Should always be a valid list
                    self.assertIsInstance(events, list)
                except Exception as e:
                    errors.append(str(e))

        with ThreadPoolExecutor(max_workers=10) as executor:
            # Start 5 writers and 5 readers
            futures = []
            for i in range(5):
                futures.append(executor.submit(writer, i))
                futures.append(executor.submit(reader))

            # Wait for all to complete
            for f in futures:
                f.result()

        self.assertEqual(errors, [])
        # Should have 500 events (5 writers * 100 each)
        self.assertEqual(len(stream.get_all()), 500)


class TestObservabilityContextFlowId(unittest.TestCase):
    """AC1: Test ObservabilityContext flow_id injection."""

    def test_auto_generated_flow_id(self):
        """Test flow_id is auto-generated when not provided."""
        ctx = ObservabilityContext()
        self.assertIsNotNone(ctx.flow_id)
        # UUID format check
        self.assertEqual(len(ctx.flow_id), 36)
        self.assertEqual(ctx.flow_id.count("-"), 4)

    def test_custom_flow_id(self):
        """Test custom flow_id is preserved."""
        ctx = ObservabilityContext(flow_id="my-custom-flow-123")
        self.assertEqual(ctx.flow_id, "my-custom-flow-123")

    def test_flow_id_in_events(self):
        """Test flow_id is included in all events."""
        ctx = ObservabilityContext(flow_id="test-flow")
        ctx.log("node1", "info", "entry", "Starting")

        events = ctx.event_stream.get_all()
        self.assertEqual(len(events), 1)
        self.assertEqual(events[0]["flow_id"], "test-flow")


class TestObservabilityContextStartEndSpan(unittest.TestCase):
    """AC1, AC4: Test ObservabilityContext span management."""

    def test_start_end_node_span(self):
        """Test starting and ending node spans."""
        ctx = ObservabilityContext(flow_id="span-test")

        span_id = ctx.start_node_span("process", metadata={"key": "value"})
        self.assertIsNotNone(span_id)

        completed = ctx.end_node_span("process", status="ok")
        self.assertIsNotNone(completed)
        self.assertEqual(completed["status"], "ok")
        self.assertIn("duration_ms", completed)

    def test_span_error_status(self):
        """Test ending span with error status."""
        ctx = ObservabilityContext()

        ctx.start_node_span("failing_node")
        completed = ctx.end_node_span(
            "failing_node", status="error", error="Division by zero"
        )

        self.assertEqual(completed["status"], "error")
        self.assertEqual(completed["error"], "Division by zero")

        # Check error event was logged
        events = ctx.event_stream.get_all()
        error_events = [e for e in events if e["event_type"] == "error"]
        self.assertEqual(len(error_events), 1)
        self.assertIn("Division by zero", error_events[0]["message"])


class TestObservabilityContextLogEvents(unittest.TestCase):
    """AC1: Test ObservabilityContext log method."""

    def test_log_event_basic(self):
        """Test logging basic events."""
        ctx = ObservabilityContext(flow_id="log-test")

        ctx.log("node1", "info", "entry", "Starting node1")
        ctx.log(
            "node1", "info", "exit", "Completed node1", metrics={"duration_ms": 100}
        )

        events = ctx.event_stream.get_all()
        self.assertEqual(len(events), 2)
        self.assertEqual(events[0]["event_type"], "entry")
        self.assertEqual(events[1]["event_type"], "exit")
        self.assertEqual(events[1]["metrics"]["duration_ms"], 100)

    def test_log_level_filtering(self):
        """Test log level threshold filtering."""
        ctx = ObservabilityContext(config={"level": "warn"})

        ctx.log("node", "debug", "metric", "Debug message")
        ctx.log("node", "info", "metric", "Info message")
        ctx.log("node", "warn", "metric", "Warning message")
        ctx.log("node", "error", "error", "Error message")

        events = ctx.event_stream.get_all()
        # Only warn and error should be logged
        self.assertEqual(len(events), 2)
        levels = [e["level"] for e in events]
        self.assertIn("warn", levels)
        self.assertIn("error", levels)


class TestObservabilityContextSchema(unittest.TestCase):
    """AC1: Test shared schema compliance."""

    def test_event_schema_compliance(self):
        """Test events conform to shared schema."""
        ctx = ObservabilityContext(flow_id="schema-test")
        ctx.start_node_span("test_node", metadata={"test": True})
        ctx.log(
            "test_node",
            "info",
            "metric",
            "Test message",
            data={"key": "value"},
            metrics={"count": 42},
        )
        ctx.end_node_span("test_node")

        events = ctx.event_stream.get_all()
        self.assertGreater(len(events), 0)

        for event in events:
            # Required fields
            self.assertIn("flow_id", event)
            self.assertIn("node", event)
            self.assertIn("level", event)
            self.assertIn("timestamp", event)
            self.assertIn("event_type", event)
            # Optional but present
            self.assertIn("span_id", event)
            self.assertIn("data", event)
            self.assertIn("metrics", event)
            # Type checks
            self.assertIsInstance(event["timestamp"], float)
            self.assertIn(event["level"], ["debug", "info", "warn", "error"])
            self.assertIn(event["event_type"], ["entry", "exit", "error", "metric"])


class TestObservabilityContextComposition(unittest.TestCase):
    """AC4: Test composition pattern with TraceContext."""

    def test_composition_not_replacement(self):
        """Test ObservabilityContext composes TraceContext."""
        trace_ctx = TraceContext()
        obs_ctx = ObservabilityContext(trace_context=trace_ctx)

        # Same underlying trace context
        self.assertIs(obs_ctx.trace_context, trace_ctx)

    def test_auto_create_trace_context(self):
        """Test TraceContext is auto-created if not provided."""
        obs_ctx = ObservabilityContext()
        self.assertIsNotNone(obs_ctx.trace_context)
        self.assertIsInstance(obs_ctx.trace_context, TraceContext)

    def test_trace_context_unchanged(self):
        """Test existing TraceContext API remains functional."""
        trace_ctx = TraceContext()

        # Use directly
        span = trace_ctx.start_span("direct_span")
        trace_ctx.log_event(message="Direct log")
        trace_ctx.end_span(status="ok")

        # Verify completed
        self.assertEqual(len(trace_ctx.completed_spans), 1)
        self.assertEqual(trace_ctx.completed_spans[0]["name"], "direct_span")


class TestConsoleHandlerFormatting(unittest.TestCase):
    """AC3: Test ConsoleHandler formatting."""

    def test_console_handler_basic(self):
        """Test ConsoleHandler prints events."""
        handler = ConsoleHandler(verbose=False)

        # Should not raise
        handler.handle(
            {
                "node": "test",
                "level": "info",
                "event_type": "entry",
                "message": "Test message",
            }
        )

    def test_console_handler_verbose(self):
        """Test ConsoleHandler verbose mode includes data."""
        handler = ConsoleHandler(verbose=True)

        # Should not raise with data
        handler.handle(
            {
                "node": "test",
                "level": "info",
                "event_type": "metric",
                "message": "Metrics",
                "data": {"key": "value"},
                "metrics": {"count": 42},
            }
        )


class TestFileHandlerJsonLines(unittest.TestCase):
    """AC3: Test FileHandler JSON lines output."""

    def test_file_handler_writes_jsonl(self):
        """Test FileHandler writes valid JSON lines."""
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "logs", "test.jsonl")
            handler = FileHandler(path=path)

            event1 = {"node": "a", "level": "info", "timestamp": 1.0}
            event2 = {"node": "b", "level": "error", "timestamp": 2.0}

            handler.handle(event1)
            handler.handle(event2)

            # Read and verify
            with open(path, "r") as f:
                lines = f.readlines()

            self.assertEqual(len(lines), 2)
            self.assertEqual(json.loads(lines[0])["node"], "a")
            self.assertEqual(json.loads(lines[1])["node"], "b")

    def test_file_handler_creates_directories(self):
        """Test FileHandler creates parent directories."""
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "nested", "deep", "logs.jsonl")
            handler = FileHandler(path=path)

            handler.handle({"node": "test"})

            self.assertTrue(os.path.exists(path))


class TestCallbackHandlerErrorResilience(unittest.TestCase):
    """AC3: Test CallbackHandler error handling."""

    def test_callback_handler_calls_function(self):
        """Test CallbackHandler calls provided function."""
        events = []
        handler = CallbackHandler(callback=lambda e: events.append(e))

        handler.handle({"node": "a"})
        handler.handle({"node": "b"})

        self.assertEqual(len(events), 2)

    def test_callback_handler_swallows_errors(self):
        """Test CallbackHandler swallows callback errors."""

        def failing_callback(event):
            raise RuntimeError("Callback error")

        handler = CallbackHandler(callback=failing_callback)

        # Should not raise
        handler.handle({"node": "test"})


class TestYAMLEngineObservabilityConfig(unittest.TestCase):
    """AC5: Test YAMLEngine observability config parsing."""

    def test_yaml_engine_observability_config_parsing(self):
        """Test observability config is parsed from YAML."""
        engine = tea.YAMLEngine()
        config = {
            "observability": {
                "enabled": True,
                "level": "debug",
                "buffer_size": 500,
                "handlers": [{"type": "console", "verbose": True}],
            },
            "nodes": [{"name": "start", "run": 'return {"value": 1}'}],
            "edges": [
                {"from": "__start__", "to": "start"},
                {"from": "start", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)

        self.assertTrue(engine._enable_observability)
        self.assertIsNotNone(engine.observability_context)
        self.assertEqual(engine.observability_context._level, "debug")


class TestYAMLEngineFlowIdInState(unittest.TestCase):
    """AC5: Test flow_id injection into state."""

    def test_yaml_engine_flow_id_in_state(self):
        """Test flow_id is injected into state during execution."""
        engine = tea.YAMLEngine()
        config = {
            "observability": {"enabled": True},
            "nodes": [
                {
                    "name": "check_flow_id",
                    "run": """
flow_id = state.get("_observability", {}).get("flow_id")
return {"has_flow_id": flow_id is not None, "flow_id": flow_id}
""",
                }
            ],
            "edges": [
                {"from": "__start__", "to": "check_flow_id"},
                {"from": "check_flow_id", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        result = list(graph.invoke({}))[-1]
        # Result is wrapped in {'type': 'final', 'state': {...}}
        final_state = result.get("state", result)

        self.assertTrue(final_state["has_flow_id"])
        self.assertEqual(final_state["flow_id"], engine.observability_context.flow_id)


class TestYAMLEngineNodeAutoInstrumentation(unittest.TestCase):
    """AC4, AC5: Test automatic node instrumentation."""

    def test_yaml_engine_node_auto_instrumentation(self):
        """Test nodes are automatically instrumented when observability enabled."""
        engine = tea.YAMLEngine()
        config = {
            "observability": {"enabled": True, "level": "info"},
            "nodes": [
                {"name": "step1", "run": 'return {"value": 1}'},
                {"name": "step2", "run": 'return {"value": state["value"] + 1}'},
            ],
            "edges": [
                {"from": "__start__", "to": "step1"},
                {"from": "step1", "to": "step2"},
                {"from": "step2", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        list(graph.invoke({}))

        flow_log = engine.observability_context.get_flow_log()

        # Should have entry and exit events for each node
        events = flow_log["events"]
        nodes_seen = set(e["node"] for e in events)
        self.assertIn("step1", nodes_seen)
        self.assertIn("step2", nodes_seen)

        # Should have metrics
        self.assertEqual(flow_log["metrics"]["node_count"], 2)
        self.assertGreater(flow_log["metrics"]["event_count"], 0)


class TestYAMLEngineObservabilityDisabled(unittest.TestCase):
    """AC5: Test observability disabled by default."""

    def test_yaml_engine_observability_disabled(self):
        """Test no overhead when observability disabled."""
        engine = tea.YAMLEngine()
        config = {
            "nodes": [{"name": "node", "run": 'return {"value": 1}'}],
            "edges": [
                {"from": "__start__", "to": "node"},
                {"from": "node", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)

        self.assertFalse(engine._enable_observability)
        self.assertIsNone(engine.observability_context)


class TestYAMLEngineMultipleHandlers(unittest.TestCase):
    """AC5: Test multiple handlers configuration."""

    def test_yaml_engine_multiple_handlers(self):
        """Test configuring multiple handlers."""
        events_captured = []

        engine = tea.YAMLEngine()
        config = {
            "observability": {
                "enabled": True,
                "handlers": [
                    {"type": "console"},
                    {
                        "type": "callback",
                        "callback": lambda e: events_captured.append(e),
                    },
                ],
            },
            "nodes": [{"name": "test", "run": 'return {"value": 1}'}],
            "edges": [
                {"from": "__start__", "to": "test"},
                {"from": "test", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        list(graph.invoke({}))

        # Callback should have received events
        self.assertGreater(len(events_captured), 0)


class TestGetFlowLogCompleteTrace(unittest.TestCase):
    """AC6: Test get_flow_log returns complete trace."""

    def test_get_flow_log_complete_trace(self):
        """Test get_flow_log returns all events, spans, and timeline."""
        ctx = ObservabilityContext(flow_id="complete-trace")

        # Simulate workflow
        ctx.start_node_span("node1")
        ctx.log("node1", "info", "metric", "Processing", metrics={"items": 10})
        ctx.end_node_span("node1")

        ctx.start_node_span("node2")
        ctx.end_node_span("node2")

        flow_log = ctx.get_flow_log()

        self.assertEqual(flow_log["flow_id"], "complete-trace")
        self.assertIn("events", flow_log)
        self.assertIn("spans", flow_log)
        self.assertIn("metrics", flow_log)
        self.assertIn("timeline", flow_log)

        # Timeline should be sorted
        timeline = flow_log["timeline"]
        timestamps = [t.get("timestamp", t.get("start_time", 0)) for t in timeline]
        self.assertEqual(timestamps, sorted(timestamps))


class TestGetFlowLogAggregateMetrics(unittest.TestCase):
    """AC6: Test get_flow_log aggregate metrics calculation."""

    def test_get_flow_log_aggregate_metrics(self):
        """Test aggregate metrics are calculated correctly."""
        ctx = ObservabilityContext(flow_id="metrics-test")

        # Create events for 3 nodes, 1 error
        ctx.start_node_span("node1")
        ctx.end_node_span("node1")

        ctx.start_node_span("node2")
        ctx.end_node_span("node2")

        ctx.start_node_span("node3")
        ctx.end_node_span("node3", status="error", error="Test error")

        flow_log = ctx.get_flow_log()
        metrics = flow_log["metrics"]

        self.assertEqual(metrics["node_count"], 3)
        self.assertEqual(metrics["error_count"], 1)
        self.assertGreater(metrics["event_count"], 0)
        self.assertIn("total_duration_ms", metrics)


class TestObsGetFlowLogAction(unittest.TestCase):
    """AC6: Test obs.get_flow_log action."""

    def test_obs_get_flow_log_action(self):
        """Test obs.get_flow_log action returns flow log."""
        engine = tea.YAMLEngine()
        config = {
            "observability": {"enabled": True},
            "nodes": [
                {"name": "work", "run": 'return {"done": True}'},
                {"name": "get_log", "uses": "obs.get_flow_log"},
            ],
            "edges": [
                {"from": "__start__", "to": "work"},
                {"from": "work", "to": "get_log"},
                {"from": "get_log", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        result = list(graph.invoke({}))[-1]
        # Result is wrapped in {'type': 'final', 'state': {...}}
        final_state = result.get("state", result)

        self.assertTrue(final_state.get("success", False))
        self.assertIn("flow_log", final_state)
        self.assertIn("flow_id", final_state["flow_log"])

    def test_obs_get_flow_log_action_disabled(self):
        """Test obs.get_flow_log returns error when observability disabled."""
        engine = tea.YAMLEngine()

        # Call action directly
        result = engine.actions_registry["obs.get_flow_log"](state={})

        self.assertFalse(result["success"])
        self.assertIn("error", result)


if __name__ == "__main__":
    unittest.main()
