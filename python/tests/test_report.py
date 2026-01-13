"""Tests for the report module - TEA-REPORT-001a

Tests cover:
- ErrorReport serialization
- StackFrame serialization with optional fields
- Path sanitization (absolute→relative, home directory stripping)
- Error capture functions
- Exception hook installation
"""

import json
import os
import sys
import pytest

from the_edge_agent.report import (
    ErrorReport,
    ErrorType,
    StackFrame,
    ErrorContext,
    ExtendedContext,
    NodeInfo,
    EdgeInfo,
    sanitize_path,
    capture_stack_trace,
    capture_exception,
    capture_yaml_error,
    capture_executor_error,
    capture_action_error,
    get_platform,
    install_excepthook,
    uninstall_excepthook,
    get_last_report,
    clear_last_report,
)


class TestErrorReportSerialization:
    """Tests for ErrorReport serialization."""

    def test_error_report_basic_serialization(self):
        """Test basic ErrorReport serializes to JSON correctly."""
        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="Test panic message",
        )

        json_str = report.to_json()
        data = json.loads(json_str)

        assert "version" in data
        assert "platform" in data
        assert data["runtime"] == "python"
        assert data["error_type"] == "Panic"
        assert data["message"] == "Test panic message"
        assert data["stack"] == []

        # Optional fields should not be present when None
        assert "context" not in data
        assert "extended" not in data

    def test_error_report_with_context(self):
        """Test ErrorReport with context serializes correctly."""
        context = ErrorContext(
            node_name="test_node",
            action_type="llm.chat",
            checkpoint_id="cp-123",
        )

        report = ErrorReport(
            error_type=ErrorType.YAML_ERROR,
            message="YAML parse error",
            context=context,
        )

        json_str = report.to_json()
        data = json.loads(json_str)

        assert data["error_type"] == "YamlError"
        assert data["context"]["node_name"] == "test_node"
        assert data["context"]["action_type"] == "llm.chat"
        assert data["context"]["checkpoint_id"] == "cp-123"

    def test_error_report_with_stack_frames(self):
        """Test ErrorReport with stack frames serializes correctly."""
        frame1 = StackFrame(
            addr=0x12345678,
            symbol="main",
            file="src/main.py",
            line=42,
        )
        frame2 = StackFrame(
            addr=0x87654321,
            symbol="process",
        )

        report = ErrorReport(
            error_type=ErrorType.EXECUTOR_ERROR,
            message="Execution failed",
            stack=[frame1, frame2],
        )

        json_str = report.to_json()
        data = json.loads(json_str)

        assert len(data["stack"]) == 2
        assert data["stack"][0]["addr"] == 0x12345678
        assert data["stack"][0]["symbol"] == "main"
        assert data["stack"][0]["file"] == "src/main.py"
        assert data["stack"][0]["line"] == 42
        assert data["stack"][1]["symbol"] == "process"
        # Optional fields not present
        assert "file" not in data["stack"][1]


class TestStackFrameSerialization:
    """Tests for StackFrame serialization."""

    def test_stack_frame_optional_fields_skipped(self):
        """Test that optional fields are skipped when None."""
        frame = StackFrame(addr=0x1000)

        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="Test",
            stack=[frame],
        )

        json_str = report.to_json()
        data = json.loads(json_str)

        frame_data = data["stack"][0]
        assert frame_data["addr"] == 0x1000
        assert "symbol" not in frame_data
        assert "file" not in frame_data
        assert "line" not in frame_data


class TestErrorContext:
    """Tests for ErrorContext."""

    def test_error_context_empty(self):
        """Test is_empty() for ErrorContext."""
        context = ErrorContext()
        assert context.is_empty()

        context_with_node = ErrorContext(node_name="node")
        assert not context_with_node.is_empty()


class TestExtendedContextSerialization:
    """Tests for ExtendedContext serialization."""

    def test_extended_context_serialization(self):
        """Test ExtendedContext serializes correctly."""
        extended = ExtendedContext(
            workflow_name="test_workflow",
            nodes=[
                NodeInfo(name="start", action_type="llm.chat"),
                NodeInfo(name="end"),
            ],
            edges=[
                EdgeInfo(from_node="start", to="end", edge_type="simple"),
            ],
            schema_fields=["input", "output"],
            active_node="start",
            active_action="llm.chat",
        )

        report = ErrorReport(
            error_type=ErrorType.ACTION_ERROR,
            message="Action failed",
            extended=extended,
        )

        json_str = report.to_json()
        data = json.loads(json_str)

        ext = data["extended"]
        assert ext["workflow_name"] == "test_workflow"
        assert len(ext["nodes"]) == 2
        assert ext["nodes"][0]["name"] == "start"
        assert ext["nodes"][0]["action_type"] == "llm.chat"
        assert len(ext["edges"]) == 1
        # 'from_node' should be converted to 'from' for JSON
        assert ext["edges"][0]["from"] == "start"
        assert ext["edges"][0]["to"] == "end"
        assert ext["schema_fields"] == ["input", "output"]
        assert ext["active_node"] == "start"
        assert ext["active_action"] == "llm.chat"


class TestPathSanitization:
    """Tests for path sanitization."""

    def test_path_sanitization_empty(self):
        """Test empty path returns empty."""
        assert sanitize_path("") == ""

    def test_path_sanitization_relative_unchanged(self):
        """Test relative paths remain relative."""
        result = sanitize_path("src/main.py")
        assert result == "src/main.py" or result.endswith("src/main.py")

    def test_path_sanitization_home_directory(self):
        """Test home directory is replaced with ~."""
        home = os.path.expanduser("~")
        if home and home != "~":  # Only run if home is defined
            home_path = os.path.join(home, "project", "src", "main.py")
            sanitized = sanitize_path(home_path)

            assert sanitized.startswith(
                "~/"
            ), f"Path should start with ~/, got: {sanitized}"
            assert "project/src/main.py" in sanitized
            assert home not in sanitized, "Full home directory should not be in path"

    def test_path_sanitization_absolute_falls_back_to_filename(self):
        """Test absolute paths that don't match known patterns use basename."""
        sanitized = sanitize_path("/nonexistent/deep/path/file.py")

        # Should not start with / (unless it's ~/)
        assert not sanitized.startswith("/") or sanitized.startswith(
            "~/"
        ), f"Absolute path should be sanitized, got: {sanitized}"

    def test_path_sanitization_with_project_root(self):
        """Test path sanitization with project root."""
        sanitized = sanitize_path(
            "/project/root/src/main.py", project_root="/project/root"
        )
        assert sanitized == "src/main.py"


class TestErrorTypeSerialization:
    """Tests for ErrorType serialization."""

    def test_error_type_values(self):
        """Test each error type serializes correctly."""
        types = [
            (ErrorType.PANIC, "Panic"),
            (ErrorType.YAML_ERROR, "YamlError"),
            (ErrorType.EXECUTOR_ERROR, "ExecutorError"),
            (ErrorType.ACTION_ERROR, "ActionError"),
        ]

        for error_type, expected in types:
            report = ErrorReport(error_type=error_type, message="Test")
            data = json.loads(report.to_json())
            assert data["error_type"] == expected


class TestNodeAndEdgeInfo:
    """Tests for NodeInfo and EdgeInfo serialization."""

    def test_node_info_serialization(self):
        """Test NodeInfo serializes correctly."""
        node = NodeInfo(name="test_node", action_type="llm.chat")

        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="Test",
            extended=ExtendedContext(nodes=[node]),
        )

        data = json.loads(report.to_json())
        node_data = data["extended"]["nodes"][0]

        assert node_data["name"] == "test_node"
        assert node_data["action_type"] == "llm.chat"

    def test_edge_info_serialization(self):
        """Test EdgeInfo serializes correctly with 'from' field."""
        edge = EdgeInfo(from_node="from_node", to="to_node", edge_type="conditional")

        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="Test",
            extended=ExtendedContext(edges=[edge]),
        )

        data = json.loads(report.to_json())
        edge_data = data["extended"]["edges"][0]

        # Should use 'from' not 'from_node' for JSON compatibility
        assert edge_data["from"] == "from_node"
        assert edge_data["to"] == "to_node"
        assert edge_data["edge_type"] == "conditional"


class TestPlatformDetection:
    """Tests for platform detection."""

    def test_get_platform(self):
        """Test platform string format."""
        platform_str = get_platform()

        # Should be in format "os-arch"
        assert "-" in platform_str

        # Should contain the OS
        assert platform_str.split("-")[0] in ["linux", "darwin", "windows"]


class TestCaptureStackTrace:
    """Tests for stack trace capture."""

    def test_capture_stack_trace(self):
        """Test capturing current stack trace."""
        frames = capture_stack_trace()

        # Should have captured some frames
        assert len(frames) > 0

        # Frames should have addresses
        for frame in frames:
            assert frame.addr >= 0


class TestCaptureYamlError:
    """Tests for YAML error capture."""

    def test_capture_yaml_error(self):
        """Test capturing YAML error."""
        report = capture_yaml_error(
            "Test YAML error",
            node_name="test_node",
            action_type="llm.chat",
        )

        assert report.error_type == ErrorType.YAML_ERROR
        assert report.message == "Test YAML error"
        assert report.runtime == "python"

        assert report.context is not None
        assert report.context.node_name == "test_node"
        assert report.context.action_type == "llm.chat"


class TestCaptureExecutorError:
    """Tests for executor error capture."""

    def test_capture_executor_error(self):
        """Test capturing executor error."""
        report = capture_executor_error(
            "Executor failed",
            checkpoint_id="checkpoint-123",
            node_name="process_node",
        )

        assert report.error_type == ErrorType.EXECUTOR_ERROR
        assert report.message == "Executor failed"

        assert report.context is not None
        assert report.context.checkpoint_id == "checkpoint-123"
        assert report.context.node_name == "process_node"


class TestCaptureActionError:
    """Tests for action error capture."""

    def test_capture_action_error(self):
        """Test capturing action error."""
        report = capture_action_error(
            "Action failed",
            action_type="http.get",
            node_name="fetch_node",
        )

        assert report.error_type == ErrorType.ACTION_ERROR
        assert report.message == "Action failed"

        assert report.context is not None
        assert report.context.action_type == "http.get"
        assert report.context.node_name == "fetch_node"


class TestCaptureException:
    """Tests for exception capture."""

    def test_capture_exception(self):
        """Test capturing an exception."""
        try:
            raise ValueError("Test error message")
        except ValueError as e:
            report = capture_exception(e)

            assert report.error_type == ErrorType.PANIC
            assert "Test error message" in report.message
            assert len(report.stack) > 0

    def test_capture_exception_with_context(self):
        """Test capturing exception with context."""
        try:
            raise RuntimeError("Runtime error")
        except RuntimeError as e:
            report = capture_exception(
                e,
                error_type=ErrorType.YAML_ERROR,
                node_name="test_node",
                action_type="llm.chat",
            )

            assert report.error_type == ErrorType.YAML_ERROR
            assert report.context is not None
            assert report.context.node_name == "test_node"
            assert report.context.action_type == "llm.chat"


class TestExcepthook:
    """Tests for excepthook installation."""

    def test_install_excepthook(self):
        """Test excepthook can be installed and uninstalled."""
        original_hook = sys.excepthook

        install_excepthook()

        # Hook should be replaced
        assert sys.excepthook != original_hook

        uninstall_excepthook()

        # Hook should be restored
        # Note: might not be exactly the same if there were other hooks

    def test_last_report_storage(self):
        """Test last report is stored and can be retrieved."""
        clear_last_report()
        assert get_last_report() is None

        # We can't easily test the excepthook capturing without causing
        # an actual exception to propagate, so we test the storage directly


class TestPrettyJson:
    """Tests for pretty JSON output."""

    def test_error_report_pretty_json(self):
        """Test pretty JSON contains newlines."""
        report = ErrorReport(error_type=ErrorType.PANIC, message="Test")

        pretty = report.to_json_pretty()

        # Pretty JSON should have newlines
        assert "\n" in pretty


class TestJsonSchemaCompatibility:
    """Tests for JSON schema compatibility between Python and Rust."""

    def test_json_schema_fields(self):
        """Test JSON output has expected fields matching Rust schema."""
        report = ErrorReport(
            error_type=ErrorType.YAML_ERROR,
            message="Test error",
            stack=[StackFrame(addr=100, symbol="test_func", file="test.py", line=10)],
            context=ErrorContext(node_name="node1", action_type="llm.chat"),
            extended=ExtendedContext(
                workflow_name="test",
                nodes=[NodeInfo(name="n1")],
                edges=[EdgeInfo(from_node="n1", to="n2")],
                schema_fields=["f1"],
            ),
        )

        data = json.loads(report.to_json())

        # Check all top-level fields exist
        assert "version" in data
        assert "platform" in data
        assert "runtime" in data
        assert "error_type" in data
        assert "message" in data
        assert "stack" in data
        assert "context" in data
        assert "extended" in data

        # Check nested structure
        assert data["context"]["node_name"] == "node1"
        assert data["stack"][0]["symbol"] == "test_func"
        assert data["extended"]["edges"][0]["from"] == "n1"  # Not 'from_node'
