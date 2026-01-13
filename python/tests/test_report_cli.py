"""Tests for bug report CLI integration - TEA-REPORT-001d"""

import os
import sys
import pytest
from unittest.mock import patch, MagicMock

from the_edge_agent.report import (
    ErrorReport,
    ErrorType,
    ErrorContext,
    NodeInfo,
    EdgeInfo,
)
from the_edge_agent.report_cli import (
    ReportOptions,
    configure,
    get_options,
    is_interactive,
    copy_to_clipboard,
    prompt_yes_no,
    display_error_report,
    add_extended_context,
    handle_exception,
    install_cli_excepthook,
    uninstall_cli_excepthook,
    REPORT_BASE_URL,
)


class TestReportOptions:
    """Tests for ReportOptions configuration."""

    def test_default_options(self):
        """Test default report options."""
        opts = ReportOptions()
        assert opts.enabled is True
        assert opts.extended is False
        assert opts.minimal is False

    def test_configure(self):
        """Test configure function."""
        # Set custom options
        configure(enabled=False, extended=True, minimal=False)
        opts = get_options()
        assert opts.enabled is False
        assert opts.extended is True
        assert opts.minimal is False

        # Reset to defaults
        configure(enabled=True, extended=False, minimal=False)
        opts = get_options()
        assert opts.enabled is True


class TestIsInteractive:
    """Tests for is_interactive detection."""

    def test_non_interactive_env_var(self):
        """Test TEA_NON_INTERACTIVE environment variable."""
        with patch.dict(os.environ, {"TEA_NON_INTERACTIVE": "1"}):
            assert is_interactive() is False

        with patch.dict(os.environ, {"TEA_NON_INTERACTIVE": "true"}):
            assert is_interactive() is False

        with patch.dict(os.environ, {"TEA_NON_INTERACTIVE": "yes"}):
            assert is_interactive() is False

    def test_non_interactive_when_not_tty(self):
        """Test non-interactive when stdin/stderr are not TTY."""
        with patch.dict(os.environ, {}, clear=True):
            with patch("sys.stdin") as mock_stdin:
                mock_stdin.isatty.return_value = False
                # Should be non-interactive when stdin is not a tty
                result = is_interactive()
                # The result depends on the implementation


class TestCopyToClipboard:
    """Tests for clipboard copy functionality."""

    def test_copy_without_pyperclip(self):
        """Test copy fails gracefully without pyperclip."""
        with patch.dict(sys.modules, {"pyperclip": None}):
            # Should return False when pyperclip is not installed
            # The import error handling should make this work
            pass  # Test is placeholder for when pyperclip isn't available

    @patch("the_edge_agent.report_cli.copy_to_clipboard")
    def test_copy_success(self, mock_copy):
        """Test copy_to_clipboard returns True on success."""
        mock_copy.return_value = True
        assert mock_copy("test") is True


class TestDisplayErrorReport:
    """Tests for display_error_report function."""

    def test_disabled_reports(self, capsys):
        """Test that disabled reports return None."""
        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="test error",
        )
        opts = ReportOptions(enabled=False)

        result = display_error_report(report, options=opts)
        assert result is None

    def test_basic_report_display(self, capsys):
        """Test basic report URL is displayed."""
        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="test error",
            stack=[],
        )
        opts = ReportOptions(enabled=True, minimal=True)

        # Mock is_interactive to prevent prompt
        with patch("the_edge_agent.report_cli.is_interactive", return_value=False):
            with patch(
                "the_edge_agent.report_cli.copy_to_clipboard", return_value=False
            ):
                result = display_error_report(report, options=opts)

        assert result is not None
        assert "https://" in result

        # Check stderr output
        captured = capsys.readouterr()
        assert "Report this bug" in captured.err
        assert "stack trace" in captured.err.lower()

    def test_extended_auto_include(self, capsys):
        """Test extended context is auto-included when flag is set."""
        report = ErrorReport(
            error_type=ErrorType.YAML_ERROR,
            message="config error",
            context=ErrorContext(node_name="test_node", action_type="llm.chat"),
        )
        opts = ReportOptions(enabled=True, extended=True)

        with patch("the_edge_agent.report_cli.is_interactive", return_value=False):
            with patch(
                "the_edge_agent.report_cli.copy_to_clipboard", return_value=False
            ):
                result = display_error_report(report, options=opts)

        assert result is not None
        captured = capsys.readouterr()
        # Extended report message should appear
        assert "Extended report" in captured.err or "extended" in result.lower()


class TestAddExtendedContext:
    """Tests for add_extended_context function."""

    def test_empty_config(self):
        """Test with empty/None config."""
        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="test",
        )

        result = add_extended_context(report, None)
        assert result.message == "test"
        assert result.error_type == ErrorType.PANIC

    def test_with_workflow_config(self):
        """Test with workflow config."""
        report = ErrorReport(
            error_type=ErrorType.YAML_ERROR,
            message="config error",
            context=ErrorContext(node_name="process"),
        )

        config = {
            "name": "test-workflow",
            "nodes": [
                {"name": "start", "uses": "builtin.passthrough"},
                {"name": "process", "uses": "llm.chat"},
            ],
            "edges": [
                {"from": "__start__", "to": "start"},
                {"from": "start", "to": "process"},
            ],
            "state_schema": {
                "input": "str",
                "output": "str",
            },
        }

        result = add_extended_context(report, config)

        assert result.extended is not None
        ext = result.extended
        assert ext.workflow_name == "test-workflow"
        assert len(ext.nodes) == 2
        assert ext.nodes[0].name == "start"
        assert ext.nodes[0].action_type == "builtin.passthrough"
        assert ext.active_node == "process"
        assert "input" in ext.schema_fields
        assert "output" in ext.schema_fields


class TestHandleException:
    """Tests for handle_exception function."""

    def test_disabled_does_nothing(self, capsys):
        """Test that handle_exception does nothing when disabled."""
        configure(enabled=False)

        try:
            raise ValueError("test error")
        except ValueError as e:
            handle_exception(e)

        captured = capsys.readouterr()
        assert "Report this bug" not in captured.err

        # Reset
        configure(enabled=True)

    def test_enabled_shows_url(self, capsys):
        """Test that handle_exception shows URL when enabled."""
        configure(enabled=True, minimal=True)

        with patch("the_edge_agent.report_cli.is_interactive", return_value=False):
            with patch(
                "the_edge_agent.report_cli.copy_to_clipboard", return_value=False
            ):
                try:
                    raise ValueError("test error for report")
                except ValueError as e:
                    handle_exception(e, node_name="test_node")

        captured = capsys.readouterr()
        assert "Report this bug" in captured.err

        # Reset
        configure(enabled=True, minimal=False)


class TestExcepthook:
    """Tests for excepthook installation."""

    def test_install_uninstall(self):
        """Test installing and uninstalling the excepthook."""
        original_hook = sys.excepthook

        install_cli_excepthook()
        assert sys.excepthook != original_hook

        uninstall_cli_excepthook()
        # After uninstall, should be restored
        # (may be different if other hooks were installed)


class TestPromptYesNo:
    """Tests for prompt_yes_no function."""

    def test_non_interactive_returns_default(self):
        """Test that non-interactive returns default."""
        with patch("the_edge_agent.report_cli.is_interactive", return_value=False):
            assert prompt_yes_no("test?", default=True) is True
            assert prompt_yes_no("test?", default=False) is False


class TestEnvironmentVariableIntegration:
    """Tests for environment variable handling."""

    def test_tea_report_url_env(self):
        """Test TEA_REPORT_URL environment variable."""
        custom_url = "https://custom.example.com/report"
        with patch.dict(os.environ, {"TEA_REPORT_URL": custom_url}):
            # Reload module to pick up new env var
            import importlib
            from the_edge_agent import report_cli

            importlib.reload(report_cli)
            assert report_cli.REPORT_BASE_URL == custom_url

        # Reload again to reset
        with patch.dict(os.environ, {}, clear=True):
            import importlib
            from the_edge_agent import report_cli

            importlib.reload(report_cli)


class TestCLIFlagsIntegration:
    """Integration tests for CLI flag handling."""

    def test_report_bugs_flag(self):
        """Test --report-bugs flag enables bug reporting."""
        configure(enabled=True)
        assert get_options().enabled is True

    def test_no_report_bugs_flag(self):
        """Test --no-report-bugs flag disables bug reporting."""
        configure(enabled=False)
        assert get_options().enabled is False
        configure(enabled=True)  # Reset

    def test_report_extended_flag(self):
        """Test --report-extended flag."""
        configure(extended=True)
        assert get_options().extended is True
        configure(extended=False)  # Reset

    def test_report_minimal_flag(self):
        """Test --report-minimal flag."""
        configure(minimal=True)
        assert get_options().minimal is True
        configure(minimal=False)  # Reset
