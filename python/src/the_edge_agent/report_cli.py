"""Bug Report CLI Integration - TEA-REPORT-001d

Provides CLI integration for displaying bug report URLs on errors.
Handles extended context prompts, clipboard support, and formatting.

Usage:
    from the_edge_agent import report_cli

    # Configure from CLI flags
    report_cli.configure(enabled=True, extended=False, minimal=False)

    # Display URL on error
    report_cli.display_error_report(report)
"""

import os
import sys
from dataclasses import dataclass
from typing import Optional
from .report import (
    ErrorReport,
    ExtendedContext,
    NodeInfo,
    EdgeInfo,
    capture_exception,
)
from .report_encoder import encode_error_report, DEFAULT_BASE_URL


# Default report viewer URL
REPORT_BASE_URL = os.environ.get(
    "TEA_REPORT_URL", "https://fabceolin.github.io/the_edge_agent/report"
)


@dataclass
class ReportOptions:
    """Configuration options for bug report display."""

    enabled: bool = True
    extended: bool = False
    minimal: bool = False


# Global configuration (set by CLI)
_options = ReportOptions()


def configure(
    enabled: bool = True, extended: bool = False, minimal: bool = False
) -> None:
    """Configure bug report options from CLI flags.

    Args:
        enabled: Whether bug report URLs are enabled
        extended: Auto-include extended context (skip prompt)
        minimal: Skip extended prompt entirely (minimal report only)
    """
    global _options
    _options = ReportOptions(enabled=enabled, extended=extended, minimal=minimal)


def get_options() -> ReportOptions:
    """Get current report options."""
    return _options


def is_interactive() -> bool:
    """Check if running in an interactive terminal."""
    # Check environment variable first (for CI/testing)
    if os.environ.get("TEA_NON_INTERACTIVE", "").lower() in ("1", "true", "yes"):
        return False
    return sys.stdin.isatty() and sys.stderr.isatty()


def copy_to_clipboard(text: str) -> bool:
    """Try to copy text to clipboard.

    Returns True if successful, False otherwise.
    Silently fails if clipboard is not available.
    """
    try:
        import pyperclip

        pyperclip.copy(text)
        return True
    except ImportError:
        # pyperclip not installed
        return False
    except Exception:
        # Clipboard operation failed (headless, wayland without access, etc.)
        return False


def prompt_yes_no(prompt: str, default: bool = False, timeout: float = 10.0) -> bool:
    """Prompt user for yes/no response with timeout.

    Args:
        prompt: The prompt to display
        default: Default value if timeout or empty input
        timeout: Timeout in seconds

    Returns:
        True for yes, False for no
    """
    import select

    if not is_interactive():
        return default

    default_str = "Y/n" if default else "y/N"
    sys.stderr.write(f"{prompt} [{default_str}]: ")
    sys.stderr.flush()

    # Use select for timeout on Unix
    if hasattr(select, "select"):
        try:
            ready, _, _ = select.select([sys.stdin], [], [], timeout)
            if not ready:
                sys.stderr.write("\n")
                return default

            response = sys.stdin.readline().strip().lower()
            if not response:
                return default
            return response in ("y", "yes")
        except (OSError, ValueError):
            return default
    else:
        # Windows fallback - no timeout
        try:
            response = input().strip().lower()
            if not response:
                return default
            return response in ("y", "yes")
        except (EOFError, KeyboardInterrupt):
            return default


def display_error_report(
    report: ErrorReport,
    base_url: str = REPORT_BASE_URL,
    options: Optional[ReportOptions] = None,
) -> Optional[str]:
    """Display error report URL with nice formatting.

    Args:
        report: The ErrorReport to display
        base_url: Base URL for the report viewer
        options: Report options (uses global if None)

    Returns:
        The URL that was displayed, or None if disabled
    """
    opts = options or _options

    if not opts.enabled:
        return None

    try:
        url = encode_error_report(report, base_url)
    except Exception as e:
        # Don't let encoding errors break error reporting
        sys.stderr.write(f"\n[Bug report encoding failed: {e}]\n")
        return None

    # Display the report section
    sys.stderr.write("\n")
    sys.stderr.write("━" * 68 + "\n")
    sys.stderr.write("🐛 Report this bug:\n")
    sys.stderr.write(f"   {url}\n")
    sys.stderr.write("\n")
    sys.stderr.write("   This URL contains only: version, platform, and stack trace.\n")
    sys.stderr.write("   No personal information or file contents are included.\n")

    final_url = url

    # Handle extended context prompt
    if not opts.minimal and not opts.extended and is_interactive():
        sys.stderr.write("\n")
        if prompt_yes_no("   Include more context to help diagnose?", default=False):
            extended_report = add_extended_context(report)
            try:
                extended_url = encode_error_report(extended_report, base_url)
                display_extended_url(extended_url, extended_report)
                final_url = extended_url
            except Exception as e:
                sys.stderr.write(f"   [Extended report encoding failed: {e}]\n")
    elif opts.extended:
        # Auto-include extended context
        extended_report = add_extended_context(report)
        try:
            extended_url = encode_error_report(extended_report, base_url)
            display_extended_url(extended_url, extended_report)
            final_url = extended_url
        except Exception as e:
            sys.stderr.write(f"   [Extended report encoding failed: {e}]\n")

    sys.stderr.write("━" * 68 + "\n")

    # Try to copy to clipboard
    if copy_to_clipboard(final_url):
        sys.stderr.write("   📋 URL copied to clipboard\n")

    return final_url


def display_extended_url(url: str, report: ErrorReport) -> None:
    """Display extended report URL with details about what's included."""
    sys.stderr.write("\n")
    sys.stderr.write("🐛 Extended report (includes workflow structure):\n")
    sys.stderr.write(f"   {url}\n")
    sys.stderr.write("\n")
    sys.stderr.write("   Additional info included:\n")

    if report.extended:
        ext = report.extended
        if ext.nodes:
            node_names = [n.name for n in ext.nodes[:5]]
            if len(ext.nodes) > 5:
                node_names.append("...")
            sys.stderr.write(f"   ✓ Node names: {', '.join(node_names)}\n")
        if ext.nodes:
            action_types = list(set(n.action_type for n in ext.nodes if n.action_type))[
                :5
            ]
            if action_types:
                sys.stderr.write(f"   ✓ Action types: {', '.join(action_types)}\n")
        if ext.edges:
            sys.stderr.write(f"   ✓ Graph structure ({len(ext.edges)} edges)\n")
        if ext.schema_fields:
            sys.stderr.write(
                f"   ✓ Schema fields: {', '.join(ext.schema_fields[:5])}\n"
            )

    sys.stderr.write("\n")
    sys.stderr.write("   Still excluded: state data, secrets, prompts, file contents\n")


def add_extended_context(
    report: ErrorReport,
    workflow_config: Optional[dict] = None,
) -> ErrorReport:
    """Add extended context to a report for opt-in detailed reports.

    This function extracts workflow structure information from the provided
    config without including sensitive data (state values, secrets, prompts).

    Args:
        report: The base ErrorReport
        workflow_config: Optional YAML config dict with workflow structure

    Returns:
        A new ErrorReport with extended context added
    """
    # Create extended context with safe information only
    extended = ExtendedContext()

    # Try to get workflow info from the provided config
    if workflow_config:
        try:
            # Workflow name is safe
            extended.workflow_name = workflow_config.get("name")

            # Node names and action types (no state data)
            for node_config in workflow_config.get("nodes", []):
                node_info = NodeInfo(
                    name=node_config.get("name", "unknown"),
                    action_type=node_config.get("uses") or node_config.get("action"),
                )
                extended.nodes.append(node_info)

            # Edge structure (from/to only, no conditions)
            for edge_config in workflow_config.get("edges", []):
                from_node = edge_config.get("from", "__start__")
                to_node = edge_config.get("to")
                if to_node:
                    edge_info = EdgeInfo(
                        from_node=from_node,
                        to=to_node,
                        edge_type=(
                            "simple"
                            if not edge_config.get("targets")
                            else "conditional"
                        ),
                    )
                    extended.edges.append(edge_info)
                # Handle targets (conditional edges)
                if edge_config.get("targets"):
                    for _, target in edge_config["targets"].items():
                        extended.edges.append(
                            EdgeInfo(
                                from_node=from_node,
                                to=target,
                                edge_type="conditional",
                            )
                        )

            # Schema field names only (not values)
            schema = workflow_config.get("state_schema", {})
            if isinstance(schema, dict):
                extended.schema_fields = list(schema.keys())
        except Exception:
            # Don't fail if we can't get extended context
            pass

    # Active node/action from error context
    if report.context and report.context.node_name:
        extended.active_node = report.context.node_name
    if report.context and report.context.action_type:
        extended.active_action = report.context.action_type

    # Create new report with extended context
    return ErrorReport(
        version=report.version,
        platform=report.platform,
        runtime=report.runtime,
        error_type=report.error_type,
        message=report.message,
        stack=report.stack,
        context=report.context,
        extended=extended if extended.nodes or extended.workflow_name else None,
    )


def handle_exception(
    exc: BaseException,
    node_name: Optional[str] = None,
    action_type: Optional[str] = None,
) -> None:
    """Handle an exception and display bug report URL if enabled.

    Args:
        exc: The exception to handle
        node_name: Optional node name for context
        action_type: Optional action type for context
    """
    if not _options.enabled:
        return

    # Capture the error
    from .report import ErrorType

    report = capture_exception(
        exc,
        error_type=ErrorType.PANIC,
        node_name=node_name,
        action_type=action_type,
    )

    # Display the report URL
    display_error_report(report)


# Enhanced excepthook that shows bug report URL
_original_excepthook = None


def _tea_cli_excepthook(exc_type, exc_value, exc_tb):
    """Custom excepthook that displays bug report URL."""
    from .report import capture_exception, ErrorType

    # Show normal traceback first
    if _original_excepthook:
        _original_excepthook(exc_type, exc_value, exc_tb)

    # Then show bug report URL if enabled
    if _options.enabled:
        report = capture_exception(exc_value, error_type=ErrorType.PANIC)
        display_error_report(report)


def install_cli_excepthook() -> None:
    """Install the CLI excepthook that shows bug report URLs.

    This enhances the basic excepthook from report.py to show
    the formatted bug report URL.
    """
    global _original_excepthook

    if _original_excepthook is not None:
        return  # Already installed

    _original_excepthook = sys.excepthook
    sys.excepthook = _tea_cli_excepthook


def uninstall_cli_excepthook() -> None:
    """Restore the original excepthook."""
    global _original_excepthook

    if _original_excepthook is not None:
        sys.excepthook = _original_excepthook
        _original_excepthook = None
