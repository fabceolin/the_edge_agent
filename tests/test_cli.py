"""
Unit tests for the CLI module.

Tests the tea-agent command-line interface for argument parsing, error handling,
and integration with YAMLEngine.
"""

import unittest
import sys
import json
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock, call
from io import StringIO

# Import the CLI module
from the_edge_agent.cli import (
    parse_args,
    load_initial_state,
    load_actions_from_module,
    load_actions_from_file,
    load_cli_actions,
    run_agent,
    main,
    load_checkpoint,
    deep_merge,
    is_interactive_terminal,
    handle_interrupt_interactive,
)


class TestParseArgs(unittest.TestCase):
    """Test argument parsing logic."""

    def test_parse_args_yaml_file_only(self):
        """Test parsing with only a YAML file argument."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml"]):
            args = parse_args()
            self.assertEqual(args.yaml_file, "test.yaml")
            self.assertIsNone(args.state)
            self.assertIsNone(args.state_file)

    def test_parse_args_with_state(self):
        """Test parsing with --state flag."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--state", '{"key": "value"}']):
            args = parse_args()
            self.assertEqual(args.yaml_file, "test.yaml")
            self.assertEqual(args.state, '{"key": "value"}')
            self.assertIsNone(args.state_file)

    def test_parse_args_with_state_file(self):
        """Test parsing with --state-file flag."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--state-file", "state.json"]):
            args = parse_args()
            self.assertEqual(args.yaml_file, "test.yaml")
            self.assertIsNone(args.state)
            self.assertEqual(args.state_file, "state.json")

    def test_parse_args_with_both_state_flags(self):
        """Test parsing with both --state and --state-file flags."""
        with patch.object(
            sys,
            "argv",
            ["tea-agent", "test.yaml", "--state", '{"key1": "value1"}', "--state-file", "state.json"],
        ):
            args = parse_args()
            self.assertEqual(args.yaml_file, "test.yaml")
            self.assertEqual(args.state, '{"key1": "value1"}')
            self.assertEqual(args.state_file, "state.json")

    def test_parse_args_with_actions_module(self):
        """Test parsing with --actions-module flag."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--actions-module", "my_package.actions"]):
            args = parse_args()
            self.assertEqual(args.yaml_file, "test.yaml")
            self.assertEqual(args.actions_modules, ["my_package.actions"])

    def test_parse_args_with_actions_file(self):
        """Test parsing with --actions-file flag."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--actions-file", "./my_actions.py"]):
            args = parse_args()
            self.assertEqual(args.yaml_file, "test.yaml")
            self.assertEqual(args.actions_files, ["./my_actions.py"])

    def test_parse_args_with_multiple_actions_modules(self):
        """Test parsing with multiple --actions-module flags."""
        with patch.object(
            sys,
            "argv",
            ["tea-agent", "test.yaml", "--actions-module", "pkg1.actions", "--actions-module", "pkg2.actions"],
        ):
            args = parse_args()
            self.assertEqual(args.actions_modules, ["pkg1.actions", "pkg2.actions"])

    def test_parse_args_with_multiple_actions_files(self):
        """Test parsing with multiple --actions-file flags."""
        with patch.object(
            sys,
            "argv",
            ["tea-agent", "test.yaml", "--actions-file", "./actions1.py", "--actions-file", "./actions2.py"],
        ):
            args = parse_args()
            self.assertEqual(args.actions_files, ["./actions1.py", "./actions2.py"])

    def test_parse_args_with_resume(self):
        """Test parsing with --resume flag."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--resume", "./checkpoints/node_123.pkl"]):
            args = parse_args()
            self.assertEqual(args.resume, "./checkpoints/node_123.pkl")

    def test_parse_args_with_auto_continue(self):
        """Test parsing with --auto-continue flag."""
        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--auto-continue"]):
            args = parse_args()
            self.assertTrue(args.auto_continue)

    def test_parse_args_with_resume_and_auto_continue(self):
        """Test parsing with both --resume and --auto-continue flags."""
        with patch.object(
            sys, "argv", ["tea-agent", "test.yaml", "--resume", "./checkpoints/test.pkl", "--auto-continue"]
        ):
            args = parse_args()
            self.assertEqual(args.resume, "./checkpoints/test.pkl")
            self.assertTrue(args.auto_continue)

    def test_parse_args_with_mixed_actions_flags(self):
        """Test parsing with both --actions-module and --actions-file flags."""
        with patch.object(
            sys,
            "argv",
            ["tea-agent", "test.yaml", "--actions-module", "pkg.actions", "--actions-file", "./actions.py"],
        ):
            args = parse_args()
            self.assertEqual(args.actions_modules, ["pkg.actions"])
            self.assertEqual(args.actions_files, ["./actions.py"])


class TestLoadInitialState(unittest.TestCase):
    """Test initial state loading logic."""

    def setUp(self):
        """Create mock args object."""
        self.args = MagicMock()
        self.args.state = None
        self.args.state_file = None

    def test_load_empty_state(self):
        """Test loading with no state provided."""
        state = load_initial_state(self.args)
        self.assertEqual(state, {})

    def test_load_state_from_arg(self):
        """Test loading state from --state argument."""
        self.args.state = '{"query": "test", "count": 5}'
        state = load_initial_state(self.args)
        self.assertEqual(state, {"query": "test", "count": 5})

    def test_load_state_from_file(self):
        """Test loading state from --state-file."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"key": "value"}, f)
            temp_file = f.name

        try:
            self.args.state_file = temp_file
            state = load_initial_state(self.args)
            self.assertEqual(state, {"key": "value"})
        finally:
            Path(temp_file).unlink()

    def test_load_state_merge(self):
        """Test that --state overrides --state-file values."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"key1": "file_value", "key2": "file_only"}, f)
            temp_file = f.name

        try:
            self.args.state_file = temp_file
            self.args.state = '{"key1": "arg_value", "key3": "arg_only"}'
            state = load_initial_state(self.args)
            self.assertEqual(
                state,
                {"key1": "arg_value", "key2": "file_only", "key3": "arg_only"},
            )
        finally:
            Path(temp_file).unlink()

    def test_invalid_json_in_state_arg(self):
        """Test that invalid JSON in --state exits with error."""
        self.args.state = "{invalid json}"
        with self.assertRaises(SystemExit):
            load_initial_state(self.args)

    def test_invalid_json_in_state_file(self):
        """Test that invalid JSON in state file exits with error."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            f.write("{invalid json}")
            temp_file = f.name

        try:
            self.args.state_file = temp_file
            with self.assertRaises(SystemExit):
                load_initial_state(self.args)
        finally:
            Path(temp_file).unlink()

    def test_missing_state_file(self):
        """Test that missing state file exits with error."""
        self.args.state_file = "/nonexistent/path/state.json"
        with self.assertRaises(SystemExit):
            load_initial_state(self.args)

    def test_non_dict_state_in_arg(self):
        """Test that non-dict JSON in --state exits with error."""
        self.args.state = '["not", "a", "dict"]'
        with self.assertRaises(SystemExit):
            load_initial_state(self.args)

    def test_non_dict_state_in_file(self):
        """Test that non-dict JSON in state file exits with error."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump(["not", "a", "dict"], f)
            temp_file = f.name

        try:
            self.args.state_file = temp_file
            with self.assertRaises(SystemExit):
                load_initial_state(self.args)
        finally:
            Path(temp_file).unlink()


class TestRunAgent(unittest.TestCase):
    """Test agent execution logic."""

    def test_missing_yaml_file(self):
        """Test that missing YAML file returns error code 1."""
        exit_code = run_agent("/nonexistent/file.yaml", {})
        self.assertEqual(exit_code, 1)

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_successful_execution(self, mock_engine_class):
        """Test successful agent execution."""
        # Create a temporary YAML file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            # Mock the graph stream to yield a final event
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [{"type": "final", "state": {"result": "success"}}]

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            exit_code = run_agent(temp_yaml, {"query": "test"})
            self.assertEqual(exit_code, 0)
            mock_engine.load_from_file.assert_called_once_with(temp_yaml)
            mock_graph.stream.assert_called_once_with({"query": "test"}, checkpoint=None)
        finally:
            Path(temp_yaml).unlink()

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_execution_with_error_event(self, mock_engine_class):
        """Test that error events return exit code 1."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [
                {"type": "error", "node": "test_node", "error": "Test error"}
            ]

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            exit_code = run_agent(temp_yaml, {})
            self.assertEqual(exit_code, 1)
        finally:
            Path(temp_yaml).unlink()

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_keyboard_interrupt(self, mock_engine_class):
        """Test that Ctrl+C returns exit code 130."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            mock_graph = MagicMock()
            mock_graph.stream.side_effect = KeyboardInterrupt()

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            exit_code = run_agent(temp_yaml, {})
            self.assertEqual(exit_code, 130)
        finally:
            Path(temp_yaml).unlink()


class TestLoadActionsFromModule(unittest.TestCase):
    """Test loading actions from Python modules."""

    def test_load_from_sample_actions_module(self):
        """Test loading actions from the sample test fixture module."""
        # Load from the fixtures.sample_actions module
        registry = load_actions_from_module("tests.fixtures.sample_actions")

        self.assertIn("test_action", registry)
        self.assertIn("another_action", registry)
        self.assertIn("greet_action", registry)
        self.assertTrue(callable(registry["test_action"]))
        self.assertTrue(callable(registry["another_action"]))
        self.assertTrue(callable(registry["greet_action"]))

        # Test that actions work
        result = registry["test_action"]({})
        self.assertEqual(result, {"test": "success"})

        result = registry["another_action"]({}, value=5)
        self.assertEqual(result, {"result": 10})

        result = registry["greet_action"]({}, name="Alice")
        self.assertEqual(result, {"greeting": "Hello, Alice!"})

    def test_load_from_nonexistent_module(self):
        """Test that loading from nonexistent module raises ImportError."""
        with self.assertRaises(ImportError):
            load_actions_from_module("nonexistent.module.path")

    def test_load_from_module_without_register_actions(self):
        """Test that module without register_actions() raises AttributeError."""
        with self.assertRaises(AttributeError):
            load_actions_from_module("json")  # stdlib module without register_actions

    def test_load_from_module_with_invalid_action(self):
        """Test that registering non-callable raises ValueError."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create a module that registers a non-callable
            module_path = Path(temp_dir) / "bad_actions.py"
            module_path.write_text("""
def register_actions(registry, engine):
    registry['not_a_function'] = 'this is a string, not a callable'
""")

            # Add temp_dir to sys.path temporarily
            sys.path.insert(0, temp_dir)
            try:
                with self.assertRaises(ValueError):
                    import importlib
                    import bad_actions
                    # Reload to ensure fresh import
                    importlib.reload(bad_actions)
                    registry = {}
                    bad_actions.register_actions(registry, None)
                    # This should raise ValueError
                    for action_name, action_func in registry.items():
                        if not callable(action_func):
                            raise ValueError(f"Non-callable: {action_name}")
            finally:
                sys.path.remove(temp_dir)


class TestLoadActionsFromFile(unittest.TestCase):
    """Test loading actions from Python files."""

    def test_load_from_sample_actions_file(self):
        """Test loading actions from the sample test fixture file."""
        fixture_path = Path(__file__).parent / "fixtures" / "sample_actions.py"
        registry = load_actions_from_file(str(fixture_path))

        self.assertIn("test_action", registry)
        self.assertIn("another_action", registry)
        self.assertIn("greet_action", registry)
        self.assertTrue(callable(registry["test_action"]))

        # Test that actions work
        result = registry["test_action"]({})
        self.assertEqual(result, {"test": "success"})

    def test_load_from_nonexistent_file(self):
        """Test that loading from nonexistent file raises FileNotFoundError."""
        with self.assertRaises(FileNotFoundError):
            load_actions_from_file("/nonexistent/path/actions.py")

    def test_load_from_file_without_register_actions(self):
        """Test that file without register_actions() raises AttributeError."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("# Empty Python file without register_actions\n")
            temp_file = f.name

        try:
            with self.assertRaises(AttributeError):
                load_actions_from_file(temp_file)
        finally:
            Path(temp_file).unlink()

    def test_load_from_file_with_syntax_error(self):
        """Test that file with syntax error raises exception."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("def register_actions(registry, engine):\n    syntax error here\n")
            temp_file = f.name

        try:
            with self.assertRaises(Exception):  # SyntaxError or similar
                load_actions_from_file(temp_file)
        finally:
            Path(temp_file).unlink()

    def test_load_from_file_with_invalid_action(self):
        """Test that registering non-callable raises ValueError."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""
def register_actions(registry, engine):
    registry['bad_action'] = 12345  # Not a callable
""")
            temp_file = f.name

        try:
            with self.assertRaises(ValueError):
                load_actions_from_file(temp_file)
        finally:
            Path(temp_file).unlink()


class TestLoadCliActions(unittest.TestCase):
    """Test the load_cli_actions() function that merges modules and files."""

    def test_load_no_actions(self):
        """Test that load_cli_actions with no arguments returns empty dict."""
        registry = load_cli_actions(actions_modules=None, actions_files=None)
        self.assertEqual(registry, {})

    def test_load_single_module(self):
        """Test loading a single module."""
        registry = load_cli_actions(actions_modules=["tests.fixtures.sample_actions"])
        self.assertIn("test_action", registry)
        self.assertEqual(len(registry), 3)  # test_action, another_action, greet_action

    def test_load_single_file(self):
        """Test loading a single file."""
        fixture_path = Path(__file__).parent / "fixtures" / "sample_actions.py"
        registry = load_cli_actions(actions_files=[str(fixture_path)])
        self.assertIn("test_action", registry)
        self.assertEqual(len(registry), 3)

    def test_load_multiple_modules(self):
        """Test loading multiple modules (later overrides earlier)."""
        # Create a second test module with overlapping action names
        with tempfile.TemporaryDirectory() as temp_dir:
            module_path = Path(temp_dir) / "override_actions.py"
            module_path.write_text("""
def register_actions(registry, engine):
    registry['test_action'] = lambda state, **kwargs: {"overridden": True}
    registry['new_action'] = lambda state, **kwargs: {"new": True}
""")
            sys.path.insert(0, temp_dir)
            try:
                # Capture stderr to check for override warnings
                with patch('sys.stderr', new_callable=StringIO):
                    registry = load_cli_actions(
                        actions_modules=["tests.fixtures.sample_actions", "override_actions"]
                    )

                # Check that override happened
                self.assertIn("test_action", registry)
                self.assertIn("new_action", registry)
                result = registry["test_action"]({})
                self.assertEqual(result, {"overridden": True})
            finally:
                sys.path.remove(temp_dir)

    def test_load_module_and_file(self):
        """Test loading both module and file (file overrides module)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""
def register_actions(registry, engine):
    registry['test_action'] = lambda state, **kwargs: {"from_file": True}
    registry['file_only'] = lambda state, **kwargs: {"file": True}
""")
            temp_file = f.name

        try:
            with patch('sys.stderr', new_callable=StringIO):
                registry = load_cli_actions(
                    actions_modules=["tests.fixtures.sample_actions"],
                    actions_files=[temp_file]
                )

            # File should override module
            self.assertIn("test_action", registry)
            self.assertIn("file_only", registry)
            result = registry["test_action"]({})
            self.assertEqual(result, {"from_file": True})
        finally:
            Path(temp_file).unlink()

    def test_load_invalid_module_exits(self):
        """Test that invalid module causes sys.exit(1)."""
        with self.assertRaises(SystemExit) as cm:
            load_cli_actions(actions_modules=["nonexistent.module"])
        self.assertEqual(cm.exception.code, 1)

    def test_load_invalid_file_exits(self):
        """Test that invalid file causes sys.exit(1)."""
        with self.assertRaises(SystemExit) as cm:
            load_cli_actions(actions_files=["/nonexistent/file.py"])
        self.assertEqual(cm.exception.code, 1)


class TestRunAgentWithActions(unittest.TestCase):
    """Test run_agent with CLI actions registry."""

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_run_agent_with_cli_actions(self, mock_engine_class):
        """Test that cli_actions are passed to YAMLEngine constructor."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [{"type": "final", "state": {}}]

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            cli_actions = {"my_action": lambda state, **kwargs: {"result": True}}
            exit_code = run_agent(temp_yaml, {}, cli_actions=cli_actions)

            self.assertEqual(exit_code, 0)
            # Verify YAMLEngine was initialized with cli_actions
            mock_engine_class.assert_called_once_with(actions_registry=cli_actions)
        finally:
            Path(temp_yaml).unlink()

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_run_agent_without_cli_actions(self, mock_engine_class):
        """Test that run_agent works without cli_actions (backward compatibility)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [{"type": "final", "state": {}}]

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            exit_code = run_agent(temp_yaml, {})

            self.assertEqual(exit_code, 0)
            # Verify YAMLEngine was initialized with empty dict
            mock_engine_class.assert_called_once_with(actions_registry={})
        finally:
            Path(temp_yaml).unlink()


class TestMainIntegration(unittest.TestCase):
    """Integration tests for the main() entry point."""

    @patch("the_edge_agent.cli.run_agent")
    @patch("the_edge_agent.cli.load_cli_actions")
    @patch("the_edge_agent.cli.load_initial_state")
    def test_main_integration(self, mock_load_state, mock_load_actions, mock_run_agent):
        """Test main() integrates parse_args, load_state, load_cli_actions, and run_agent."""
        mock_load_state.return_value = {"key": "value"}
        mock_load_actions.return_value = {}
        mock_run_agent.return_value = 0

        with patch.object(sys, "argv", ["tea-agent", "test.yaml", "--state", '{"key": "value"}']):
            exit_code = main()

        self.assertEqual(exit_code, 0)
        mock_load_state.assert_called_once()
        mock_load_actions.assert_called_once()
        mock_run_agent.assert_called_once_with(
            "test.yaml", {"key": "value"}, cli_actions={}, resume_checkpoint=None, auto_continue=False
        )

    @patch("the_edge_agent.cli.run_agent")
    @patch("the_edge_agent.cli.load_cli_actions")
    @patch("the_edge_agent.cli.load_initial_state")
    def test_main_integration_with_actions(self, mock_load_state, mock_load_actions, mock_run_agent):
        """Test main() with --actions-module and --actions-file flags."""
        mock_load_state.return_value = {}
        test_actions = {"my_action": lambda state, **kwargs: {}}
        mock_load_actions.return_value = test_actions
        mock_run_agent.return_value = 0

        with patch.object(
            sys,
            "argv",
            ["tea-agent", "test.yaml", "--actions-module", "pkg.actions", "--actions-file", "./actions.py"],
        ):
            exit_code = main()

        self.assertEqual(exit_code, 0)
        mock_load_actions.assert_called_once_with(
            actions_modules=["pkg.actions"],
            actions_files=["./actions.py"]
        )
        mock_run_agent.assert_called_once_with(
            "test.yaml", {}, cli_actions=test_actions, resume_checkpoint=None, auto_continue=False
        )


class TestDeepMerge(unittest.TestCase):
    """Test deep merge utility function."""

    def test_deep_merge_simple(self):
        """Test simple deep merge."""
        base = {"a": 1, "b": 2}
        override = {"b": 3, "c": 4}
        result = deep_merge(base, override)
        self.assertEqual(result, {"a": 1, "b": 3, "c": 4})

    def test_deep_merge_nested(self):
        """Test deep merge with nested dicts (DATA-001 risk mitigation)."""
        base = {"a": {"x": 1, "y": 2}, "b": 3}
        override = {"a": {"y": 20, "z": 30}}
        result = deep_merge(base, override)
        self.assertEqual(result, {"a": {"x": 1, "y": 20, "z": 30}, "b": 3})

    def test_deep_merge_none_value(self):
        """Test deep merge preserves explicit None values."""
        base = {"a": 1, "b": 2}
        override = {"b": None}
        result = deep_merge(base, override)
        self.assertEqual(result, {"a": 1, "b": None})

    def test_deep_merge_empty_override(self):
        """Test deep merge with empty override dict."""
        base = {"a": 1, "b": 2}
        override = {}
        result = deep_merge(base, override)
        self.assertEqual(result, {"a": 1, "b": 2})


class TestLoadCheckpoint(unittest.TestCase):
    """Test checkpoint loading functionality."""

    def test_load_checkpoint_valid(self):
        """Test loading a valid checkpoint file."""
        import pickle

        with tempfile.TemporaryDirectory() as tmpdir:
            checkpoint_path = Path(tmpdir) / "test.pkl"
            checkpoint_data = {"state": {"value": 42}, "node": "test_node", "version": "1.0"}

            with open(checkpoint_path, "wb") as f:
                pickle.dump(checkpoint_data, f, protocol=4)

            loaded = load_checkpoint(str(checkpoint_path))
            self.assertEqual(loaded["state"], {"value": 42})
            self.assertEqual(loaded["node"], "test_node")

    def test_load_checkpoint_missing_file(self):
        """Test loading a non-existent checkpoint file."""
        with self.assertRaises(FileNotFoundError) as cm:
            load_checkpoint("/nonexistent/checkpoint.pkl")
        self.assertIn("not found", str(cm.exception))

    def test_load_checkpoint_invalid_format(self):
        """Test loading a checkpoint with invalid format."""
        import pickle

        with tempfile.TemporaryDirectory() as tmpdir:
            checkpoint_path = Path(tmpdir) / "invalid.pkl"

            # Save invalid data (missing 'state' key)
            with open(checkpoint_path, "wb") as f:
                pickle.dump({"node": "test"}, f)

            with self.assertRaises(ValueError) as cm:
                load_checkpoint(str(checkpoint_path))
            self.assertIn("Invalid checkpoint format", str(cm.exception))

    def test_load_checkpoint_corrupted(self):
        """Test loading a corrupted checkpoint file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            checkpoint_path = Path(tmpdir) / "corrupted.pkl"

            # Write corrupted data
            with open(checkpoint_path, "wb") as f:
                f.write(b"corrupted data")

            with self.assertRaises(Exception):  # pickle.UnpicklingError
                load_checkpoint(str(checkpoint_path))


class TestIsInteractiveTerminal(unittest.TestCase):
    """Test TTY detection (TECH-003 risk mitigation)."""

    @patch("sys.stdin")
    def test_is_interactive_terminal_tty(self, mock_stdin):
        """Test TTY detection when stdin is a TTY."""
        mock_stdin.isatty.return_value = True
        self.assertTrue(is_interactive_terminal())

    @patch("sys.stdin")
    def test_is_interactive_terminal_non_tty(self, mock_stdin):
        """Test TTY detection when stdin is not a TTY (Docker/CI)."""
        mock_stdin.isatty.return_value = False
        self.assertFalse(is_interactive_terminal())


class TestHandleInterruptInteractive(unittest.TestCase):
    """Test interactive interrupt handling."""

    @patch("builtins.input", side_effect=["c"])
    def test_handle_interrupt_continue(self, mock_input):
        """Test interactive prompt - continue option."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertEqual(result, {"value": 42})
            # Verify checkpoint was saved
            checkpoints = list(Path(tmpdir).glob("test_node_*.pkl"))
            self.assertEqual(len(checkpoints), 1)

    @patch("builtins.input", side_effect=["u", '{"approved": true}'])
    def test_handle_interrupt_update_state(self, mock_input):
        """Test interactive prompt - update state option."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertEqual(result, {"value": 42, "approved": True})

    @patch("builtins.input", side_effect=["u", ""])
    def test_handle_interrupt_update_empty(self, mock_input):
        """Test interactive prompt - update with empty input (skip)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertEqual(result, {"value": 42})

    @patch("builtins.input", side_effect=["a"])
    def test_handle_interrupt_abort(self, mock_input):
        """Test interactive prompt - abort option."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertIsNone(result)

    @patch("builtins.input", side_effect=["u", "invalid json"])
    def test_handle_interrupt_invalid_json(self, mock_input):
        """Test interactive prompt - invalid JSON input."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertIsNone(result)

    @patch("builtins.input", side_effect=["x"])
    def test_handle_interrupt_invalid_choice(self, mock_input):
        """Test interactive prompt - invalid menu choice."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertIsNone(result)


class TestRunAgentWithInterrupts(unittest.TestCase):
    """Integration tests for run_agent with interrupt support."""

    def test_run_agent_with_resume(self):
        """Test run_agent with --resume flag."""
        import pickle

        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a checkpoint file
            checkpoint_path = Path(tmpdir) / "test.pkl"
            checkpoint_data = {"state": {"value": 10}, "node": "step1"}

            with open(checkpoint_path, "wb") as f:
                pickle.dump(checkpoint_data, f, protocol=4)

            # Create a simple YAML file
            yaml_path = Path(tmpdir) / "test.yaml"
            yaml_path.write_text("""
name: test
nodes:
  - name: step1
    run: 'return {"value": state["value"] + 1}'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
""")

            # Test resume with state merge
            exit_code = run_agent(
                str(yaml_path),
                initial_state={"extra": "data"},
                resume_checkpoint=str(checkpoint_path),
                auto_continue=True
            )

            self.assertEqual(exit_code, 0)


if __name__ == "__main__":
    unittest.main()
