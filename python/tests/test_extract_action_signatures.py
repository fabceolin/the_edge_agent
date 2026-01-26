"""
Tests for the action signature extraction script.

Tests cover:
- AST parsing of action registration patterns
- Parameter extraction from function signatures
- JSON/YAML output generation
- Validation against documentation
"""

import json
import sys
import tempfile
import unittest
from pathlib import Path
from textwrap import dedent

# Add scripts directory to path for imports
scripts_dir = Path(__file__).parent.parent.parent / "scripts"
sys.path.insert(0, str(scripts_dir))

from extract_action_signatures import (
    ActionExtractor,
    ActionSignature,
    ModuleInventory,
    ActionInventory,
    Parameter,
    ValidationResult,
    extract_signatures,
    extract_module_actions,
    extract_all_actions,
    inventory_to_dict,
    parse_documented_actions,
    validate_against_docs,
    get_actions_dir,
)


class TestActionExtractor(unittest.TestCase):
    """Test the AST-based action extraction."""

    def test_extract_simple_registration(self):
        """Test extraction of registry['action'] = func pattern."""
        source = dedent(
            '''
            def my_action(state, param1: str, param2: int = 10):
                """Do something."""
                return {"result": "ok"}

            def register_actions(registry, engine):
                registry["test.action"] = my_action
        '''
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 1)
        action = actions[0]
        self.assertEqual(action.name, "test.action")
        self.assertEqual(action.function, "my_action")
        self.assertEqual(action.docstring, "Do something.")
        self.assertEqual(len(action.parameters), 2)
        self.assertEqual(action.parameters[0].name, "param1")
        self.assertEqual(action.parameters[0].type, "str")
        self.assertTrue(action.parameters[0].required)
        self.assertEqual(action.parameters[1].name, "param2")
        self.assertEqual(action.parameters[1].type, "int")
        self.assertFalse(action.parameters[1].required)
        self.assertEqual(action.parameters[1].default, "10")

    def test_extract_wrapped_registration(self):
        """Test extraction of registry['action'] = wrapper(func) pattern."""
        source = dedent(
            '''
            def wrap(fn):
                return fn

            def wrapped_action(state, data: dict):
                """Wrapped action."""
                return data

            def register_actions(registry, engine):
                registry["test.wrapped"] = wrap(wrapped_action)
        '''
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 1)
        action = actions[0]
        self.assertEqual(action.name, "test.wrapped")
        self.assertEqual(action.function, "wrapped_action")

    def test_extract_multiple_registrations(self):
        """Test extraction of multiple actions from one module."""
        source = dedent(
            """
            def action_one(state):
                return {}

            def action_two(state, x: int):
                return {}

            def register_actions(registry, engine):
                registry["ns.one"] = action_one
                registry["ns.two"] = action_two
        """
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 2)
        names = {a.name for a in actions}
        self.assertEqual(names, {"ns.one", "ns.two"})

    def test_skip_private_actions(self):
        """Test that actions starting with _ are skipped."""
        source = dedent(
            """
            def private_action(state):
                return {}

            def register_actions(registry, engine):
                registry["_private.action"] = private_action
        """
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 0)

    def test_extract_lambda_registration(self):
        """Test extraction of lambda registrations."""
        source = dedent(
            """
            def register_actions(registry, engine):
                registry["test.lambda"] = lambda state, x: {"x": x}
        """
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 1)
        self.assertEqual(actions[0].function, "<lambda>")

    def test_extract_optional_type_hints(self):
        """Test extraction of Optional type hints."""
        source = dedent(
            """
            from typing import Optional, Dict, Any

            def action_with_optional(
                state,
                required: str,
                optional: Optional[str] = None,
                default_val: int = 42
            ):
                return {}

            def register_actions(registry, engine):
                registry["test.optional"] = action_with_optional
        """
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 1)
        params = actions[0].parameters

        self.assertEqual(len(params), 3)
        self.assertEqual(params[0].name, "required")
        self.assertTrue(params[0].required)

        self.assertEqual(params[1].name, "optional")
        self.assertFalse(params[1].required)
        self.assertEqual(params[1].default, "None")

        self.assertEqual(params[2].name, "default_val")
        self.assertFalse(params[2].required)
        self.assertEqual(params[2].default, "42")

    def test_extract_kwargs(self):
        """Test extraction of **kwargs parameter."""
        source = dedent(
            """
            def action_with_kwargs(state, name: str, **kwargs):
                return {}

            def register_actions(registry, engine):
                registry["test.kwargs"] = action_with_kwargs
        """
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 1)
        params = actions[0].parameters

        self.assertEqual(len(params), 2)
        self.assertEqual(params[0].name, "name")
        self.assertEqual(params[1].name, "**kwargs")
        self.assertFalse(params[1].required)

    def test_no_register_actions_function(self):
        """Test handling of modules without register_actions."""
        source = dedent(
            """
            def some_helper():
                pass
        """
        )

        extractor = ActionExtractor(source, "test_module.py")
        actions = extractor.extract()

        self.assertEqual(len(actions), 0)


class TestInventorySerialization(unittest.TestCase):
    """Test inventory serialization to dict/JSON/YAML."""

    def test_inventory_to_dict(self):
        """Test conversion of inventory to dictionary."""
        inventory = ActionInventory(
            generated_at="2026-01-25T12:00:00Z",
            git_commit="abc123",
            total_actions=1,
            modules=[
                ModuleInventory(
                    file="test_actions.py",
                    namespace="test",
                    actions=[
                        ActionSignature(
                            name="test.action",
                            function="test_func",
                            parameters=[
                                Parameter(
                                    name="param1",
                                    type="str",
                                    required=True,
                                ),
                            ],
                            return_type="dict",
                            docstring="Test docstring",
                            line_number=10,
                        ),
                    ],
                ),
            ],
        )

        result = inventory_to_dict(inventory)

        self.assertEqual(result["generated_at"], "2026-01-25T12:00:00Z")
        self.assertEqual(result["git_commit"], "abc123")
        self.assertEqual(result["total_actions"], 1)
        self.assertEqual(len(result["modules"]), 1)

        module = result["modules"][0]
        self.assertEqual(module["file"], "test_actions.py")
        self.assertEqual(module["namespace"], "test")
        self.assertEqual(len(module["actions"]), 1)

        action = module["actions"][0]
        self.assertEqual(action["name"], "test.action")
        self.assertEqual(action["function"], "test_func")
        self.assertEqual(action["docstring"], "Test docstring")
        self.assertEqual(len(action["parameters"]), 1)

    def test_inventory_json_serializable(self):
        """Test that inventory dict is JSON serializable."""
        inventory = ActionInventory(
            generated_at="2026-01-25T12:00:00Z",
            git_commit="abc123",
            total_actions=0,
            modules=[],
        )

        result = inventory_to_dict(inventory)
        json_str = json.dumps(result)

        self.assertIsInstance(json_str, str)
        parsed = json.loads(json_str)
        self.assertEqual(parsed["git_commit"], "abc123")


class TestDocumentationParsing(unittest.TestCase):
    """Test parsing of documented actions from markdown."""

    def test_parse_table_pattern(self):
        """Test parsing actions from markdown tables."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                dedent(
                    """
                # Actions Reference

                | Action | Description |
                |--------|-------------|
                | `llm.call` | Call LLM |
                | `http.get` | HTTP GET |
            """
                )
            )
            f.flush()

            documented = parse_documented_actions(Path(f.name))

        self.assertIn("llm.call", documented)
        self.assertIn("http.get", documented)

    def test_parse_uses_pattern(self):
        """Test parsing actions from YAML uses: syntax."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                dedent(
                    """
                ```yaml
                - name: call_llm
                  uses: llm.call
                  with:
                    model: gpt-4
                ```
            """
                )
            )
            f.flush()

            documented = parse_documented_actions(Path(f.name))

        self.assertIn("llm.call", documented)

    def test_parse_registry_pattern(self):
        """Test parsing actions from registry['...'] examples."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                dedent(
                    """
                ```python
                result = registry['memory.store'](state, key="test", value="data")
                ```
            """
                )
            )
            f.flush()

            documented = parse_documented_actions(Path(f.name))

        self.assertIn("memory.store", documented)


class TestValidation(unittest.TestCase):
    """Test validation of inventory against documentation."""

    def test_validation_identifies_gaps(self):
        """Test that validation identifies undocumented actions."""
        # Create a minimal inventory with an action
        inventory = ActionInventory(
            generated_at="2026-01-25T12:00:00Z",
            git_commit="abc123",
            total_actions=1,
            modules=[
                ModuleInventory(
                    file="test_actions.py",
                    namespace="test",
                    actions=[
                        ActionSignature(
                            name="test.undocumented",
                            function="test_func",
                            parameters=[],
                            line_number=1,
                        ),
                    ],
                ),
            ],
        )

        # Create temp docs with no actions documented
        with tempfile.TemporaryDirectory() as tmpdir:
            docs_dir = Path(tmpdir)
            python_dir = docs_dir / "python"
            python_dir.mkdir()
            shared_dir = docs_dir / "shared"
            shared_dir.mkdir()

            (python_dir / "actions-reference.md").write_text("# No actions here")
            (shared_dir / "YAML_REFERENCE.md").write_text("# Also empty")

            result = validate_against_docs(inventory, docs_dir)

        self.assertEqual(result.gaps, 1)
        self.assertEqual(len(result.undocumented), 1)
        self.assertEqual(result.undocumented[0]["action"], "test.undocumented")

    def test_validation_identifies_hallucinations(self):
        """Test that validation identifies documented-but-not-implemented actions."""
        # Empty inventory
        inventory = ActionInventory(
            generated_at="2026-01-25T12:00:00Z",
            git_commit="abc123",
            total_actions=0,
            modules=[],
        )

        # Create temp docs with a documented action
        with tempfile.TemporaryDirectory() as tmpdir:
            docs_dir = Path(tmpdir)
            python_dir = docs_dir / "python"
            python_dir.mkdir()
            shared_dir = docs_dir / "shared"
            shared_dir.mkdir()

            (python_dir / "actions-reference.md").write_text(
                "| `fake.action` | Does nothing |"
            )
            (shared_dir / "YAML_REFERENCE.md").write_text("")

            result = validate_against_docs(inventory, docs_dir)

        self.assertEqual(result.hallucinations, 1)
        self.assertEqual(len(result.hallucinated), 1)
        self.assertEqual(result.hallucinated[0]["action"], "fake.action")

    def test_coverage_calculation(self):
        """Test that coverage percentage is calculated correctly."""
        # 2 actions implemented, 1 documented
        inventory = ActionInventory(
            generated_at="2026-01-25T12:00:00Z",
            git_commit="abc123",
            total_actions=2,
            modules=[
                ModuleInventory(
                    file="test_actions.py",
                    namespace="test",
                    actions=[
                        ActionSignature(
                            name="test.documented",
                            function="f1",
                            parameters=[],
                            line_number=1,
                        ),
                        ActionSignature(
                            name="test.undocumented",
                            function="f2",
                            parameters=[],
                            line_number=2,
                        ),
                    ],
                ),
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            docs_dir = Path(tmpdir)
            python_dir = docs_dir / "python"
            python_dir.mkdir()
            shared_dir = docs_dir / "shared"
            shared_dir.mkdir()

            (python_dir / "actions-reference.md").write_text(
                "| `test.documented` | Documented action |"
            )
            (shared_dir / "YAML_REFERENCE.md").write_text("")

            result = validate_against_docs(inventory, docs_dir)

        self.assertEqual(result.total_implemented, 2)
        self.assertEqual(result.coverage_percent, 50.0)


class TestIntegration(unittest.TestCase):
    """Integration tests using the real codebase."""

    def test_extract_from_real_codebase(self):
        """Test extraction from actual TEA actions directory."""
        actions_dir = get_actions_dir()

        if not actions_dir.exists():
            self.skipTest("Actions directory not found")

        inventory = extract_all_actions(actions_dir)

        # Should find significant number of actions
        self.assertGreater(inventory.total_actions, 100)
        self.assertGreater(len(inventory.modules), 30)

        # All modules should have files ending in .py
        for module in inventory.modules:
            self.assertTrue(module.file.endswith(".py"))

        # All actions should have valid names
        for module in inventory.modules:
            for action in module.actions:
                # Action name should be a non-empty string
                self.assertIsInstance(action.name, str)
                self.assertGreater(len(action.name), 0)
                # Most actions have namespace.action format, but some don't
                # Just verify the name doesn't have invalid characters
                self.assertFalse(action.name.startswith("_"))

    def test_programmatic_api(self):
        """Test the programmatic API (extract_signatures)."""
        actions_dir = get_actions_dir()

        if not actions_dir.exists():
            self.skipTest("Actions directory not found")

        inventory, validation = extract_signatures(validate=False)

        self.assertIsNotNone(inventory)
        self.assertIsNone(validation)
        self.assertGreater(inventory.total_actions, 0)

    def test_programmatic_api_with_validation(self):
        """Test programmatic API with validation enabled."""
        actions_dir = get_actions_dir()

        if not actions_dir.exists():
            self.skipTest("Actions directory not found")

        inventory, validation = extract_signatures(validate=True)

        self.assertIsNotNone(inventory)
        self.assertIsNotNone(validation)
        self.assertIsInstance(validation, ValidationResult)


if __name__ == "__main__":
    unittest.main()
