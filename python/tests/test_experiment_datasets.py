"""
Tests for TEA-BUILTIN-005.4: Experiment Framework - Datasets Module

Test coverage for:
- create_dataset_from_list
- create_dataset_from_fixtures
- Graceful degradation when Opik not installed
"""

import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


class TestCreateDatasetFromList(unittest.TestCase):
    """Unit tests for create_dataset_from_list function."""

    def setUp(self):
        """Set up mock Opik for each test."""
        # Create patches for datasets module
        self.opik_available_patch = patch(
            "the_edge_agent.experiments.datasets.OPIK_AVAILABLE", True
        )
        self.opik_patch = patch("the_edge_agent.experiments.datasets.opik")

        self.opik_available_patch.start()
        self.mock_opik = self.opik_patch.start()

        self.mock_client = MagicMock()
        self.mock_dataset = MagicMock()
        self.mock_opik.Opik.return_value = self.mock_client
        self.mock_client.get_or_create_dataset.return_value = self.mock_dataset

    def tearDown(self):
        """Stop patches."""
        self.opik_patch.stop()
        self.opik_available_patch.stop()

    def test_creates_dataset_with_name(self):
        """P0: Creates dataset with correct name."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        items = [{"input": {"text": "hello"}}]
        create_dataset_from_list("test_dataset", items)

        self.mock_client.get_or_create_dataset.assert_called_with(
            name="test_dataset", description=None
        )

    def test_inserts_items_into_dataset(self):
        """P0: Items are inserted into dataset."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        items = [
            {"input": {"text": "hello"}, "expected_output": {"response": "hi"}},
            {"input": {"text": "bye"}, "expected_output": {"response": "goodbye"}},
        ]
        create_dataset_from_list("test_dataset", items)

        # Verify insert was called with transformed items
        self.mock_dataset.insert.assert_called_once()
        inserted_items = self.mock_dataset.insert.call_args[0][0]
        self.assertEqual(len(inserted_items), 2)

    def test_transforms_items_correctly(self):
        """P0: Items are transformed to Opik format."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        items = [
            {
                "input": {"query": "test"},
                "expected_output": {"answer": "result"},
                "metadata": {"source": "unit_test"},
            }
        ]
        create_dataset_from_list("test_dataset", items)

        inserted_items = self.mock_dataset.insert.call_args[0][0]
        item = inserted_items[0]

        self.assertIn("input", item)
        self.assertIn("expected_output", item)
        self.assertIn("metadata", item)
        self.assertEqual(item["input"]["query"], "test")
        self.assertEqual(item["expected_output"]["answer"], "result")
        self.assertEqual(item["metadata"]["source"], "unit_test")

    def test_handles_items_without_expected_output(self):
        """P1: Items without expected_output are handled."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        items = [{"input": {"text": "hello"}}]  # No expected_output
        create_dataset_from_list("test_dataset", items)

        inserted_items = self.mock_dataset.insert.call_args[0][0]
        item = inserted_items[0]

        self.assertIn("input", item)
        self.assertNotIn("expected_output", item)

    def test_empty_items_raises_error(self):
        """P0: Empty items list raises ValueError."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        with self.assertRaises(ValueError) as ctx:
            create_dataset_from_list("test_dataset", [])

        self.assertIn("empty", str(ctx.exception).lower())

    def test_with_description(self):
        """P1: Description is passed to get_or_create_dataset."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        items = [{"input": {"text": "hello"}}]
        create_dataset_from_list("test_dataset", items, description="Test cases")

        self.mock_client.get_or_create_dataset.assert_called_with(
            name="test_dataset", description="Test cases"
        )

    def test_with_project_name(self):
        """P1: Project name is passed to Opik client."""
        from the_edge_agent.experiments.datasets import create_dataset_from_list

        items = [{"input": {"text": "hello"}}]
        create_dataset_from_list("test_dataset", items, project_name="my-project")

        self.mock_opik.Opik.assert_called_with(project_name="my-project")


class TestCreateDatasetFromFixtures(unittest.TestCase):
    """Unit tests for create_dataset_from_fixtures function."""

    def setUp(self):
        """Set up mock Opik and temp directory for each test."""
        # Create patches for datasets module
        self.opik_available_patch = patch(
            "the_edge_agent.experiments.datasets.OPIK_AVAILABLE", True
        )
        self.opik_patch = patch("the_edge_agent.experiments.datasets.opik")

        self.opik_available_patch.start()
        self.mock_opik = self.opik_patch.start()

        self.mock_client = MagicMock()
        self.mock_dataset = MagicMock()
        self.mock_opik.Opik.return_value = self.mock_client
        self.mock_client.get_or_create_dataset.return_value = self.mock_dataset

        # Create temp directory for fixtures
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temp directory and stop patches."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

        self.opik_patch.stop()
        self.opik_available_patch.stop()

    def _create_fixture(self, filename: str, content: dict) -> Path:
        """Helper to create a fixture file."""
        filepath = Path(self.temp_dir) / filename
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(content, f)
        return filepath

    def test_loads_json_fixtures(self):
        """P0: Loads JSON files from directory."""
        self._create_fixture("case_001.json", {"input": {"text": "hello"}})
        self._create_fixture("case_002.json", {"input": {"text": "world"}})

        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        create_dataset_from_fixtures("test_fixtures", self.temp_dir)

        inserted_items = self.mock_dataset.insert.call_args[0][0]
        self.assertEqual(len(inserted_items), 2)

    def test_nonexistent_path_raises_error(self):
        """P0: FileNotFoundError for nonexistent path."""
        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        with self.assertRaises(FileNotFoundError):
            create_dataset_from_fixtures("test", "/nonexistent/path")

    def test_no_json_files_raises_error(self):
        """P0: ValueError when no JSON files found."""
        # Create non-JSON file
        txt_file = Path(self.temp_dir) / "readme.txt"
        txt_file.write_text("Not JSON")

        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        with self.assertRaises(ValueError) as ctx:
            create_dataset_from_fixtures("test", self.temp_dir)

        self.assertIn("No JSON files", str(ctx.exception))

    def test_file_path_raises_error(self):
        """P1: ValueError when path is a file, not directory."""
        json_file = self._create_fixture("test.json", {"input": "data"})

        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        with self.assertRaises(ValueError) as ctx:
            create_dataset_from_fixtures("test", str(json_file))

        self.assertIn("directory", str(ctx.exception).lower())

    def test_input_transform_applied(self):
        """P1: Input transform function is applied."""
        self._create_fixture("case.json", {"raw_text": "hello"})

        def transform(data):
            return {"input": {"text": data["raw_text"].upper()}}

        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        create_dataset_from_fixtures(
            "test_fixtures", self.temp_dir, input_transform=transform
        )

        inserted_items = self.mock_dataset.insert.call_args[0][0]
        self.assertEqual(inserted_items[0]["input"]["text"], "HELLO")

    def test_wraps_data_without_input_key(self):
        """P1: Data without 'input' key is wrapped."""
        self._create_fixture("case.json", {"text": "hello", "value": 42})

        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        create_dataset_from_fixtures("test_fixtures", self.temp_dir)

        inserted_items = self.mock_dataset.insert.call_args[0][0]
        # The whole dict should be wrapped under "input"
        self.assertIn("text", inserted_items[0]["input"])

    def test_invalid_json_skipped(self):
        """P2: Invalid JSON files are skipped with warning."""
        self._create_fixture("valid.json", {"input": {"text": "hello"}})

        # Create invalid JSON
        invalid_file = Path(self.temp_dir) / "invalid.json"
        invalid_file.write_text("{ not valid json }")

        from the_edge_agent.experiments.datasets import (
            create_dataset_from_fixtures,
        )

        create_dataset_from_fixtures("test_fixtures", self.temp_dir)

        # Only valid file should be loaded
        inserted_items = self.mock_dataset.insert.call_args[0][0]
        self.assertEqual(len(inserted_items), 1)


class TestDatasetsMissingOpik(unittest.TestCase):
    """Tests for behavior when Opik is not installed."""

    def test_import_error_without_opik(self):
        """P0: ImportError raised when Opik not installed."""
        # Clear opik from modules
        saved_modules = {}
        modules_to_hide = [k for k in list(sys.modules.keys()) if "opik" in k]
        for mod in modules_to_hide:
            saved_modules[mod] = sys.modules.pop(mod)

        try:
            with patch.dict("sys.modules", {"opik": None}):
                # Force reimport
                import importlib
                from the_edge_agent.experiments import datasets

                importlib.reload(datasets)

                with self.assertRaises(ImportError) as ctx:
                    datasets.create_dataset_from_list("test", [{"input": "data"}])

                self.assertIn("opik", str(ctx.exception).lower())

        finally:
            sys.modules.update(saved_modules)


if __name__ == "__main__":
    unittest.main()
