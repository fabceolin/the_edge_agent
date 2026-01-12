"""
Dataset utilities for TEA experiments.

This module provides functions for creating Opik datasets from various sources:
- create_dataset_from_fixtures: Create dataset from JSON fixture files
- create_dataset_from_list: Create dataset from a list of dictionaries

Datasets are used by Opik's evaluate() function to run experiments.

Example:
    >>> from the_edge_agent.experiments import (
    ...     create_dataset_from_fixtures,
    ...     create_dataset_from_list,
    ... )
    >>>
    >>> # Create from fixture files
    >>> dataset = create_dataset_from_fixtures(
    ...     name="my_test_cases",
    ...     fixtures_path="./tests/fixtures/",
    ... )
    >>>
    >>> # Create from a list
    >>> items = [
    ...     {"input": {"text": "Hello"}, "expected_output": {"response": "Hi"}},
    ...     {"input": {"text": "Bye"}, "expected_output": {"response": "Goodbye"}},
    ... ]
    >>> dataset = create_dataset_from_list(name="greeting_tests", items=items)
"""

import json
import logging
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Union

logger = logging.getLogger(__name__)


# Graceful degradation when Opik is not installed
try:
    import opik

    OPIK_AVAILABLE = True
except ImportError:
    OPIK_AVAILABLE = False


def _check_opik_available() -> None:
    """
    Check if the opik package is installed.

    Raises:
        ImportError: If opik is not installed.
    """
    if not OPIK_AVAILABLE:
        raise ImportError(
            "Opik SDK not installed. Install with: pip install opik\n"
            "Or install The Edge Agent with Opik support: "
            "pip install the-edge-agent[opik]"
        )


def create_dataset_from_fixtures(
    name: str,
    fixtures_path: Union[str, Path],
    input_transform: Optional[Callable[[Dict[str, Any]], Dict[str, Any]]] = None,
    description: Optional[str] = None,
    project_name: Optional[str] = None,
) -> Any:
    """
    Create an Opik dataset from JSON fixture files.

    Scans a directory for JSON files and creates dataset items from them.
    Each JSON file should contain a dictionary with at least an "input" key.

    Args:
        name: Name for the dataset in Opik.
        fixtures_path: Path to directory containing JSON fixture files.
        input_transform: Optional function to transform each fixture before
                        adding to the dataset. Receives the parsed JSON dict.
        description: Optional description for the dataset.
        project_name: Optional Opik project name.

    Returns:
        The created or updated Opik Dataset object.

    Raises:
        ImportError: If opik is not installed.
        FileNotFoundError: If fixtures_path does not exist.
        ValueError: If no valid JSON files found.

    Example:
        >>> # Fixtures directory structure:
        >>> # fixtures/
        >>> #   case_001.json  # {"input": {"text": "hello"}, "expected_output": {...}}
        >>> #   case_002.json  # {"input": {"text": "world"}, "expected_output": {...}}
        >>>
        >>> dataset = create_dataset_from_fixtures(
        ...     name="test_cases",
        ...     fixtures_path="./fixtures/",
        ... )
    """
    _check_opik_available()

    fixtures_path = Path(fixtures_path)
    if not fixtures_path.exists():
        raise FileNotFoundError(f"Fixtures path does not exist: {fixtures_path}")

    if not fixtures_path.is_dir():
        raise ValueError(f"Fixtures path must be a directory: {fixtures_path}")

    # Find all JSON files
    json_files = sorted(fixtures_path.glob("*.json"))
    if not json_files:
        raise ValueError(f"No JSON files found in: {fixtures_path}")

    # Parse fixtures
    items: List[Dict[str, Any]] = []
    for json_file in json_files:
        try:
            with open(json_file, "r", encoding="utf-8") as f:
                data = json.load(f)

            # Apply transform if provided
            if input_transform is not None:
                data = input_transform(data)

            # Ensure required structure
            if "input" not in data:
                # If no "input" key, wrap the entire data as input
                data = {"input": data}

            items.append(data)
            logger.debug(f"Loaded fixture: {json_file.name}")

        except json.JSONDecodeError as e:
            logger.warning(f"Skipping invalid JSON file {json_file.name}: {e}")
        except Exception as e:
            logger.warning(f"Error loading fixture {json_file.name}: {e}")

    if not items:
        raise ValueError(f"No valid fixtures loaded from: {fixtures_path}")

    logger.info(f"Loaded {len(items)} fixtures from {fixtures_path}")

    # Create dataset
    return create_dataset_from_list(
        name=name,
        items=items,
        description=description,
        project_name=project_name,
    )


def create_dataset_from_list(
    name: str,
    items: List[Dict[str, Any]],
    description: Optional[str] = None,
    project_name: Optional[str] = None,
) -> Any:
    """
    Create an Opik dataset from a list of dictionaries.

    Each item should have at minimum an "input" key. Optional keys:
    - expected_output: The expected output for scoring
    - metadata: Additional metadata for the item

    Args:
        name: Name for the dataset in Opik.
        items: List of dictionaries, each with at least "input" key.
        description: Optional description for the dataset.
        project_name: Optional Opik project name.

    Returns:
        The created or updated Opik Dataset object.

    Raises:
        ImportError: If opik is not installed.
        ValueError: If items list is empty.

    Example:
        >>> items = [
        ...     {
        ...         "input": {"query": "What is TEA?"},
        ...         "expected_output": {"answer": "The Edge Agent"},
        ...         "metadata": {"category": "faq"}
        ...     },
        ...     {
        ...         "input": {"query": "How to install?"},
        ...         "expected_output": {"answer": "pip install the-edge-agent"},
        ...     }
        ... ]
        >>> dataset = create_dataset_from_list("qa_tests", items)
    """
    _check_opik_available()

    if not items:
        raise ValueError("Items list cannot be empty")

    # Get or create client
    client = opik.Opik(project_name=project_name)

    # Get or create dataset
    dataset = client.get_or_create_dataset(name=name, description=description)

    # Convert items to Opik DatasetItem format
    dataset_items = []
    for item in items:
        # Build the dataset item
        dataset_item = {
            "input": item.get("input", item),
        }

        # Add expected_output if present
        if "expected_output" in item:
            dataset_item["expected_output"] = item["expected_output"]

        # Add metadata if present
        if "metadata" in item:
            dataset_item["metadata"] = item["metadata"]

        # Add reference if present (for traceability)
        if "reference" in item:
            dataset_item["reference"] = item["reference"]

        dataset_items.append(dataset_item)

    # Insert items into dataset
    dataset.insert(dataset_items)

    logger.info(f"Created dataset '{name}' with {len(dataset_items)} items")

    return dataset
