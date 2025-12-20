"""
Pytest configuration and shared fixtures for TEA tests.

This file imports fixtures from various modules to make them available
across all test files.
"""

import pytest
import sys
from pathlib import Path

# Ensure src and tests directories are in path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))
sys.path.insert(0, str(Path(__file__).parent))

# Import memory action fixtures
from conftest_memory_actions import (
    # Mock implementations
    MockMetadataStore,
    MockBlobStorage,
    MockQueryEngine,
    MockVectorIndex,
    # Pytest fixtures
    mock_metadata_store,
    mock_blob_storage,
    mock_query_engine,
    mock_vector_index,
    mock_state,
    sample_embedding,
    sample_memory_docs,
)

# Re-export fixtures so pytest can find them
__all__ = [
    'MockMetadataStore',
    'MockBlobStorage',
    'MockQueryEngine',
    'MockVectorIndex',
    'mock_metadata_store',
    'mock_blob_storage',
    'mock_query_engine',
    'mock_vector_index',
    'mock_state',
    'sample_embedding',
    'sample_memory_docs',
]
