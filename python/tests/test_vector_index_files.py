"""
Tests for vector.index_files Action (TEA-BUILTIN-002.3).

Tests cover:
- File listing with glob patterns and extensions
- Chunking strategies (line, paragraph, document)
- Change detection for incremental updates
- Error handling (missing files, binary files)
- Integration with vector store

All tests use mocks - no external services required.
"""

import os
import tempfile
import unittest
from unittest.mock import patch, MagicMock, Mock

from the_edge_agent.actions.rag_actions import (
    ChunkStrategy,
    Chunk,
    FileState,
    register_actions,
    InMemoryVectorStore,
)


class TestChunkStrategy(unittest.TestCase):
    """Tests for ChunkStrategy enum."""

    def test_chunk_strategy_values(self):
        """ChunkStrategy has correct values."""
        self.assertEqual(ChunkStrategy.LINE.value, "line")
        self.assertEqual(ChunkStrategy.PARAGRAPH.value, "paragraph")
        self.assertEqual(ChunkStrategy.DOCUMENT.value, "document")

    def test_chunk_strategy_from_string(self):
        """ChunkStrategy can be created from string."""
        self.assertEqual(ChunkStrategy("line"), ChunkStrategy.LINE)
        self.assertEqual(ChunkStrategy("paragraph"), ChunkStrategy.PARAGRAPH)
        self.assertEqual(ChunkStrategy("document"), ChunkStrategy.DOCUMENT)


class TestChunkDataClass(unittest.TestCase):
    """Tests for Chunk dataclass."""

    def test_chunk_creation(self):
        """Chunk can be created with all fields."""
        chunk = Chunk(text="Hello world", start_line=0, end_line=1, chunk_type="line")
        self.assertEqual(chunk.text, "Hello world")
        self.assertEqual(chunk.start_line, 0)
        self.assertEqual(chunk.end_line, 1)
        self.assertEqual(chunk.chunk_type, "line")


class TestFileState(unittest.TestCase):
    """Tests for FileState dataclass."""

    def test_file_state_creation(self):
        """FileState can be created with all fields."""
        state = FileState(
            path="/path/to/file.py",
            size=1024,
            mtime=1234567890.0,
            indexed_at=1234567900.0,
        )
        self.assertEqual(state.path, "/path/to/file.py")
        self.assertEqual(state.size, 1024)
        self.assertEqual(state.mtime, 1234567890.0)
        self.assertEqual(state.indexed_at, 1234567900.0)


class TestFileListingLocal(unittest.TestCase):
    """Tests for local file listing."""

    def setUp(self):
        """Set up test fixtures."""
        # Create temporary directory with test files
        self.temp_dir = tempfile.mkdtemp()

        # Create test files
        self.files = {
            "test1.py": "print('hello')",
            "test2.py": "def foo():\n    pass",
            "readme.md": "# README",
            "data.json": '{"key": "value"}',
            "nested/deep.py": "# nested file",
        }

        for filename, content in self.files.items():
            filepath = os.path.join(self.temp_dir, filename)
            os.makedirs(os.path.dirname(filepath), exist_ok=True)
            with open(filepath, "w") as f:
                f.write(content)

    def tearDown(self):
        """Clean up test fixtures."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_list_files_all(self):
        """List all files in directory."""
        registry = {}
        mock_engine = MagicMock()

        # Set up mock provider before registration
        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        # Access the internal _list_files function via the action
        # We'll test indirectly through the action
        result = registry["vector.index_files"](
            state={},
            paths=[self.temp_dir],
            pattern="**/*",
            recursive=True,
            chunk_by="document",
            incremental=False,
        )

        # Should find all text files
        self.assertTrue(result.get("success", False))
        self.assertIn("files", result)
        self.assertGreater(result["files"], 0)

    def test_list_files_extension_filter(self):
        """Extension filtering works."""
        registry = {}
        mock_engine = MagicMock()

        # Mock embedding provider to return embeddings for any input
        def mock_embed(texts):
            return [[0.1] * 384 for _ in texts]

        mock_provider = MagicMock()
        mock_provider.embed.side_effect = mock_embed
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={},
            paths=[self.temp_dir],
            extensions=[".py"],
            chunk_by="document",
            incremental=False,
        )

        self.assertTrue(result.get("success", False))
        # Should only index .py files (we have 3 .py files including nested)


class TestChunking(unittest.TestCase):
    """Tests for file chunking strategies."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_file = os.path.join(self.temp_dir, "test.py")

        # Create test file with known content
        self.content = """# Line 0
def foo():  # Line 1
    pass  # Line 2

# Line 4 (after empty line)
class Bar:  # Line 5
    x = 1  # Line 6
"""
        with open(self.test_file, "w") as f:
            f.write(self.content)

    def tearDown(self):
        """Clean up."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_chunk_by_line(self):
        """Line chunking produces correct output."""
        registry = {}
        mock_engine = MagicMock()

        # Provider must return enough embeddings for all lines
        def mock_embed(texts):
            return [[0.1] * 384 for _ in texts]

        mock_provider = MagicMock()
        mock_provider.embed.side_effect = mock_embed
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={}, paths=[self.test_file], chunk_by="line", incremental=False
        )

        self.assertTrue(result.get("success", False))
        # Should have indexed non-empty lines
        self.assertGreater(result["indexed"], 0)

    def test_chunk_by_paragraph(self):
        """Paragraph chunking works."""
        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={}, paths=[self.test_file], chunk_by="paragraph", incremental=False
        )

        self.assertTrue(result.get("success", False))

    def test_chunk_by_document(self):
        """Document chunking produces single chunk."""
        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={}, paths=[self.test_file], chunk_by="document", incremental=False
        )

        self.assertTrue(result.get("success", False))
        # Document mode creates one chunk per file
        self.assertEqual(result["indexed"], 1)


class TestChangeDetection(unittest.TestCase):
    """Tests for incremental change detection."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_file = os.path.join(self.temp_dir, "test.py")

        with open(self.test_file, "w") as f:
            f.write("# Initial content")

    def tearDown(self):
        """Clean up."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_new_file_detected(self):
        """New file is indexed."""
        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={}, paths=[self.test_file], chunk_by="document", incremental=True
        )

        self.assertTrue(result.get("success", False))
        self.assertEqual(result["indexed"], 1)
        self.assertEqual(result["skipped"], 0)

    def test_unchanged_file_skipped(self):
        """Unchanged file is skipped on re-index."""
        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        # First index
        result1 = registry["vector.index_files"](
            state={}, paths=[self.test_file], chunk_by="document", incremental=True
        )

        self.assertEqual(result1["indexed"], 1)

        # Second index (same file, unchanged)
        result2 = registry["vector.index_files"](
            state={}, paths=[self.test_file], chunk_by="document", incremental=True
        )

        self.assertEqual(result2["skipped"], 1)


class TestErrorHandling(unittest.TestCase):
    """Tests for error handling."""

    def test_missing_file_error(self):
        """Missing file is handled gracefully."""
        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={},
            paths=["/nonexistent/path/file.py"],
            chunk_by="line",
            incremental=False,
        )

        self.assertTrue(result.get("success", False))
        self.assertEqual(result["files"], 0)

    def test_binary_file_skipped(self):
        """Binary files are skipped."""
        temp_dir = tempfile.mkdtemp()
        binary_file = os.path.join(temp_dir, "binary.bin")

        try:
            # Create a binary file
            with open(binary_file, "wb") as f:
                f.write(b"\x00\x01\x02\x03" * 100)

            registry = {}
            mock_engine = MagicMock()

            mock_provider = MagicMock()
            mock_provider.embed.return_value = [[0.1] * 384]
            mock_provider.dimensions = 384
            mock_provider.model_name = "test"
            mock_engine._rag_provider = mock_provider
            mock_engine._rag_vector_store = InMemoryVectorStore()

            register_actions(registry, mock_engine)

            result = registry["vector.index_files"](
                state={}, paths=[binary_file], chunk_by="document", incremental=False
            )

            self.assertTrue(result.get("success", False))
            self.assertEqual(result["skipped"], 1)
            self.assertEqual(result["indexed"], 0)
        finally:
            import shutil

            shutil.rmtree(temp_dir, ignore_errors=True)

    def test_invalid_chunk_strategy(self):
        """Invalid chunk_by value returns error."""
        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        result = registry["vector.index_files"](
            state={}, paths=["/some/path"], chunk_by="invalid_strategy"
        )

        self.assertFalse(result.get("success", True))
        self.assertIn("Invalid chunk_by", result.get("error", ""))


class TestFsspecMock(unittest.TestCase):
    """Tests for fsspec URI handling (mocked)."""

    @patch("the_edge_agent.actions.rag_actions.FSSPEC_AVAILABLE", True)
    @patch("the_edge_agent.actions.rag_actions.fsspec")
    def test_s3_uri_handling(self, mock_fsspec):
        """S3 URIs are handled correctly (mocked)."""
        # Mock fsspec filesystem
        mock_fs = MagicMock()
        mock_fs.isfile.return_value = False
        mock_fs.isdir.return_value = True
        mock_fs.glob.return_value = ["bucket/path/file.py"]
        mock_fs.isfile.side_effect = lambda p: p.endswith(".py")
        mock_fsspec.core.url_to_fs.return_value = (mock_fs, "bucket/path")

        registry = {}
        mock_engine = MagicMock()

        mock_provider = MagicMock()
        mock_provider.embed.return_value = [[0.1] * 384]
        mock_provider.dimensions = 384
        mock_provider.model_name = "test"
        mock_engine._rag_provider = mock_provider
        mock_engine._rag_vector_store = InMemoryVectorStore()

        register_actions(registry, mock_engine)

        # This will trigger the fsspec path
        result = registry["vector.index_files"](
            state={},
            paths=["s3://bucket/path/"],
            chunk_by="document",
            incremental=False,
        )

        # Should attempt to use fsspec
        mock_fsspec.core.url_to_fs.assert_called()


class TestIntegration(unittest.TestCase):
    """Integration tests with real file operations."""

    def test_index_then_query(self):
        """Index files then query them."""
        temp_dir = tempfile.mkdtemp()
        test_file = os.path.join(temp_dir, "auth.py")

        try:
            # Create test file
            with open(test_file, "w") as f:
                f.write(
                    """
def authenticate_user(username, password):
    '''Authenticate a user with username and password.'''
    if not username or not password:
        raise ValueError("Username and password required")
    return check_credentials(username, password)
"""
                )

            registry = {}
            mock_engine = MagicMock()

            # Use a simple mock provider that returns different embeddings
            call_count = [0]

            def mock_embed(texts):
                results = []
                for i, t in enumerate(texts):
                    # Create slightly different embedding based on text
                    base = 0.1 + (call_count[0] * 0.01)
                    call_count[0] += 1
                    results.append([base + (j * 0.001) for j in range(384)])
                return results

            mock_provider = MagicMock()
            mock_provider.embed.side_effect = mock_embed
            mock_provider.dimensions = 384
            mock_provider.model_name = "test"
            mock_engine._rag_provider = mock_provider
            mock_engine._rag_vector_store = InMemoryVectorStore()

            register_actions(registry, mock_engine)

            # Index the file
            index_result = registry["vector.index_files"](
                state={},
                paths=[test_file],
                chunk_by="line",
                collection="test_code",
                incremental=False,
            )

            self.assertTrue(index_result.get("success", False))
            self.assertGreater(index_result["indexed"], 0)

            # Query the indexed content
            query_result = registry["vector.query"](
                state={},
                query="authentication error handling",
                collection="test_code",
                k=5,
            )

            # Should find results
            self.assertIn("results", query_result)

        finally:
            import shutil

            shutil.rmtree(temp_dir, ignore_errors=True)


class TestMetadata(unittest.TestCase):
    """Tests for metadata generation."""

    def test_line_metadata(self):
        """Line chunks have correct metadata."""
        temp_dir = tempfile.mkdtemp()
        test_file = os.path.join(temp_dir, "test.py")

        try:
            with open(test_file, "w") as f:
                f.write("line 0\nline 1\nline 2")

            registry = {}
            mock_engine = MagicMock()

            mock_provider = MagicMock()
            mock_provider.embed.return_value = [[0.1] * 384] * 3
            mock_provider.dimensions = 384
            mock_provider.model_name = "test"
            mock_engine._rag_provider = mock_provider

            # Use real store to verify metadata
            store = InMemoryVectorStore()
            mock_engine._rag_vector_store = store

            register_actions(registry, mock_engine)

            result = registry["vector.index_files"](
                state={},
                paths=[test_file],
                chunk_by="line",
                collection="meta_test",
                incremental=False,
            )

            self.assertTrue(result.get("success", False))

            # Query to get stored documents
            query_result = store.query(
                embedding=[0.1] * 384, k=10, collection="meta_test"
            )

            # Check metadata
            for item in query_result:
                self.assertIn("file", item["metadata"])
                self.assertIn("line", item["metadata"])
                self.assertIn("chunk_type", item["metadata"])
                self.assertEqual(item["metadata"]["chunk_type"], "line")

        finally:
            import shutil

            shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
