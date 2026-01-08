"""
Unit tests for LanceDBVectorStore (TEA-BUILTIN-002.5).

Tests cover:
- VectorStore protocol compliance
- Add single and batch documents
- Upsert (update existing documents)
- Query with filters
- Collection isolation
- Index creation at threshold
- State methods
- Factory integration
- Import error handling
"""

import pytest
from unittest.mock import MagicMock, patch, PropertyMock
import sys
import tempfile
import json


class TestLanceDBProtocol:
    """Test VectorStore protocol compliance (AC: 1)."""

    def test_has_add_method(self):
        """Store has add method."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        with patch.dict(sys.modules, {"lancedb": MagicMock()}):
            with patch.object(LanceDBVectorStore, "__init__", lambda self, path: None):
                store = LanceDBVectorStore.__new__(LanceDBVectorStore)
                assert hasattr(store, "add")
                assert callable(store.add)

    def test_has_query_method(self):
        """Store has query method."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        with patch.dict(sys.modules, {"lancedb": MagicMock()}):
            with patch.object(LanceDBVectorStore, "__init__", lambda self, path: None):
                store = LanceDBVectorStore.__new__(LanceDBVectorStore)
                assert hasattr(store, "query")
                assert callable(store.query)

    def test_has_get_state_method(self):
        """Store has get_state method."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        with patch.dict(sys.modules, {"lancedb": MagicMock()}):
            with patch.object(LanceDBVectorStore, "__init__", lambda self, path: None):
                store = LanceDBVectorStore.__new__(LanceDBVectorStore)
                assert hasattr(store, "get_state")
                assert callable(store.get_state)

    def test_has_restore_state_method(self):
        """Store has restore_state method."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        with patch.dict(sys.modules, {"lancedb": MagicMock()}):
            with patch.object(LanceDBVectorStore, "__init__", lambda self, path: None):
                store = LanceDBVectorStore.__new__(LanceDBVectorStore)
                assert hasattr(store, "restore_state")
                assert callable(store.restore_state)


class TestLanceDBImportError:
    """Test ImportError handling (AC: 12)."""

    def test_import_error_message(self):
        """Clear error message when lancedb not installed."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        # Remove lancedb from sys.modules if present
        with patch.dict(sys.modules, {"lancedb": None}):
            with pytest.raises(ImportError) as exc_info:
                LanceDBVectorStore()

            assert "lancedb not installed" in str(exc_info.value)
            assert "pip install lancedb pyarrow" in str(exc_info.value)


class TestLanceDBAdd:
    """Test add method (AC: 1, 5)."""

    def test_add_single_document(self):
        """Add single document to store."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 1
        mock_db.create_table.return_value = mock_table
        mock_db.open_table.side_effect = Exception("Table not found")

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            result = store.add(
                ids=["doc1"],
                texts=["Hello world"],
                embeddings=[[0.1, 0.2, 0.3]],
                metadatas=[{"type": "greeting"}],
                collection="test",
            )

            assert result == 1
            mock_db.create_table.assert_called_once()

    def test_add_batch_documents(self):
        """Add multiple documents at once."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 3
        mock_db.create_table.return_value = mock_table
        mock_db.open_table.side_effect = Exception("Table not found")

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            result = store.add(
                ids=["doc1", "doc2", "doc3"],
                texts=["Text 1", "Text 2", "Text 3"],
                embeddings=[[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]],
                collection="test",
            )

            assert result == 3

    def test_upsert_existing_document(self):
        """Upsert deletes existing before adding."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 1
        mock_db.open_table.return_value = mock_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            result = store.add(
                ids=["doc1"],
                texts=["Updated text"],
                embeddings=[[0.5, 0.6]],
                collection="test",
            )

            assert result == 1
            # Should have called delete for upsert
            mock_table.delete.assert_called()
            mock_table.add.assert_called_once()


class TestLanceDBQuery:
    """Test query method (AC: 6)."""

    def test_query_empty_collection(self):
        """Query returns empty list for non-existent collection."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_db.open_table.side_effect = Exception("Table not found")

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            results = store.query(
                embedding=[0.1, 0.2, 0.3], k=5, collection="nonexistent"
            )

            assert results == []

    def test_query_returns_results(self):
        """Query returns formatted results."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_search = MagicMock()
        mock_search.metric.return_value = mock_search
        mock_search.limit.return_value = mock_search
        mock_search.to_list.return_value = [
            {
                "id": "doc1",
                "text": "Hello",
                "_distance": 0.1,
                "metadata": '{"type": "greeting"}',
                "vector": [0.1, 0.2],
            },
            {
                "id": "doc2",
                "text": "World",
                "_distance": 0.2,
                "metadata": "{}",
                "vector": [0.3, 0.4],
            },
        ]
        mock_table.search.return_value = mock_search
        mock_db.open_table.return_value = mock_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            results = store.query(embedding=[0.1, 0.2], k=5, collection="test")

            assert len(results) == 2
            assert results[0]["id"] == "doc1"
            assert results[0]["text"] == "Hello"
            assert results[0]["score"] == 0.9  # 1 - 0.1
            assert results[0]["metadata"] == {"type": "greeting"}

    def test_query_with_filter(self):
        """Query with metadata filter."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_search = MagicMock()
        mock_search.metric.return_value = mock_search
        mock_search.limit.return_value = mock_search
        mock_search.where.return_value = mock_search
        mock_search.to_list.return_value = []
        mock_table.search.return_value = mock_search
        mock_db.open_table.return_value = mock_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            store.query(
                embedding=[0.1, 0.2],
                k=5,
                collection="test",
                filter={"type": "greeting"},
            )

            # Should have called where with filter
            mock_search.where.assert_called_once()

    def test_query_includes_embeddings(self):
        """Query can include embeddings in results."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_search = MagicMock()
        mock_search.metric.return_value = mock_search
        mock_search.limit.return_value = mock_search
        mock_search.to_list.return_value = [
            {
                "id": "doc1",
                "text": "Hello",
                "_distance": 0.1,
                "metadata": "{}",
                "vector": [0.1, 0.2, 0.3],
            },
        ]
        mock_table.search.return_value = mock_search
        mock_db.open_table.return_value = mock_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            results = store.query(
                embedding=[0.1, 0.2], k=5, collection="test", include_embeddings=True
            )

            assert "embedding" in results[0]
            assert results[0]["embedding"] == [0.1, 0.2, 0.3]


class TestLanceDBIndex:
    """Test index creation at threshold (AC: 4)."""

    def test_index_threshold_constant(self):
        """INDEX_THRESHOLD is 256."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        assert LanceDBVectorStore.INDEX_THRESHOLD == 256

    def test_index_created_at_threshold(self):
        """Index is created when row count reaches threshold."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 256
        mock_db.open_table.return_value = mock_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            store.add(
                ids=["doc1"],
                texts=["Hello"],
                embeddings=[[0.1, 0.2]],
                collection="test",
            )

            mock_table.create_index.assert_called_once()

    def test_no_index_below_threshold(self):
        """No index created below threshold."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 255
        mock_db.open_table.return_value = mock_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            store.add(
                ids=["doc1"],
                texts=["Hello"],
                embeddings=[[0.1, 0.2]],
                collection="test",
            )

            mock_table.create_index.assert_not_called()


class TestLanceDBState:
    """Test state methods (AC: 1)."""

    def test_get_state_returns_info(self):
        """get_state returns store info."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 1
        mock_db.create_table.return_value = mock_table
        mock_db.open_table.side_effect = Exception("Not found")

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")
            store.add(["doc1"], ["Text"], [[0.1]], collection="test")

            state = store.get_state()

            assert state["type"] == "lancedb"
            assert "path" in state
            assert "collections" in state

    def test_restore_state_noop(self):
        """restore_state is no-op for persistent store."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            # Should not raise
            store.restore_state({"type": "lancedb"})


class TestLanceDBFactory:
    """Test factory integration (AC: 7, 8, 9, 10)."""

    def test_factory_creates_lancedb_store(self):
        """Factory creates LanceDBVectorStore for store_type='lancedb'."""
        from the_edge_agent.actions.rag_actions import (
            create_vector_store,
            LanceDBVectorStore,
        )

        mock_db = MagicMock()
        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = create_vector_store(store_type="lancedb")
            assert isinstance(store, LanceDBVectorStore)

    def test_factory_uses_custom_path(self):
        """Factory passes custom path."""
        from the_edge_agent.actions.rag_actions import create_vector_store

        mock_db = MagicMock()
        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = create_vector_store(
                store_type="lancedb", lancedb_path="/custom/path"
            )

            mock_lancedb.connect.assert_called_with("/custom/path")

    def test_factory_uses_default_path(self):
        """Factory uses default path when not specified."""
        from the_edge_agent.actions.rag_actions import create_vector_store
        import os

        mock_db = MagicMock()
        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            create_vector_store(store_type="lancedb")

            # Should use expanded default path
            expected_path = os.path.expanduser("~/.tea/vectors/")
            mock_lancedb.connect.assert_called_with(expected_path)


class TestLanceDBCollectionIsolation:
    """Test collection isolation (AC: 1)."""

    def test_collections_isolated(self):
        """Different collections are isolated."""
        from the_edge_agent.actions.rag_actions import LanceDBVectorStore

        mock_db = MagicMock()
        tables = {}

        def create_table(name, data):
            mock_table = MagicMock()
            mock_table.count_rows.return_value = len(data)
            tables[name] = mock_table
            return mock_table

        def open_table(name):
            if name in tables:
                return tables[name]
            raise Exception("Not found")

        mock_db.create_table = create_table
        mock_db.open_table = open_table

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            store = LanceDBVectorStore(path="/tmp/test")

            store.add(["doc1"], ["Text A"], [[0.1, 0.2]], collection="coll_a")
            store.add(["doc2"], ["Text B"], [[0.3, 0.4]], collection="coll_b")

            assert "coll_a" in tables
            assert "coll_b" in tables


class TestLanceDBIntegration:
    """Integration tests with RAG actions."""

    def test_vector_store_action_with_lancedb(self):
        """vector.store action works with lancedb store."""
        from the_edge_agent.actions.rag_actions import (
            LanceDBVectorStore,
            register_actions,
        )

        mock_db = MagicMock()
        mock_table = MagicMock()
        mock_table.count_rows.return_value = 1
        mock_db.create_table.return_value = mock_table
        mock_db.open_table.side_effect = Exception("Not found")

        mock_lancedb = MagicMock()
        mock_lancedb.connect.return_value = mock_db

        # Create mock engine
        mock_engine = MagicMock()
        mock_engine._rag_provider = MagicMock()
        mock_engine._rag_provider.embed.return_value = [[0.1, 0.2, 0.3]]
        mock_engine._rag_provider.dimensions = 3
        mock_engine._rag_vector_store = None
        mock_engine.variables = {"settings": {"rag": {"vector_store": "lancedb"}}}

        registry = {}

        with patch.dict(sys.modules, {"lancedb": mock_lancedb}):
            register_actions(registry, mock_engine)

            result = registry["vector.store"](
                {}, texts=["Hello world"], collection="test"
            )

            assert "stored" in result
            assert result["stored"] == 1
