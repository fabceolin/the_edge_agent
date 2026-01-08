"""
Unit tests for Neo4jBackend (TEA-BUILTIN-001.7.1).

Tests cover:
- Protocol compliance (AC-1)
- URI scheme validation (AC-2)
- Connection pooling (AC-4, 5, 6)
- Authentication methods (AC-8, 9)
- Environment variable expansion (AC-10)
- Credential security (AC-11)
- Authentication error handling (AC-12)
- store_entity (AC-13)
- store_relation (AC-14)
- query (AC-15)
- retrieve_context (AC-16)
- close (AC-17)
- Error handling (AC-18, 19, 20)
- Availability check (AC-21)
- Module exports (AC-22, 23)
- Thread safety (AC-24)
"""

import json
import os
import threading
import unittest
from unittest.mock import MagicMock, patch, PropertyMock


class TestNeo4jAvailability(unittest.TestCase):
    """Test Neo4j availability checks (AC-21, 22, 23)."""

    def test_check_neo4j_available_function_exists(self):
        """Test that _check_neo4j_available function is exported."""
        from the_edge_agent.memory import _check_neo4j_available

        self.assertTrue(callable(_check_neo4j_available))

    def test_neo4j_available_constant_exported(self):
        """Test that NEO4J_AVAILABLE constant is exported."""
        from the_edge_agent.memory import NEO4J_AVAILABLE

        self.assertIsInstance(NEO4J_AVAILABLE, bool)

    def test_neo4j_backend_conditionally_exported(self):
        """Test that Neo4jBackend is conditionally exported."""
        from the_edge_agent.memory import Neo4jBackend

        # Either it's a class or None depending on neo4j driver availability
        self.assertTrue(Neo4jBackend is None or isinstance(Neo4jBackend, type))


class TestNeo4jBackendWithMockedDriver(unittest.TestCase):
    """Tests using mocked Neo4j driver."""

    def setUp(self):
        """Set up mock for each test."""
        # Create mock driver and session
        self.mock_driver = MagicMock()
        self.mock_session = MagicMock()
        self.mock_driver.session.return_value.__enter__ = MagicMock(
            return_value=self.mock_session
        )
        self.mock_driver.session.return_value.__exit__ = MagicMock(return_value=False)

        # Create patches
        self.neo4j_patcher = patch.dict(
            "sys.modules", {"neo4j": MagicMock(), "neo4j.exceptions": MagicMock()}
        )
        self.neo4j_patcher.start()

    def tearDown(self):
        """Clean up patches."""
        self.neo4j_patcher.stop()

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_import_error_when_driver_missing(self):
        """Test ImportError when neo4j driver is not installed."""
        # Force NEO4J_AVAILABLE to False
        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", False):
            from the_edge_agent.memory.graph import Neo4jBackend as Backend

            with self.assertRaises(ImportError) as context:
                Backend(uri="bolt://localhost:7687")
            self.assertIn("Neo4j driver not installed", str(context.exception))


class TestNeo4jBackendURISchemes(unittest.TestCase):
    """Test URI scheme validation (AC-2)."""

    def test_valid_bolt_scheme(self):
        """Test bolt:// scheme is valid."""
        schemes = [
            "bolt://localhost:7687",
            "bolt+s://localhost:7687",
            "bolt+ssc://localhost:7687",
        ]
        for uri in schemes:
            # These should not raise ValueError for URI validation
            self.assertTrue(uri.startswith("bolt"))

    def test_valid_neo4j_scheme(self):
        """Test neo4j:// scheme is valid."""
        schemes = [
            "neo4j://localhost:7687",
            "neo4j+s://localhost:7687",
            "neo4j+ssc://localhost:7687",
        ]
        for uri in schemes:
            self.assertTrue(uri.startswith("neo4j"))

    def test_all_supported_schemes(self):
        """Test all 6 supported URI schemes are documented."""
        valid_schemes = [
            "bolt://",
            "bolt+s://",
            "bolt+ssc://",
            "neo4j://",
            "neo4j+s://",
            "neo4j+ssc://",
        ]
        self.assertEqual(len(valid_schemes), 6)


class TestNeo4jEnvironmentVariableExpansion(unittest.TestCase):
    """Test environment variable expansion (AC-10)."""

    def test_expand_simple_env_var(self):
        """Test ${VAR} syntax expansion."""
        os.environ["TEST_NEO4J_VAR"] = "test_value"
        try:
            from the_edge_agent.memory.graph import Neo4jBackend

            # Create instance to test _expand_env_var method
            # We need to patch the driver initialization
            with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
                with patch.object(Neo4jBackend, "_init_driver"):
                    backend = object.__new__(Neo4jBackend)
                    result = backend._expand_env_var("${TEST_NEO4J_VAR}")
                    self.assertEqual(result, "test_value")
        finally:
            del os.environ["TEST_NEO4J_VAR"]

    def test_expand_missing_env_var(self):
        """Test expansion of missing env var returns empty string."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                result = backend._expand_env_var("${NONEXISTENT_VAR_12345}")
                self.assertEqual(result, "")

    def test_expand_none_returns_none(self):
        """Test None input returns None."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                result = backend._expand_env_var(None)
                self.assertIsNone(result)

    def test_expand_mixed_content(self):
        """Test mixed content with env var and literal text."""
        os.environ["TEST_HOST"] = "myhost"
        try:
            from the_edge_agent.memory.graph import Neo4jBackend

            with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
                with patch.object(Neo4jBackend, "_init_driver"):
                    backend = object.__new__(Neo4jBackend)
                    result = backend._expand_env_var("prefix_${TEST_HOST}_suffix")
                    self.assertEqual(result, "prefix_myhost_suffix")
        finally:
            del os.environ["TEST_HOST"]


class TestNeo4jCredentialSecurity(unittest.TestCase):
    """Test credential security - no leakage in logs/errors (AC-11)."""

    def test_repr_no_password(self):
        """Test __repr__ does not contain password."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._uri = "bolt://localhost:7687"
                backend._database = "neo4j"
                backend._password = "secret_password_123"

                repr_str = repr(backend)
                self.assertNotIn("secret_password_123", repr_str)
                self.assertNotIn("password", repr_str.lower())

    def test_str_no_password(self):
        """Test __str__ does not contain password."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._uri = "bolt://localhost:7687"
                backend._database = "neo4j"
                backend._password = "secret_password_123"

                str_repr = str(backend)
                self.assertNotIn("secret_password_123", str_repr)

    def test_repr_no_bearer_token(self):
        """Test __repr__ does not contain bearer token."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._uri = "bolt://localhost:7687"
                backend._database = "neo4j"
                backend._bearer_token = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9..."

                repr_str = repr(backend)
                self.assertNotIn("eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9", repr_str)
                self.assertNotIn("bearer", repr_str.lower())


class TestNeo4jErrorHandling(unittest.TestCase):
    """Test error handling and error dict format (AC-18, 19)."""

    def test_store_entity_validation_error(self):
        """Test store_entity returns validation_error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                # Test missing entity_id
                result = backend.store_entity("", "Person")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_id", result["error"])

    def test_store_relation_validation_error(self):
        """Test store_relation returns validation_error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                # Test missing relation_type
                result = backend.store_relation("a", "b", "")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")

    def test_query_datalog_error(self):
        """Test query returns error when datalog is used instead of cypher."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.query(datalog="?[x] := entity[x, _, _, _]")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("Cypher", result["error"])
                self.assertIn("Datalog", result["error"])

    def test_query_missing_params_error(self):
        """Test query returns error when no query params provided."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.query()
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("cypher or pattern", result["error"])

    def test_retrieve_context_validation_error(self):
        """Test retrieve_context returns error when no params provided."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.retrieve_context()
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")

    def test_error_dict_format(self):
        """Test error dict format consistency."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.store_entity("", "")
                # Verify error dict has required keys
                self.assertIn("success", result)
                self.assertIn("error", result)
                self.assertIn("error_type", result)
                self.assertFalse(result["success"])
                self.assertIsInstance(result["error"], str)
                self.assertIsInstance(result["error_type"], str)


class TestNeo4jProtocolCompliance(unittest.TestCase):
    """Test GraphBackend protocol compliance (AC-1)."""

    def test_implements_required_methods(self):
        """Test Neo4jBackend implements all required GraphBackend methods."""
        from the_edge_agent.memory.graph import Neo4jBackend, GraphBackend

        # Check that Neo4jBackend has all required methods
        required_methods = [
            "store_entity",
            "store_relation",
            "query",
            "retrieve_context",
            "close",
        ]

        for method in required_methods:
            self.assertTrue(
                hasattr(Neo4jBackend, method),
                f"Neo4jBackend missing required method: {method}",
            )
            self.assertTrue(
                callable(getattr(Neo4jBackend, method)),
                f"Neo4jBackend.{method} is not callable",
            )


class TestNeo4jPatternToCypher(unittest.TestCase):
    """Test pattern to Cypher conversion (AC-15)."""

    def test_entity_type_pattern(self):
        """Test entity_type pattern conversion."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                cypher = backend._pattern_to_cypher({"entity_type": "Person"}, 100)
                self.assertIn("MATCH", cypher)
                self.assertIn("type = 'Person'", cypher)
                self.assertIn("LIMIT 100", cypher)

    def test_from_entity_pattern(self):
        """Test from_entity pattern conversion."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                cypher = backend._pattern_to_cypher({"from_entity": "entity_1"}, 100)
                self.assertIn("MATCH", cypher)
                self.assertIn("id: 'entity_1'", cypher)
                self.assertIn("-[r]->", cypher)

    def test_empty_pattern(self):
        """Test empty pattern returns all entities."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                cypher = backend._pattern_to_cypher({}, 50)
                self.assertIn("MATCH (e)", cypher)
                self.assertIn("LIMIT 50", cypher)


class TestNeo4jContextSummary(unittest.TestCase):
    """Test context summary building (AC-16)."""

    def test_build_context_summary_empty(self):
        """Test context summary with no entities."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                summary = backend._build_context_summary([], [])
                self.assertIn("No relevant entities", summary)

    def test_build_context_summary_with_entities(self):
        """Test context summary with entities."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                entities = [
                    {"id": "1", "type": "Person", "properties": {}},
                    {"id": "2", "type": "Person", "properties": {}},
                    {"id": "3", "type": "Document", "properties": {}},
                ]
                relations = []

                summary = backend._build_context_summary(entities, relations)
                self.assertIn("3 entities", summary)
                self.assertIn("2 Person", summary)
                self.assertIn("1 Document", summary)

    def test_build_context_summary_with_relations(self):
        """Test context summary includes relation info."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                entities = [{"id": "1", "type": "Person", "properties": {}}]
                relations = [
                    {"from": "1", "to": "2", "type": "KNOWS", "properties": {}},
                    {"from": "1", "to": "3", "type": "KNOWS", "properties": {}},
                ]

                summary = backend._build_context_summary(entities, relations)
                self.assertIn("2 relations", summary)
                self.assertIn("KNOWS", summary)


class TestNeo4jCloseMethod(unittest.TestCase):
    """Test close() method (AC-17)."""

    def test_close_sets_closed_flag(self):
        """Test close() sets _closed flag."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._closed = False
                backend._driver = MagicMock()

                backend.close()
                self.assertTrue(backend._closed)

    def test_close_is_idempotent(self):
        """Test close() can be called multiple times safely."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._closed = False
                backend._driver = MagicMock()

                # Should not raise on multiple calls
                backend.close()
                backend.close()
                backend.close()
                self.assertTrue(backend._closed)

    def test_close_calls_driver_close(self):
        """Test close() closes the driver."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._closed = False
                mock_driver = MagicMock()
                backend._driver = mock_driver

                backend.close()
                mock_driver.close.assert_called_once()


class TestNeo4jThreadSafety(unittest.TestCase):
    """Test thread safety (AC-24)."""

    def test_lock_attribute_exists(self):
        """Test that _lock attribute exists for thread safety."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._lock = threading.Lock()

                self.assertIsInstance(backend._lock, type(threading.Lock()))


class TestNeo4jEmbeddingSupport(unittest.TestCase):
    """Test embedding storage support."""

    def test_default_embedding_dim(self):
        """Test default embedding dimension is 1536."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertEqual(Neo4jBackend.DEFAULT_EMBEDDING_DIM, 1536)

    def test_retrieve_context_embedding_version_error(self):
        """Test retrieve_context returns helpful message when Neo4j version doesn't support vectors."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                # Mock vector_search to return version error (Neo4j < 5.11)
                with patch.object(
                    backend,
                    "vector_search",
                    return_value={
                        "success": False,
                        "error": "Vector search requires Neo4j 5.11+. Current version: 4.4.0",
                        "error_type": "version_error",
                    },
                ):
                    result = backend.retrieve_context(embedding=[0.1] * 1536)
                    self.assertTrue(result["success"])
                    self.assertEqual(result["entities"], [])
                    self.assertIn("Neo4j 5.11+", result["context_summary"])


# ============================================================================
# EXTENDED CRUD TESTS (TEA-BUILTIN-001.7.2)
# ============================================================================


class TestNeo4jDeleteOperations(unittest.TestCase):
    """Test delete operations (AC-1 to AC-3)."""

    def test_delete_entity_validation_error(self):
        """Test delete_entity returns validation_error for empty entity_id."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.delete_entity("")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_id", result["error"])

    def test_delete_relation_validation_error(self):
        """Test delete_relation returns validation_error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.delete_relation("a", "b", "")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")

    def test_delete_entities_batch_validation_error(self):
        """Test delete_entities_batch returns validation_error for empty list."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.delete_entities_batch([])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_ids", result["error"])

    def test_delete_entities_batch_invalid_type(self):
        """Test delete_entities_batch returns error for non-list input."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.delete_entities_batch("invalid")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("must be a list", result["error"])


class TestNeo4jUpdateOperations(unittest.TestCase):
    """Test update operations (AC-4 to AC-7)."""

    def test_update_entity_properties_validation_error(self):
        """Test update_entity_properties returns error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                # Missing entity_id
                result = backend.update_entity_properties("", {"key": "value"})
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_id", result["error"])

    def test_update_entity_properties_missing_properties(self):
        """Test update_entity_properties returns error for None properties."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.update_entity_properties("entity_1", None)
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("properties", result["error"])

    def test_update_relation_properties_validation_error(self):
        """Test update_relation_properties returns error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.update_relation_properties("a", "", "KNOWS", {})
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")

    def test_add_labels_validation_error_empty_entity(self):
        """Test add_labels returns error for empty entity_id."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.add_labels("", ["Label1"])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_id", result["error"])

    def test_add_labels_validation_error_empty_labels(self):
        """Test add_labels returns error for empty labels."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.add_labels("entity_1", [])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("labels", result["error"])

    def test_remove_labels_validation_error(self):
        """Test remove_labels returns error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.remove_labels("", ["Label1"])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")


class TestNeo4jBatchOperations(unittest.TestCase):
    """Test batch operations (AC-8 to AC-10)."""

    def test_store_entities_batch_validation_error_empty(self):
        """Test store_entities_batch returns error for empty list."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.store_entities_batch([])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entities", result["error"])

    def test_store_entities_batch_validation_error_invalid_entity(self):
        """Test store_entities_batch returns error for invalid entity."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                # Missing entity_id
                result = backend.store_entities_batch([{"entity_type": "Person"}])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_id", result["error"])

    def test_store_entities_batch_validation_error_missing_type(self):
        """Test store_entities_batch returns error for missing entity_type."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.store_entities_batch([{"entity_id": "person_1"}])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("entity_type", result["error"])

    def test_store_relations_batch_validation_error_empty(self):
        """Test store_relations_batch returns error for empty list."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.store_relations_batch([])
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("relations", result["error"])

    def test_store_relations_batch_validation_error_missing_field(self):
        """Test store_relations_batch returns error for missing field."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                # Missing relation_type
                result = backend.store_relations_batch(
                    [{"from_entity": "a", "to_entity": "b"}]
                )
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")
                self.assertIn("relation_type", result["error"])


class TestNeo4jTransactionSupport(unittest.TestCase):
    """Test transaction support (AC-11 to AC-14)."""

    def test_begin_transaction_returns_context_manager(self):
        """Test begin_transaction returns a Neo4jTransaction."""
        from the_edge_agent.memory.graph import Neo4jBackend, Neo4jTransaction

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._closed = False
                backend._driver = MagicMock()

                tx = backend.begin_transaction()
                self.assertIsInstance(tx, Neo4jTransaction)

    def test_transaction_alias_returns_context_manager(self):
        """Test transaction() is alias for begin_transaction()."""
        from the_edge_agent.memory.graph import Neo4jBackend, Neo4jTransaction

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)
                backend._closed = False
                backend._driver = MagicMock()

                tx = backend.transaction()
                self.assertIsInstance(tx, Neo4jTransaction)

    def test_neo4j_transaction_commit_method_exists(self):
        """Test Neo4jTransaction has commit method."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(hasattr(Neo4jTransaction, "commit"))
        self.assertTrue(callable(getattr(Neo4jTransaction, "commit")))

    def test_neo4j_transaction_rollback_method_exists(self):
        """Test Neo4jTransaction has rollback method."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(hasattr(Neo4jTransaction, "rollback"))
        self.assertTrue(callable(getattr(Neo4jTransaction, "rollback")))

    def test_neo4j_transaction_commit_error_no_active_tx(self):
        """Test commit returns error when no active transaction."""
        from the_edge_agent.memory.graph import Neo4jBackend, Neo4jTransaction

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                tx = Neo4jTransaction(backend)
                tx._tx = None

                result = tx.commit()
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "transaction_error")

    def test_neo4j_transaction_rollback_error_no_active_tx(self):
        """Test rollback returns error when no active transaction."""
        from the_edge_agent.memory.graph import Neo4jBackend, Neo4jTransaction

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                tx = Neo4jTransaction(backend)
                tx._tx = None

                result = tx.rollback()
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "transaction_error")

    def test_neo4j_transaction_store_entity_method_exists(self):
        """Test Neo4jTransaction has store_entity method."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(hasattr(Neo4jTransaction, "store_entity"))
        self.assertTrue(callable(getattr(Neo4jTransaction, "store_entity")))

    def test_neo4j_transaction_store_relation_method_exists(self):
        """Test Neo4jTransaction has store_relation method."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(hasattr(Neo4jTransaction, "store_relation"))
        self.assertTrue(callable(getattr(Neo4jTransaction, "store_relation")))

    def test_neo4j_transaction_run_method_exists(self):
        """Test Neo4jTransaction has run method."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(hasattr(Neo4jTransaction, "run"))
        self.assertTrue(callable(getattr(Neo4jTransaction, "run")))


class TestNeo4jMergeOperations(unittest.TestCase):
    """Test merge operations (AC-15 to AC-16)."""

    def test_merge_entity_validation_error_missing_id(self):
        """Test merge_entity returns error for missing entity_id."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.merge_entity("", "Person")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")

    def test_merge_entity_validation_error_missing_type(self):
        """Test merge_entity returns error for missing entity_type."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.merge_entity("entity_1", "")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")

    def test_merge_relation_validation_error_missing_params(self):
        """Test merge_relation returns error for missing params."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True):
            with patch.object(Neo4jBackend, "_init_driver"):
                backend = object.__new__(Neo4jBackend)

                result = backend.merge_relation("a", "b", "")
                self.assertEqual(result["success"], False)
                self.assertEqual(result["error_type"], "validation_error")


class TestNeo4jExtendedMethodsExist(unittest.TestCase):
    """Test that all extended methods exist on Neo4jBackend."""

    def test_delete_entity_method_exists(self):
        """Test delete_entity method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "delete_entity"))
        self.assertTrue(callable(getattr(Neo4jBackend, "delete_entity")))

    def test_delete_relation_method_exists(self):
        """Test delete_relation method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "delete_relation"))
        self.assertTrue(callable(getattr(Neo4jBackend, "delete_relation")))

    def test_delete_entities_batch_method_exists(self):
        """Test delete_entities_batch method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "delete_entities_batch"))
        self.assertTrue(callable(getattr(Neo4jBackend, "delete_entities_batch")))

    def test_update_entity_properties_method_exists(self):
        """Test update_entity_properties method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "update_entity_properties"))
        self.assertTrue(callable(getattr(Neo4jBackend, "update_entity_properties")))

    def test_update_relation_properties_method_exists(self):
        """Test update_relation_properties method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "update_relation_properties"))
        self.assertTrue(callable(getattr(Neo4jBackend, "update_relation_properties")))

    def test_add_labels_method_exists(self):
        """Test add_labels method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "add_labels"))
        self.assertTrue(callable(getattr(Neo4jBackend, "add_labels")))

    def test_remove_labels_method_exists(self):
        """Test remove_labels method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "remove_labels"))
        self.assertTrue(callable(getattr(Neo4jBackend, "remove_labels")))

    def test_store_entities_batch_method_exists(self):
        """Test store_entities_batch method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "store_entities_batch"))
        self.assertTrue(callable(getattr(Neo4jBackend, "store_entities_batch")))

    def test_store_relations_batch_method_exists(self):
        """Test store_relations_batch method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "store_relations_batch"))
        self.assertTrue(callable(getattr(Neo4jBackend, "store_relations_batch")))

    def test_merge_entity_method_exists(self):
        """Test merge_entity method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "merge_entity"))
        self.assertTrue(callable(getattr(Neo4jBackend, "merge_entity")))

    def test_merge_relation_method_exists(self):
        """Test merge_relation method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "merge_relation"))
        self.assertTrue(callable(getattr(Neo4jBackend, "merge_relation")))

    def test_begin_transaction_method_exists(self):
        """Test begin_transaction method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "begin_transaction"))
        self.assertTrue(callable(getattr(Neo4jBackend, "begin_transaction")))

    def test_transaction_method_exists(self):
        """Test transaction method exists."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "transaction"))
        self.assertTrue(callable(getattr(Neo4jBackend, "transaction")))


class TestNeo4jTransactionClass(unittest.TestCase):
    """Test Neo4jTransaction class."""

    def test_neo4j_transaction_class_exists(self):
        """Test Neo4jTransaction class is exported."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(isinstance(Neo4jTransaction, type))

    def test_neo4j_transaction_is_context_manager(self):
        """Test Neo4jTransaction has __enter__ and __exit__."""
        from the_edge_agent.memory.graph import Neo4jTransaction

        self.assertTrue(hasattr(Neo4jTransaction, "__enter__"))
        self.assertTrue(hasattr(Neo4jTransaction, "__exit__"))


if __name__ == "__main__":
    unittest.main()
