"""
Unit tests for Neo4j APOC Trigger Support (TEA-BUILTIN-001.7.5).

Tests cover:
- APOC detection (AC-1 to AC-4)
- Trigger registration (AC-5 to AC-10)
- Trigger execution via Cypher (AC-11 to AC-12)
- Callback mechanisms (AC-13 to AC-14)
- Lifecycle management (AC-15 to AC-17)
- Configuration (AC-18)
- Action registration (AC-19)
- Error handling (AC-20 to AC-22)
- Thread safety (AC-23)
"""

import unittest
from unittest.mock import MagicMock, patch, PropertyMock


class TestNeo4jAPOCDetection(unittest.TestCase):
    """Test APOC detection functionality (AC-1 to AC-4)."""

    def test_check_apoc_available_method_exists(self):
        """Test that check_apoc_available method exists (AC-1)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "check_apoc_available"))
        self.assertTrue(callable(getattr(Neo4jBackend, "check_apoc_available")))

    def test_get_apoc_version_method_exists(self):
        """Test that get_apoc_version method exists (AC-2)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "get_apoc_version"))
        self.assertTrue(callable(getattr(Neo4jBackend, "get_apoc_version")))

    def test_check_triggers_enabled_method_exists(self):
        """Test that check_triggers_enabled method exists (AC-3)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "check_triggers_enabled"))
        self.assertTrue(callable(getattr(Neo4jBackend, "check_triggers_enabled")))

    def test_apoc_available_property_exists(self):
        """Test that APOC_AVAILABLE property exists (AC-4)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "APOC_AVAILABLE"))

    def test_triggers_enabled_property_exists(self):
        """Test that TRIGGERS_ENABLED property exists (AC-4)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "TRIGGERS_ENABLED"))

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_check_apoc_returns_dict_format(self):
        """Test check_apoc_available returns proper dict format."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()
            backend._closed = False

            # Mock _execute_with_retry to simulate APOC not available
            with patch.object(
                backend, "_execute_with_retry", side_effect=Exception("APOC not found")
            ):
                result = backend.check_apoc_available()

            self.assertIn("success", result)
            self.assertIn("available", result)
            self.assertIn("version", result)
            self.assertTrue(result["success"])
            self.assertFalse(result["available"])

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_get_apoc_version_when_unavailable(self):
        """Test get_apoc_version returns error when APOC unavailable."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()
            backend._closed = False

            with patch.object(
                backend,
                "check_apoc_available",
                return_value={"success": True, "available": False, "version": None},
            ):
                result = backend.get_apoc_version()

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "dependency_missing")
            self.assertIn("APOC", result["error"])


class TestNeo4jTriggerRegistration(unittest.TestCase):
    """Test trigger registration functionality (AC-5 to AC-10)."""

    def test_register_trigger_method_exists(self):
        """Test that register_trigger method exists (AC-5)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "register_trigger"))
        self.assertTrue(callable(getattr(Neo4jBackend, "register_trigger")))

    def test_trigger_selectors_constant_exists(self):
        """Test that TRIGGER_SELECTORS constant exists (AC-6)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "TRIGGER_SELECTORS"))
        selectors = Neo4jBackend.TRIGGER_SELECTORS
        self.assertIn("createdNodes", selectors)
        self.assertIn("createdRelationships", selectors)
        self.assertIn("deletedNodes", selectors)
        self.assertIn("deletedRelationships", selectors)
        self.assertIn("assignedLabels", selectors)
        self.assertIn("removedLabels", selectors)
        self.assertIn("assignedNodeProperties", selectors)
        self.assertIn("assignedRelationshipProperties", selectors)

    def test_unregister_trigger_method_exists(self):
        """Test that unregister_trigger method exists (AC-7)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "unregister_trigger"))
        self.assertTrue(callable(getattr(Neo4jBackend, "unregister_trigger")))

    def test_list_triggers_method_exists(self):
        """Test that list_triggers method exists (AC-8)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "list_triggers"))
        self.assertTrue(callable(getattr(Neo4jBackend, "list_triggers")))

    def test_pause_trigger_method_exists(self):
        """Test that pause_trigger method exists (AC-9)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "pause_trigger"))
        self.assertTrue(callable(getattr(Neo4jBackend, "pause_trigger")))

    def test_resume_trigger_method_exists(self):
        """Test that resume_trigger method exists (AC-10)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "resume_trigger"))
        self.assertTrue(callable(getattr(Neo4jBackend, "resume_trigger")))

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_trigger_validation_name_required(self):
        """Test register_trigger validates name is required."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            # Mock properties
            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)
            type(backend).TRIGGERS_ENABLED = PropertyMock(return_value=True)

            result = backend.register_trigger(name="", query="RETURN 1")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")
            self.assertIn("name", result["error"].lower())

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_trigger_validation_query_required(self):
        """Test register_trigger validates query is required."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)
            type(backend).TRIGGERS_ENABLED = PropertyMock(return_value=True)

            result = backend.register_trigger(name="test", query="")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")
            self.assertIn("query", result["error"].lower())

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_trigger_apoc_not_available_error(self):
        """Test register_trigger returns error when APOC unavailable (AC-20)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=False)

            result = backend.register_trigger(name="test", query="RETURN 1")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "dependency_missing")

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_trigger_triggers_not_enabled_error(self):
        """Test register_trigger returns error when triggers disabled (AC-21)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)
            type(backend).TRIGGERS_ENABLED = PropertyMock(return_value=False)

            result = backend.register_trigger(name="test", query="RETURN 1")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration_error")
            self.assertIn("apoc.trigger.enabled", result["error"].lower())

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_unregister_trigger_validation(self):
        """Test unregister_trigger validates name."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)

            result = backend.unregister_trigger("")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_pause_trigger_validation(self):
        """Test pause_trigger validates name."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)

            result = backend.pause_trigger("")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_resume_trigger_validation(self):
        """Test resume_trigger validates name."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)

            result = backend.resume_trigger("")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")


class TestNeo4jCallbackMechanisms(unittest.TestCase):
    """Test callback mechanisms (AC-13 to AC-14)."""

    def test_register_trigger_callback_method_exists(self):
        """Test that register_trigger_callback method exists (AC-13)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "register_trigger_callback"))
        self.assertTrue(callable(getattr(Neo4jBackend, "register_trigger_callback")))

    def test_register_trigger_state_update_method_exists(self):
        """Test that register_trigger_state_update method exists (AC-14)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "register_trigger_state_update"))
        self.assertTrue(
            callable(getattr(Neo4jBackend, "register_trigger_state_update"))
        )

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_callback_validation_name(self):
        """Test register_trigger_callback validates name."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            result = backend.register_trigger_callback("", "http://example.com/webhook")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")
            self.assertIn("name", result["error"].lower())

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_callback_validation_url(self):
        """Test register_trigger_callback validates callback_url."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            result = backend.register_trigger_callback("test", "")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")
            self.assertIn("url", result["error"].lower())

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_state_update_validation_name(self):
        """Test register_trigger_state_update validates name."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            result = backend.register_trigger_state_update("", "my_key")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")
            self.assertIn("name", result["error"].lower())

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_state_update_validation_state_key(self):
        """Test register_trigger_state_update validates state_key."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            result = backend.register_trigger_state_update("test", "")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")
            self.assertIn("state key", result["error"].lower())


class TestNeo4jLifecycleManagement(unittest.TestCase):
    """Test lifecycle management (AC-15 to AC-17)."""

    def test_cleanup_triggers_method_exists(self):
        """Test that cleanup_triggers method exists (AC-15)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        self.assertTrue(hasattr(Neo4jBackend, "cleanup_triggers"))
        self.assertTrue(callable(getattr(Neo4jBackend, "cleanup_triggers")))

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_cleanup_triggers_apoc_not_available(self):
        """Test cleanup_triggers returns error when APOC unavailable."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=False)

            result = backend.cleanup_triggers()
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "dependency_missing")

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_cleanup_triggers_with_prefix(self):
        """Test cleanup_triggers respects prefix filter (AC-16)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)

            # Mock list_triggers to return test data
            with patch.object(
                backend,
                "list_triggers",
                return_value={
                    "success": True,
                    "triggers": [
                        {"name": "session_1_log"},
                        {"name": "session_1_notify"},
                        {"name": "session_2_log"},
                    ],
                    "count": 3,
                },
            ):
                # Mock unregister to always succeed
                with patch.object(
                    backend,
                    "unregister_trigger",
                    return_value={"success": True, "removed": True},
                ) as mock_unregister:
                    result = backend.cleanup_triggers(prefix="session_1_")

                    self.assertTrue(result["success"])
                    # Should only have unregistered 2 triggers (session_1_*)
                    self.assertEqual(mock_unregister.call_count, 2)
                    self.assertEqual(result["count"], 2)
                    self.assertIn("session_1_log", result["removed"])
                    self.assertIn("session_1_notify", result["removed"])


class TestNeo4jTriggerActions(unittest.TestCase):
    """Test YAML action registration (AC-19)."""

    def test_neo4j_trigger_actions_module_exists(self):
        """Test that neo4j_trigger_actions module exists."""
        from the_edge_agent.actions import neo4j_trigger_actions

        self.assertTrue(hasattr(neo4j_trigger_actions, "register_actions"))

    def test_actions_registered_in_build_registry(self):
        """Test that trigger actions are registered in build_actions_registry."""
        from the_edge_agent.actions import build_actions_registry

        # Create a mock engine
        mock_engine = MagicMock()
        mock_engine._memory_backend = None
        mock_engine._graph_backend = None
        mock_engine._ltm_backend = None
        mock_engine._checkpointer = None
        mock_engine._trace_context = None
        mock_engine.actions_registry = {}

        registry = build_actions_registry(mock_engine)

        # Check that neo4j trigger actions are registered
        expected_actions = [
            "neo4j.check_apoc",
            "neo4j.check_triggers",
            "neo4j.register_trigger",
            "neo4j.unregister_trigger",
            "neo4j.list_triggers",
            "neo4j.pause_trigger",
            "neo4j.resume_trigger",
            "neo4j.register_callback",
            "neo4j.register_state_update",
            "neo4j.cleanup_triggers",
        ]

        for action in expected_actions:
            self.assertIn(action, registry, f"Missing action: {action}")

    def test_actions_return_error_when_no_backend(self):
        """Test that actions return appropriate error when no Neo4j backend."""
        from the_edge_agent.actions import build_actions_registry

        mock_engine = MagicMock()
        mock_engine._memory_backend = None
        mock_engine._graph_backend = None  # No Neo4j backend
        mock_engine._ltm_backend = None
        mock_engine._checkpointer = None
        mock_engine._trace_context = None
        mock_engine.actions_registry = {}

        registry = build_actions_registry(mock_engine)

        # Call an action without Neo4j backend
        result = registry["neo4j.check_apoc"](state={})
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "configuration_error")


class TestNeo4jTriggerErrorHandling(unittest.TestCase):
    """Test error handling (AC-20 to AC-22)."""

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_register_trigger_handles_exceptions(self):
        """Test register_trigger handles exceptions gracefully (AC-22)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()
            backend._closed = False

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)
            type(backend).TRIGGERS_ENABLED = PropertyMock(return_value=True)

            # Mock _execute_with_retry to raise an exception
            with patch.object(
                backend,
                "_execute_with_retry",
                side_effect=Exception("Connection failed"),
            ):
                result = backend.register_trigger(name="test", query="RETURN 1")

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "query_error")
            self.assertIn("Failed to register trigger", result["error"])

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_list_triggers_handles_exceptions(self):
        """Test list_triggers handles exceptions gracefully."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = MagicMock()
            backend._closed = False

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)

            with patch.object(
                backend, "_execute_with_retry", side_effect=Exception("Query failed")
            ):
                result = backend.list_triggers()

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "query_error")


class TestNeo4jTriggerThreadSafety(unittest.TestCase):
    """Test thread safety (AC-23)."""

    @patch("the_edge_agent.memory.graph.NEO4J_AVAILABLE", True)
    def test_trigger_methods_use_lock(self):
        """Test that trigger methods use thread lock for safety."""
        from the_edge_agent.memory.graph import Neo4jBackend
        import threading

        with patch.object(Neo4jBackend, "_init_driver"):
            backend = object.__new__(Neo4jBackend)
            backend._lock = threading.Lock()
            backend._closed = False

            type(backend).APOC_AVAILABLE = PropertyMock(return_value=True)
            type(backend).TRIGGERS_ENABLED = PropertyMock(return_value=True)

            # Mock _execute_with_retry
            mock_result = MagicMock()
            mock_result.return_value = {"name": "test", "installed": True}

            with patch.object(
                backend,
                "_execute_with_retry",
                return_value={"name": "test", "installed": True},
            ):
                # This should complete without deadlock
                result = backend.register_trigger(name="test", query="RETURN 1")
                self.assertTrue(result["success"])


class TestNeo4jTriggerDocumentation(unittest.TestCase):
    """Test documentation completeness (AC-12)."""

    def test_register_trigger_docstring_contains_context_vars(self):
        """Test register_trigger documents transaction context variables (AC-12)."""
        from the_edge_agent.memory.graph import Neo4jBackend

        docstring = Neo4jBackend.register_trigger.__doc__
        self.assertIsNotNone(docstring)

        # Check that context variables are documented
        self.assertIn("createdNodes", docstring)
        self.assertIn("deletedNodes", docstring)
        self.assertIn("createdRelationships", docstring)
        self.assertIn("deletedRelationships", docstring)


if __name__ == "__main__":
    unittest.main()
