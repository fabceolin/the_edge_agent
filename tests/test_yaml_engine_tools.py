"""
Tests for Tools Bridge Actions (TEA-BUILTIN-002.3).

This module tests the tools bridge actions that connect to external tool ecosystems:
- tools.crewai: CrewAI tool execution
- tools.mcp: MCP server tool execution
- tools.langchain: LangChain tool execution
- tools.discover: Tool discovery from all sources

Tests use mocking to avoid requiring actual tool installations.
"""

import unittest
from unittest.mock import Mock, patch, MagicMock
import sys

# Check which dependencies are available
try:
    import crewai_tools
    HAS_CREWAI = True
except ImportError:
    HAS_CREWAI = False

try:
    import mcp
    HAS_MCP = True
except ImportError:
    HAS_MCP = False

try:
    import langchain
    HAS_LANGCHAIN = True
except ImportError:
    HAS_LANGCHAIN = False


class TestToolsBridgeActions(unittest.TestCase):
    """Tests for tools bridge actions."""

    def setUp(self):
        """Set up test fixtures."""
        from the_edge_agent import YAMLEngine
        self.engine = YAMLEngine()

    # ========================================================================
    # P0 - Critical Tests: CrewAI
    # ========================================================================

    def test_tools_crewai_not_installed(self):
        """Test graceful handling when CrewAI is not installed (P0)."""
        # Mock crewai_tools as not available
        with patch.dict('sys.modules', {'crewai_tools': None}):
            # Force reimport of tools_actions to pick up the mock
            from the_edge_agent.actions import tools_actions

            # Patch the HAS_CREWAI flag
            original_flag = tools_actions.HAS_CREWAI
            tools_actions.HAS_CREWAI = False

            try:
                result = self.engine.actions_registry['tools.crewai'](
                    state={},
                    tool="SerperDevTool",
                    query="test"
                )

                self.assertFalse(result['success'])
                self.assertEqual(result['error_type'], 'import')
                self.assertIn('CrewAI not installed', result['error'])
                self.assertEqual(result['tool'], 'SerperDevTool')
            finally:
                tools_actions.HAS_CREWAI = original_flag

    @unittest.skipUnless(HAS_CREWAI, "CrewAI not installed")
    def test_tools_crewai_basic(self):
        """Test basic CrewAI tool execution (P0)."""
        # This test only runs if CrewAI is actually installed
        # Mock a specific tool
        mock_tool = MagicMock()
        mock_tool._run = MagicMock(return_value="Test result")

        with patch('the_edge_agent.actions.tools_actions._get_crewai_tool') as mock_get:
            mock_get.return_value = mock_tool

            result = self.engine.actions_registry['tools.crewai'](
                state={},
                tool="SerperDevTool",
                query="test"
            )

            self.assertTrue(result['success'])
            self.assertEqual(result['result'], "Test result")
            self.assertEqual(result['tool'], "SerperDevTool")

    def test_tools_crewai_execution_error(self):
        """Test CrewAI error handling during execution (P0)."""
        from the_edge_agent.actions import tools_actions

        if not tools_actions.HAS_CREWAI:
            self.skipTest("CrewAI not installed")

        mock_tool = MagicMock()
        mock_tool._run = MagicMock(side_effect=RuntimeError("Tool execution failed"))

        with patch('the_edge_agent.actions.tools_actions._get_crewai_tool') as mock_get:
            mock_get.return_value = mock_tool

            result = self.engine.actions_registry['tools.crewai'](
                state={},
                tool="SerperDevTool",
                query="test"
            )

            self.assertFalse(result['success'])
            self.assertEqual(result['error_type'], 'execution')
            self.assertIn('Tool execution failed', result['error'])

    # ========================================================================
    # P0 - Critical Tests: MCP
    # ========================================================================

    def test_tools_mcp_not_installed(self):
        """Test graceful handling when MCP is not installed (P0)."""
        from the_edge_agent.actions import tools_actions

        original_flag = tools_actions.HAS_MCP
        tools_actions.HAS_MCP = False

        try:
            result = self.engine.actions_registry['tools.mcp'](
                state={},
                server={"command": "npx", "args": ["-y", "@test/server"]},
                tool="read_file",
                path="/tmp/test.txt"
            )

            self.assertFalse(result['success'])
            self.assertEqual(result['error_type'], 'import')
            self.assertIn('MCP not installed', result['error'])
            self.assertEqual(result['tool'], 'read_file')
        finally:
            tools_actions.HAS_MCP = original_flag

    @unittest.skipUnless(HAS_MCP, "MCP not installed")
    def test_tools_mcp_basic(self):
        """Test basic MCP tool execution (P0)."""
        # This test only runs if MCP is actually installed
        # We still mock the actual connection
        mock_result = MagicMock()
        mock_result.content = [MagicMock(text="File contents")]

        with patch('the_edge_agent.actions.tools_actions._mcp_connect_and_execute') as mock_exec:
            mock_exec.return_value = {
                "result": "File contents",
                "tool": "read_file",
                "success": True
            }

            result = self.engine.actions_registry['tools.mcp'](
                state={},
                server={"command": "npx", "args": ["-y", "@test/server"]},
                tool="read_file",
                path="/tmp/test.txt"
            )

            self.assertTrue(result['success'])
            self.assertEqual(result['result'], "File contents")

    # ========================================================================
    # P0 - Critical Tests: LangChain
    # ========================================================================

    def test_tools_langchain_not_installed(self):
        """Test graceful handling when LangChain is not installed (P0)."""
        from the_edge_agent.actions import tools_actions

        original_flag = tools_actions.HAS_LANGCHAIN
        tools_actions.HAS_LANGCHAIN = False

        try:
            result = self.engine.actions_registry['tools.langchain'](
                state={},
                tool="DuckDuckGoSearchRun",
                query="test"
            )

            self.assertFalse(result['success'])
            self.assertEqual(result['error_type'], 'import')
            self.assertIn('LangChain not installed', result['error'])
            self.assertEqual(result['tool'], 'DuckDuckGoSearchRun')
        finally:
            tools_actions.HAS_LANGCHAIN = original_flag

    @unittest.skipUnless(HAS_LANGCHAIN, "LangChain not installed")
    def test_tools_langchain_basic(self):
        """Test basic LangChain tool execution (P0)."""
        mock_tool = MagicMock()
        mock_tool.invoke = MagicMock(return_value="Search results")

        with patch('the_edge_agent.actions.tools_actions._get_langchain_tool') as mock_get:
            mock_get.return_value = mock_tool

            result = self.engine.actions_registry['tools.langchain'](
                state={},
                tool="DuckDuckGoSearchRun",
                query="test"
            )

            self.assertTrue(result['success'])
            self.assertEqual(result['result'], "Search results")
            self.assertEqual(result['tool'], "DuckDuckGoSearchRun")

    def test_tools_langchain_execution_error(self):
        """Test LangChain error handling during execution (P0)."""
        from the_edge_agent.actions import tools_actions

        if not tools_actions.HAS_LANGCHAIN:
            self.skipTest("LangChain not installed")

        mock_tool = MagicMock()
        mock_tool.invoke = MagicMock(side_effect=RuntimeError("Search failed"))

        with patch('the_edge_agent.actions.tools_actions._get_langchain_tool') as mock_get:
            mock_get.return_value = mock_tool

            result = self.engine.actions_registry['tools.langchain'](
                state={},
                tool="DuckDuckGoSearchRun",
                query="test"
            )

            self.assertFalse(result['success'])
            self.assertEqual(result['error_type'], 'execution')
            self.assertIn('Search failed', result['error'])

    # ========================================================================
    # P1 - Core Functionality Tests
    # ========================================================================

    def test_dual_namespace_access(self):
        """Test actions.tools_* namespace (P1 - AC8)."""
        # Verify both namespaces exist
        self.assertIn('tools.crewai', self.engine.actions_registry)
        self.assertIn('actions.tools_crewai', self.engine.actions_registry)
        self.assertIn('tools.mcp', self.engine.actions_registry)
        self.assertIn('actions.tools_mcp', self.engine.actions_registry)
        self.assertIn('tools.langchain', self.engine.actions_registry)
        self.assertIn('actions.tools_langchain', self.engine.actions_registry)
        self.assertIn('tools.discover', self.engine.actions_registry)
        self.assertIn('actions.tools_discover', self.engine.actions_registry)

        # Verify they point to the same function
        self.assertIs(
            self.engine.actions_registry['tools.crewai'],
            self.engine.actions_registry['actions.tools_crewai']
        )

    def test_action_pattern_compliance(self):
        """Test that actions follow tea pattern: state in, dict out (P1 - AC5)."""
        from the_edge_agent.actions import tools_actions

        # Test with mocked execution
        original_flag = tools_actions.HAS_CREWAI
        tools_actions.HAS_CREWAI = False

        try:
            result = self.engine.actions_registry['tools.crewai'](
                state={"test": "value"},
                tool="TestTool"
            )

            # Result must be a dict
            self.assertIsInstance(result, dict)

            # Must have required keys
            self.assertIn('success', result)
            self.assertIn('tool', result)

            # On failure, must have error info
            if not result['success']:
                self.assertIn('error', result)
                self.assertIn('error_type', result)
        finally:
            tools_actions.HAS_CREWAI = original_flag

    def test_non_dict_result_wrapped(self):
        """Test that non-dict results are wrapped (P1)."""
        from the_edge_agent.actions.tools_actions import normalize_result

        # String result
        result = normalize_result("test string", "TestTool")
        self.assertIsInstance(result, dict)
        self.assertEqual(result['result'], "test string")
        self.assertEqual(result['tool'], "TestTool")
        self.assertTrue(result['success'])

        # Number result
        result = normalize_result(42, "TestTool")
        self.assertEqual(result['result'], 42)

        # List result
        result = normalize_result([1, 2, 3], "TestTool")
        self.assertEqual(result['result'], [1, 2, 3])

        # Dict result (should preserve and add missing keys)
        result = normalize_result({"data": "value"}, "TestTool")
        self.assertEqual(result['data'], "value")
        self.assertEqual(result['tool'], "TestTool")
        self.assertTrue(result['success'])

    def test_tools_discover_all(self):
        """Test discovering tools from all sources (P1)."""
        from the_edge_agent.actions import tools_actions

        # Mock all discovery functions to return test data
        with patch.object(tools_actions, '_discover_crewai_tools') as mock_crewai, \
             patch.object(tools_actions, '_discover_langchain_tools') as mock_langchain, \
             patch.object(tools_actions, 'run_async_in_sync') as mock_async:

            mock_crewai.return_value = [
                {"name": "CrewTool", "description": "A CrewAI tool", "parameters": {}, "source": "crewai"}
            ]
            mock_langchain.return_value = [
                {"name": "LangTool", "description": "A LangChain tool", "parameters": {}, "source": "langchain"}
            ]
            mock_async.return_value = []  # No MCP tools

            result = self.engine.actions_registry['tools.discover'](
                state={},
                source="all",
                use_cache=False
            )

            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 2)
            self.assertIn('crewai', result['sources'])
            self.assertIn('langchain', result['sources'])

            # Check tool list
            tool_names = [t['name'] for t in result['tools']]
            self.assertIn('CrewTool', tool_names)
            self.assertIn('LangTool', tool_names)

    def test_tools_discover_filtered(self):
        """Test discovering tools with filter (P2)."""
        from the_edge_agent.actions import tools_actions

        with patch.object(tools_actions, '_discover_crewai_tools') as mock_crewai:
            mock_crewai.return_value = [
                {"name": "SearchTool", "description": "Search", "parameters": {}, "source": "crewai"},
                {"name": "FileTool", "description": "File ops", "parameters": {}, "source": "crewai"}
            ]

            result = self.engine.actions_registry['tools.discover'](
                state={},
                source="crewai",
                filter="Search",
                use_cache=False
            )

            self.assertTrue(result['success'])
            # Filter should have been passed to discovery function
            mock_crewai.assert_called_once_with("Search")

    def test_discovery_cache_hit(self):
        """Test that discovery results are cached (P2)."""
        from the_edge_agent.actions import tools_actions

        # Clear any existing cache
        tools_actions._discovery_cache.clear()

        with patch.object(tools_actions, '_discover_crewai_tools') as mock_crewai:
            mock_crewai.return_value = [
                {"name": "CachedTool", "description": "Cached", "parameters": {}, "source": "crewai"}
            ]

            # First call should invoke discovery
            result1 = self.engine.actions_registry['tools.discover'](
                state={},
                source="crewai",
                use_cache=True
            )
            self.assertEqual(mock_crewai.call_count, 1)

            # Second call should use cache
            result2 = self.engine.actions_registry['tools.discover'](
                state={},
                source="crewai",
                use_cache=True
            )
            self.assertEqual(mock_crewai.call_count, 1)  # Should not have increased

            # Results should be the same
            self.assertEqual(result1['tools'], result2['tools'])

    def test_tools_clear_cache(self):
        """Test cache clearing functionality (P2)."""
        from the_edge_agent.actions import tools_actions

        # Set up some cache entries
        tools_actions._discovery_cache.set("crewai", [{"name": "Tool1"}])
        tools_actions._discovery_cache.set("langchain", [{"name": "Tool2"}])

        # Clear specific source
        result = self.engine.actions_registry['tools.clear_cache'](
            state={},
            source="crewai"
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['source'], "crewai")

        # Verify only crewai is cleared
        self.assertIsNone(tools_actions._discovery_cache.get("crewai"))
        self.assertIsNotNone(tools_actions._discovery_cache.get("langchain"))

        # Clear all
        result = self.engine.actions_registry['tools.clear_cache'](state={})
        self.assertTrue(result['success'])
        self.assertIsNone(result['source'])
        self.assertIsNone(tools_actions._discovery_cache.get("langchain"))

    # ========================================================================
    # Schema Translation Tests
    # ========================================================================

    def test_create_tool_schema(self):
        """Test tool schema creation (P1)."""
        from the_edge_agent.actions.tools_actions import create_tool_schema

        schema = create_tool_schema(
            name="TestTool",
            description="A test tool",
            parameters={
                "query": {"type": "string", "description": "Search query", "required": True},
                "limit": {"type": "integer", "description": "Max results", "required": False}
            },
            source="test"
        )

        self.assertEqual(schema['name'], "TestTool")
        self.assertEqual(schema['description'], "A test tool")
        self.assertEqual(schema['source'], "test")
        self.assertIn('query', schema['parameters'])
        self.assertTrue(schema['parameters']['query']['required'])
        self.assertFalse(schema['parameters']['limit']['required'])

    def test_create_error_result(self):
        """Test error result creation (P1)."""
        from the_edge_agent.actions.tools_actions import create_error_result

        result = create_error_result(
            error="Test error message",
            error_type="execution",
            tool_name="TestTool"
        )

        self.assertFalse(result['success'])
        self.assertEqual(result['error'], "Test error message")
        self.assertEqual(result['error_type'], "execution")
        self.assertEqual(result['tool'], "TestTool")


class TestToolsBridgeActionsIntegration(unittest.TestCase):
    """Integration tests for tools bridge actions."""

    def setUp(self):
        """Set up test fixtures."""
        from the_edge_agent import YAMLEngine
        self.engine = YAMLEngine()

    def test_tools_bridge_in_yaml_workflow(self):
        """Test tools bridge actions in YAML workflow (P0)."""
        from the_edge_agent.actions import tools_actions

        # Mock CrewAI as available and mock tool execution
        original_flag = tools_actions.HAS_CREWAI
        tools_actions.HAS_CREWAI = True

        mock_tool = MagicMock()
        mock_tool._run = MagicMock(return_value="Search result: AI news")

        try:
            with patch('the_edge_agent.actions.tools_actions._get_crewai_tool') as mock_get:
                mock_get.return_value = mock_tool

                config = {
                    'state_schema': {'query': str, 'result': dict},
                    'nodes': [
                        {
                            'name': 'search',
                            'uses': 'tools.crewai',
                            'with': {
                                'tool': 'SerperDevTool',
                                'query': '{{ state.query }}'
                            },
                            'output': 'result'
                        }
                    ],
                    'edges': [
                        {'from': '__start__', 'to': 'search'},
                        {'from': 'search', 'to': '__end__'}
                    ]
                }

                graph = self.engine.load_from_dict(config)
                events = list(graph.invoke({'query': 'AI news'}))

                # Should complete successfully
                final_event = events[-1]
                self.assertEqual(final_event['type'], 'final')

                # Check result
                result = final_event['state'].get('result', {})
                self.assertTrue(result.get('success', False))
        finally:
            tools_actions.HAS_CREWAI = original_flag

    def test_tools_error_handling_in_workflow(self):
        """Test error handling in YAML workflow (P0)."""
        from the_edge_agent.actions import tools_actions

        # Test with unavailable tool
        original_flag = tools_actions.HAS_CREWAI
        tools_actions.HAS_CREWAI = False

        try:
            config = {
                'state_schema': {'query': str, 'result': dict},
                'nodes': [
                    {
                        'name': 'search',
                        'uses': 'tools.crewai',
                        'with': {
                            'tool': 'SerperDevTool',
                            'query': '{{ state.query }}'
                        },
                        'output': 'result'
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'search'},
                    {'from': 'search', 'to': '__end__'}
                ]
            }

            graph = self.engine.load_from_dict(config)
            events = list(graph.invoke({'query': 'AI news'}))

            # Should complete (not crash)
            final_event = events[-1]
            self.assertEqual(final_event['type'], 'final')

            # Result should indicate failure gracefully
            result = final_event['state'].get('result', {})
            self.assertFalse(result.get('success', True))
            self.assertIn('error', result)
        finally:
            tools_actions.HAS_CREWAI = original_flag

    def test_tools_discover_in_workflow(self):
        """Test tools.discover in YAML workflow (P1)."""
        from the_edge_agent.actions import tools_actions

        with patch.object(tools_actions, '_discover_crewai_tools') as mock_crewai:
            mock_crewai.return_value = [
                {"name": "Tool1", "description": "Desc1", "parameters": {}, "source": "crewai"}
            ]

            config = {
                'state_schema': {'available_tools': dict},
                'nodes': [
                    {
                        'name': 'discover',
                        'uses': 'tools.discover',
                        'with': {
                            'source': 'crewai'
                        },
                        'output': 'available_tools'
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'discover'},
                    {'from': 'discover', 'to': '__end__'}
                ]
            }

            graph = self.engine.load_from_dict(config)
            events = list(graph.invoke({}))

            final_event = events[-1]
            self.assertEqual(final_event['type'], 'final')

            tools_result = final_event['state'].get('available_tools', {})
            self.assertTrue(tools_result.get('success', False))
            self.assertEqual(tools_result.get('count', 0), 1)


class TestToolsBridgeHelperFunctions(unittest.TestCase):
    """Tests for helper functions in tools_actions module."""

    def test_run_async_in_sync(self):
        """Test async to sync bridge."""
        from the_edge_agent.actions.tools_actions import run_async_in_sync
        import asyncio

        async def async_func():
            await asyncio.sleep(0.01)
            return "async result"

        result = run_async_in_sync(async_func())
        self.assertEqual(result, "async result")

    def test_discovery_cache_expiration(self):
        """Test that cache entries expire."""
        from the_edge_agent.actions.tools_actions import DiscoveryCache
        import time

        # Create cache with very short TTL
        cache = DiscoveryCache(ttl_seconds=0.1)

        cache.set("test", [{"name": "Tool"}])
        self.assertIsNotNone(cache.get("test"))

        # Wait for expiration
        time.sleep(0.15)

        # Should be expired
        self.assertIsNone(cache.get("test"))


if __name__ == '__main__':
    unittest.main()
