"""
Tests for YAML Engine - Security and E2E Tests

Security and end-to-end tests including:
- Security boundaries
- E2E example validations
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END, MemoryCheckpointer
from the_edge_agent.yaml_engine import DotDict


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


# =============================================================================
# Security Tests
# =============================================================================

class TestSecurityBoundaries:
    """Security tests for code execution boundaries.

    Note: Current implementation uses exec/eval which are inherently dangerous.
    These tests document the attack surface.
    """

    def test_sec_001_inline_cannot_os_system(self, engine):
        """YAML-001-SEC-001: Document that inline code CAN access os module.

        This test documents current behavior - inline code is NOT sandboxed.
        """
        config = {
            'config': {'raise_exceptions': True},
            'nodes': [
                {
                    'name': 'test',
                    'run': '''
import os
# This WOULD work - documenting that inline code is not sandboxed
return {"os_available": True}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        # This demonstrates os IS available - security concern documented
        assert events[-1]['state']['os_available'] is True

    def test_sec_003_template_blocks_import(self, engine):
        """YAML-001-SEC-003: Jinja2 templates block __import__ (security improvement).

        TEA-YAML-001: Migrating to Jinja2 improves security - templates can no longer
        use __import__ or other dangerous builtins. This is a significant security
        improvement over the previous eval()-based approach.

        Note: `run:` blocks still use exec() and have full Python access by design.
        """
        # With Jinja2, __import__ is not available in the template context
        with pytest.raises(ValueError) as exc_info:
            engine._process_template('{{ __import__("os") }}', {})
        # Error indicates __import__ is undefined (blocked by Jinja2)
        assert '__import__' in str(exc_info.value)


# =============================================================================
# E2E Tests
# =============================================================================

class TestE2EExamples:
    """End-to-end tests for example YAML files."""

    @pytest.fixture
    def examples_dir(self):
        """Get examples directory path."""
        return Path(__file__).parent.parent / 'examples'

    def test_e2e_001_research_agent(self, examples_dir):
        """YAML-001-E2E-001: Load and execute yaml_agent_example.yaml."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_agent_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))
        events = list(graph.stream({'query': 'test research'}))

        final_state = events[-1]['state']
        assert 'search_results' in final_state
        assert 'formatted_output' in final_state
        assert len(final_state['search_results']) == 5

    def test_e2e_002_customer_support(self, examples_dir):
        """YAML-001-E2E-002: Load and execute yaml_customer_support_example.yaml."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_customer_support_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        # YAML file has interrupt_after, so we need checkpointer
        checkpointer = MemoryCheckpointer()
        graph = engine.load_from_file(str(yaml_path), checkpointer=checkpointer)

        # Test billing path - executes until interrupt_after classify_intent
        events = list(graph.stream({
            'customer_id': 'CUST-001',
            'customer_message': 'I was charged twice for my bill!'
        }))

        # Find interrupt event and resume
        interrupt_event = next((e for e in events if e.get('type', '').startswith('interrupt')), None)
        if interrupt_event:
            # Resume from checkpoint
            checkpoint_path = interrupt_event.get('checkpoint_path')
            resume_events = list(graph.stream(None, checkpoint=checkpoint_path))
            events.extend(resume_events)

        final_state = events[-1]['state']
        assert final_state['intent'] == 'billing'
        assert 'BILL-' in final_state.get('ticket_id', '')

    def test_e2e_003_perplexity_research(self, examples_dir):
        """YAML-001-E2E-003: Load and execute yaml_perplexity_example.yaml (mocked)."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_perplexity_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))
        events = list(graph.stream({'user_query': 'What is AI?'}))

        final_state = events[-1]['state']
        assert 'formatted_report' in final_state
        assert 'Research Report' in final_state['formatted_report']

    def test_e2e_005_stream_yields_intermediate(self, examples_dir):
        """YAML-001-E2E-005: Stream execution yields intermediate states."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_agent_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))
        events = list(graph.stream({'query': 'test'}))

        # Should have multiple events, not just final
        assert len(events) > 1

        # Check for intermediate state events
        state_events = [e for e in events if e.get('type') == 'state']
        assert len(state_events) > 0


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
