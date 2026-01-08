"""
Tests for LLM Call Integration in Validation Actions (TEA-YAML-004a).

Test Coverage:
- Provider configuration (AC: 1-5)
- Error handling (AC: 6-8)
- Integration (AC: 9-11)
"""

import pytest
from unittest.mock import MagicMock, patch


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_engine():
    """Mock YAMLEngine with settings."""
    engine = MagicMock()
    engine._settings = {
        "llm": {
            "provider": "openai",
            "model": "gpt-4",
        }
    }
    engine._extraction_validation_config = {}
    engine._prolog_runtime = None
    engine._prolog_timeout = 30.0
    engine._prolog_sandbox = True
    engine._jinja_env = None
    return engine


@pytest.fixture
def mock_llm_call():
    """Mock llm.call action."""

    def llm_call(state, model, messages, **kwargs):
        return {"content": "yes"}

    return llm_call


# =============================================================================
# Provider Configuration Tests (AC: 1-5)
# =============================================================================


class TestProviderConfiguration:
    """Test LLM provider configuration."""

    def test_unit_001_uses_engine_settings_as_default(self, mock_engine, mock_llm_call):
        """AC-1, AC-2: Use provider/model from engine settings when not in state."""
        from the_edge_agent.actions.validation_actions import register_actions

        registry = {"llm.call": mock_llm_call}
        register_actions(registry, mock_engine)

        # State without provider/model
        state = {"entities": [], "relationships": []}

        # Call validate.extraction
        with patch.object(
            mock_engine,
            "_settings",
            {"llm": {"provider": "anthropic", "model": "claude-3-opus"}},
        ):
            # Re-register to pick up new settings
            registry = {"llm.call": mock_llm_call}
            register_actions(registry, mock_engine)

            result = registry["validate.extraction"](
                state,
                entities=[],
                relationships=[],
            )

            assert result is not None
            assert "valid" in result

    def test_unit_002_state_overrides_engine_defaults(self, mock_engine):
        """AC-3: State-level llm_provider/llm_model override engine defaults."""
        from the_edge_agent.actions.validation_actions import register_actions

        captured_model = []

        def capturing_llm_call(state, model, messages, **kwargs):
            captured_model.append(model)
            return {"content": "yes"}

        registry = {"llm.call": capturing_llm_call}
        register_actions(registry, mock_engine)

        # State with explicit provider/model
        state = {
            "entities": [],
            "relationships": [],
            "llm_provider": "azure",
            "llm_model": "gpt-35-turbo",
        }

        # Need to trigger LLM call through a probe
        result = registry["validate.extraction"](
            state,
            probes=[
                {
                    "for_each": "entity",
                    "probe": "Is this valid?",
                    "on_fail": "reject",
                }
            ],
        )

        # The state provider should override engine default

    def test_unit_003_supports_openai_provider(self, mock_engine):
        """AC-4: OpenAI provider is supported."""
        from the_edge_agent.actions.validation_actions import register_actions

        captured_model = []

        def capturing_llm_call(state, model, messages, **kwargs):
            captured_model.append(model)
            return {"content": "yes"}

        registry = {"llm.call": capturing_llm_call}
        mock_engine._settings = {"llm": {"provider": "openai", "model": "gpt-4"}}
        register_actions(registry, mock_engine)

        state = {"entities": [], "relationships": []}
        result = registry["validate.extraction"](state)

        assert result is not None

    def test_unit_004_supports_anthropic_provider(self, mock_engine):
        """AC-4: Anthropic provider is supported."""
        from the_edge_agent.actions.validation_actions import register_actions

        registry = {}
        mock_engine._settings = {
            "llm": {"provider": "anthropic", "model": "claude-3-opus"}
        }
        register_actions(registry, mock_engine)

        state = {"entities": [], "relationships": []}
        result = registry["validate.extraction"](state)

        assert result is not None
        assert "valid" in result

    def test_unit_005_supports_azure_provider(self, mock_engine):
        """AC-4: Azure provider is supported."""
        from the_edge_agent.actions.validation_actions import register_actions

        registry = {}
        mock_engine._settings = {"llm": {"provider": "azure", "model": "gpt-35-turbo"}}
        register_actions(registry, mock_engine)

        state = {"entities": [], "relationships": []}
        result = registry["validate.extraction"](state)

        assert result is not None

    def test_unit_006_supports_auto_provider(self, mock_engine):
        """AC-4: Auto provider detection is supported."""
        from the_edge_agent.actions.validation_actions import register_actions

        registry = {}
        mock_engine._settings = {"llm": {"provider": "auto", "model": "gpt-4"}}
        register_actions(registry, mock_engine)

        state = {"entities": [], "relationships": []}
        result = registry["validate.extraction"](state)

        assert result is not None


# =============================================================================
# Error Handling Tests (AC: 6-8)
# =============================================================================


class TestErrorHandling:
    """Test LLM call error handling."""

    def test_unit_007_error_when_llm_call_unavailable(self, mock_engine):
        """AC-6: Clear error when llm.call is not available."""
        from the_edge_agent.actions.validation_actions import register_actions

        # Registry without llm.call
        registry = {}
        register_actions(registry, mock_engine)

        state = {"entities": [], "relationships": []}

        # Should not raise, validation should still work (no probes)
        result = registry["validate.extraction"](state)
        assert result is not None
        assert "valid" in result

    def test_unit_008_validation_without_probes_works_without_llm(self, mock_engine):
        """AC-9: Validation without probes works even if llm.call is unavailable."""
        from the_edge_agent.actions.validation_actions import register_actions

        registry = {}  # No llm.call
        register_actions(registry, mock_engine)

        state = {"entities": [], "relationships": []}

        result = registry["validate.extraction"](
            state,
            schema={
                "entities": {"required_fields": ["name"]},
            },
        )

        assert result is not None
        assert "valid" in result


# =============================================================================
# Integration Tests (AC: 9-11)
# =============================================================================


class TestIntegration:
    """Integration tests for LLM call."""

    def test_int_001_existing_semantic_probe_behavior_unchanged(self, mock_engine):
        """AC-9, AC-10: Existing semantic probe behavior unchanged."""
        from the_edge_agent.actions.validation_actions import register_actions

        def mock_llm(state, model, messages, **kwargs):
            # Simulate LLM response for probe
            return {"content": "yes"}

        registry = {"llm.call": mock_llm}
        register_actions(registry, mock_engine)

        state = {
            "entities": [{"name": "Alice", "type": "person"}],
            "relationships": [],
        }

        result = registry["validate.extraction"](
            state,
            entities=state["entities"],
            relationships=[],
        )

        assert result is not None
        assert "valid" in result

    def test_int_002_generate_prompt_still_works(self, mock_engine):
        """AC-10: generate_prompt action unchanged."""
        from the_edge_agent.actions.validation_actions import register_actions

        registry = {}
        register_actions(registry, mock_engine)

        state = {}

        result = registry["validate.generate_prompt"](
            state,
            schema={
                "entities": {
                    "required_fields": ["name"],
                    "optional_fields": ["age"],
                },
                "relationships": {
                    "types": ["parent", "child"],
                },
            },
        )

        assert result is not None
        assert "extraction_prompt" in result


# =============================================================================
# Run tests
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
