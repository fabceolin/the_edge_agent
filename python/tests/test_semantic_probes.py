"""
Integration tests for semantic probes (TEA-YAML-004 AC: 11-15, 22).

Test coverage:
- Probe syntax parsing (AC: 11)
- for_each iteration (AC: 12)
- LLM yes/no question execution (AC: 13)
- fail-fast on rejection (AC: 14)
- Probe failure error details (AC: 15, 22)
"""

import pytest
from unittest.mock import MagicMock, patch
from the_edge_agent.extraction_validation import (
    SemanticProbe,
    SemanticProbeExecutor,
    ValidationResult,
)


class TestSemanticProbeParsing:
    """Tests for SemanticProbe.from_dict() parsing (AC: 11)."""

    def test_parse_minimal_probe(self):
        """Parse probe with required fields only."""
        data = {
            "for_each": "relationship",
            "probe": "Is {{ subject }} related to {{ object }}?",
        }
        probe = SemanticProbe.from_dict(data)

        assert probe.for_each == "relationship"
        assert probe.probe == "Is {{ subject }} related to {{ object }}?"
        assert probe.where is None
        assert probe.on_fail == "reject"  # default

    def test_parse_probe_with_where(self):
        """Parse probe with where condition."""
        data = {
            "for_each": "relationship",
            "where": "type == 'mother'",
            "probe": "Is {{ subject }} the mother of {{ object }}?",
        }
        probe = SemanticProbe.from_dict(data)

        assert probe.where == "type == 'mother'"

    def test_parse_probe_with_on_fail_warn(self):
        """Parse probe with on_fail=warn."""
        data = {
            "for_each": "entity",
            "probe": "Is {{ name }} mentioned in the text?",
            "on_fail": "warn",
        }
        probe = SemanticProbe.from_dict(data)

        assert probe.on_fail == "warn"


class TestSemanticProbeIteration:
    """Tests for probe iteration (AC: 12)."""

    def test_iterate_over_entities(self):
        """Probe iterates over all entities."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} valid?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "yes"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        entities = [
            {"name": "Alice"},
            {"name": "Bob"},
            {"name": "Charlie"},
        ]

        result = executor.validate(entities=entities, relationships=[])

        # LLM should be called 3 times (once per entity)
        assert mock_llm.call_count == 3
        assert result.valid is True

    def test_iterate_over_relationships(self):
        """Probe iterates over all relationships."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                probe="Is {{ subject }} related to {{ object }}?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "yes"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        relationships = [
            {"type": "mother", "subject": "Alice", "object": "Bob"},
            {"type": "father", "subject": "Charlie", "object": "Bob"},
        ]

        result = executor.validate(entities=[], relationships=relationships)

        assert mock_llm.call_count == 2
        assert result.valid is True

    def test_where_filters_items(self):
        """Where condition filters items for iteration."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                where="type == 'mother'",
                probe="Is {{ subject }} the mother of {{ object }}?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "yes"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        relationships = [
            {"type": "mother", "subject": "Alice", "object": "Bob"},
            {"type": "father", "subject": "Charlie", "object": "Bob"},
            {"type": "mother", "subject": "Diana", "object": "Eve"},
        ]

        result = executor.validate(entities=[], relationships=relationships)

        # Only 2 mother relationships should be probed
        assert mock_llm.call_count == 2
        assert result.valid is True


class TestLLMProbeExecution:
    """Tests for LLM probe execution (AC: 13)."""

    def test_llm_called_with_yes_no_question(self):
        """LLM is called with a yes/no question prompt."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} a person?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "yes"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[{"name": "Alice"}],
            relationships=[],
        )

        # Check LLM was called with expected format
        mock_llm.assert_called_once()
        call_args = mock_llm.call_args
        messages = call_args.kwargs.get(
            "messages", call_args.args[1] if len(call_args.args) > 1 else []
        )

        # Verify system message asks for yes/no
        assert any("yes" in str(m).lower() and "no" in str(m).lower() for m in messages)
        # Verify user message contains the probe question
        assert any("Alice" in str(m) for m in messages)

    def test_affirmative_response_passes(self):
        """Affirmative LLM response passes validation."""
        probes = [SemanticProbe(for_each="entity", probe="Is {{ name }} valid?")]

        for response in ["yes", "Yes", "YES", "true", "correct", "indeed"]:
            mock_llm = MagicMock(return_value={"response": response})
            executor = SemanticProbeExecutor(probes, llm_call=mock_llm)
            result = executor.validate(entities=[{"name": "Alice"}], relationships=[])
            assert result.valid is True, f"Failed for response: {response}"

    def test_negative_response_fails(self):
        """Negative LLM response fails validation."""
        probes = [SemanticProbe(for_each="entity", probe="Is {{ name }} valid?")]

        for response in ["no", "No", "NO", "false", "incorrect"]:
            mock_llm = MagicMock(return_value={"response": response})
            executor = SemanticProbeExecutor(probes, llm_call=mock_llm)
            result = executor.validate(entities=[{"name": "Alice"}], relationships=[])
            assert result.valid is False, f"Failed for response: {response}"


class TestFailFastBehavior:
    """Tests for fail-fast behavior (AC: 14)."""

    def test_fail_fast_on_rejection(self):
        """Validation fails immediately on first 'no' with on_fail=reject."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} valid?",
                on_fail="reject",
            )
        ]

        # First call returns "no", subsequent calls shouldn't happen
        mock_llm = MagicMock(return_value={"response": "no"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"},
            ],
            relationships=[],
        )

        assert result.valid is False
        # Should stop after first failure
        assert mock_llm.call_count == 1

    def test_continue_on_warn(self):
        """Validation continues on 'no' with on_fail=warn."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} valid?",
                on_fail="warn",
            )
        ]

        mock_llm = MagicMock(return_value={"response": "no"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"},
            ],
            relationships=[],
        )

        # All entities should be checked (no fail-fast)
        assert mock_llm.call_count == 3
        # But result is still invalid (warnings collected)
        assert result.valid is False
        assert len(result.errors) == 3


class TestProbeErrorDetails:
    """Tests for probe failure error details (AC: 15, 22)."""

    def test_error_includes_question(self):
        """Error includes the probe question asked."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} a real person?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "no"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[{"name": "Alice"}],
            relationships=[],
        )

        assert result.valid is False
        error = result.errors[0]
        assert error["type"] == "semantic_probe_failed"
        assert "Is Alice a real person?" in error["question"]

    def test_error_includes_response(self):
        """Error includes the LLM response."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} valid?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "no, definitely not"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[{"name": "Alice"}],
            relationships=[],
        )

        error = result.errors[0]
        assert "no, definitely not" in error["response"]

    def test_error_includes_item_context(self):
        """Error includes the item that failed validation."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                probe="Is {{ subject }} the {{ type }} of {{ object }}?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "no"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[],
            relationships=[{"type": "mother", "subject": "Alice", "object": "Bob"}],
        )

        error = result.errors[0]
        assert error["item"]["type"] == "mother"
        assert error["item"]["subject"] == "Alice"
        assert error["item"]["object"] == "Bob"


class TestProbeTemplateRendering:
    """Tests for probe template variable interpolation."""

    def test_template_variable_interpolation(self):
        """Probe templates correctly interpolate variables."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                probe="Does {{ state.text }} say {{ subject }} is the {{ type }} of {{ object }}?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "yes"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[],
            relationships=[{"type": "mother", "subject": "Alice", "object": "Bob"}],
            source_text="Alice is the mother of Bob.",
            state={"text": "the document"},
        )

        # Check that the question was rendered with variables
        call_args = mock_llm.call_args
        messages = call_args.kwargs.get("messages", [])
        question_content = str(messages)
        assert "the document" in question_content or "Alice" in question_content


class TestNoLLMConfiguration:
    """Tests for missing LLM configuration."""

    def test_no_llm_returns_error(self):
        """Validation with probes but no LLM returns error."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} valid?",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=None)

        result = executor.validate(
            entities=[{"name": "Alice"}],
            relationships=[],
        )

        assert result.valid is False
        assert result.errors[0]["type"] == "configuration_error"


class TestInvalidProbeConfig:
    """Tests for invalid probe configuration."""

    def test_invalid_for_each_value(self):
        """Invalid for_each value is reported."""
        probes = [
            SemanticProbe(
                for_each="invalid_type",
                probe="Is this valid?",
            )
        ]
        mock_llm = MagicMock(return_value={"response": "yes"})
        executor = SemanticProbeExecutor(probes, llm_call=mock_llm)

        result = executor.validate(
            entities=[{"name": "Alice"}],
            relationships=[],
        )

        assert result.valid is False
        assert result.errors[0]["type"] == "invalid_probe_config"
