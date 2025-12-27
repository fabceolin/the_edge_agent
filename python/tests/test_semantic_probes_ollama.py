"""
Integration tests for semantic probes using real Ollama server.

Tests the full stack validation with actual LLM calls to verify grounding.
"""

import os
import pytest
import requests
from typing import Dict, Any, List

from the_edge_agent.extraction_validation import (
    SemanticProbe,
    SemanticProbeExecutor,
    ValidationResult,
)


# Ollama server configuration
OLLAMA_HOST = os.environ.get("OLLAMA_HOST", "100.82.186.88")
OLLAMA_PORT = os.environ.get("OLLAMA_PORT", "11434")
OLLAMA_MODEL = os.environ.get("OLLAMA_MODEL", "phi4-mini")
OLLAMA_URL = f"http://{OLLAMA_HOST}:{OLLAMA_PORT}/api/chat"


def ollama_call(
    state: Dict, messages: List[Dict], max_tokens: int = 10
) -> Dict[str, Any]:
    """Make a real call to Ollama server."""
    payload = {
        "model": OLLAMA_MODEL,
        "messages": messages,
        "stream": False,
        "options": {
            "num_predict": max_tokens,
            "temperature": 0,  # Deterministic for tests
        },
    }

    response = requests.post(OLLAMA_URL, json=payload, timeout=30)
    response.raise_for_status()
    data = response.json()

    # Extract content from Ollama response format
    content = data.get("message", {}).get("content", "")
    return {"response": content}


def is_ollama_available() -> bool:
    """Check if Ollama server is reachable."""
    try:
        response = requests.get(
            f"http://{OLLAMA_HOST}:{OLLAMA_PORT}/api/tags", timeout=5
        )
        return response.status_code == 200
    except Exception:
        return False


# Skip all tests if Ollama is not available
pytestmark = pytest.mark.skipif(
    not is_ollama_available(),
    reason=f"Ollama server not available at {OLLAMA_HOST}:{OLLAMA_PORT}",
)


class TestSemanticProbesWithOllama:
    """Integration tests using real Ollama LLM."""

    def test_affirmative_response_for_true_fact(self):
        """LLM confirms a factually true statement."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} a typically female name?",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=ollama_call)

        result = executor.validate(
            entities=[{"name": "Alice", "type": "person"}],
            relationships=[],
        )

        # Alice is traditionally a female name, should pass
        assert result.valid is True, f"Expected True, got errors: {result.errors}"

    def test_negative_response_for_false_fact(self):
        """LLM rejects a factually false statement."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                probe="Is 2+2 equal to 5?",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=ollama_call)

        result = executor.validate(
            entities=[],
            relationships=[{"type": "test", "subject": "2+2", "object": "5"}],
        )

        # 2+2 != 5, should fail
        assert result.valid is False

    def test_grounding_verification(self):
        """Verify relationship mentioned in text."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                where="type == 'mother'",
                probe="Based on the text '{{ state.source_text }}', is {{ subject }} the mother of {{ object }}?",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=ollama_call)

        result = executor.validate(
            entities=[],
            relationships=[{"type": "mother", "subject": "Mary", "object": "John"}],
            state={"source_text": "Mary is the mother of John."},
        )

        assert result.valid is True, f"Expected True, got errors: {result.errors}"

    def test_grounding_rejection(self):
        """Reject relationship NOT mentioned in text."""
        probes = [
            SemanticProbe(
                for_each="relationship",
                probe="According to the text '{{ state.source_text }}', is {{ subject }} the {{ type }} of {{ object }}?",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=ollama_call)

        result = executor.validate(
            entities=[],
            relationships=[{"type": "mother", "subject": "Alice", "object": "Bob"}],
            state={
                "source_text": "The sky is blue and water is wet."
            },  # No family info
        )

        # Relationship not grounded in text
        assert result.valid is False

    def test_fail_fast_on_reject(self):
        """Validation stops at first failure with on_fail=reject."""
        call_count = [0]

        def counting_llm_call(state, messages, max_tokens=10):
            call_count[0] += 1
            return ollama_call(state, messages, max_tokens)

        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is XYZ123 a valid planet name?",  # Always false
                on_fail="reject",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=counting_llm_call)

        result = executor.validate(
            entities=[
                {"name": "XYZ123", "type": "planet"},
                {"name": "ABC456", "type": "planet"},
                {"name": "DEF789", "type": "planet"},
            ],
            relationships=[],
        )

        assert result.valid is False
        # Should stop after first failure
        assert call_count[0] == 1

    def test_continue_on_warn(self):
        """Validation continues with on_fail=warn."""
        call_count = [0]

        def counting_llm_call(state, messages, max_tokens=10):
            call_count[0] += 1
            return ollama_call(state, messages, max_tokens)

        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is XYZ123 a valid planet name?",  # Always false
                on_fail="warn",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=counting_llm_call)

        result = executor.validate(
            entities=[
                {"name": "XYZ123", "type": "planet"},
                {"name": "ABC456", "type": "planet"},
            ],
            relationships=[],
        )

        assert result.valid is False
        # Should check both entities
        assert call_count[0] == 2

    def test_mixed_results(self):
        """Multiple probes with mixed pass/fail results."""
        probes = [
            SemanticProbe(
                for_each="entity",
                where="type == 'math'",
                probe="Is {{ name }} a correct mathematical statement?",
                on_fail="warn",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=ollama_call)

        result = executor.validate(
            entities=[
                {"name": "2+2=4", "type": "math"},  # True
                {"name": "1+1=3", "type": "math"},  # False
                {"name": "3x3=9", "type": "math"},  # True
            ],
            relationships=[],
        )

        # Has at least one failure
        assert result.valid is False
        # But not all failed
        assert len(result.errors) < 3

    def test_error_includes_question_and_response(self):
        """Error details include question asked and LLM response."""
        probes = [
            SemanticProbe(
                for_each="entity",
                probe="Is {{ name }} a planet in our solar system?",
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=ollama_call)

        result = executor.validate(
            entities=[{"name": "Zorbon", "type": "fake_planet"}],
            relationships=[],
        )

        assert result.valid is False
        error = result.errors[0]
        assert error["type"] == "semantic_probe_failed"
        assert "Zorbon" in error["question"]
        assert "response" in error
        assert error["item"]["name"] == "Zorbon"


class TestWhereFiltering:
    """Tests for where condition filtering with real LLM."""

    def test_where_filters_items(self):
        """Only items matching where condition are probed."""
        call_count = [0]

        def counting_llm_call(state, messages, max_tokens=10):
            call_count[0] += 1
            return ollama_call(state, messages, max_tokens)

        probes = [
            SemanticProbe(
                for_each="relationship",
                where="type == 'mother'",
                probe="Is this a valid relationship?",
                on_fail="warn",  # Use warn to avoid fail-fast
            )
        ]
        executor = SemanticProbeExecutor(probes, llm_call=counting_llm_call)

        result = executor.validate(
            entities=[],
            relationships=[
                {"type": "mother", "subject": "Alice", "object": "Bob"},
                {"type": "father", "subject": "Charlie", "object": "Bob"},
                {"type": "mother", "subject": "Diana", "object": "Eve"},
            ],
        )

        # Only 2 mother relationships should be probed (where filters out father)
        assert call_count[0] == 2


if __name__ == "__main__":
    # Allow running directly for quick testing
    print(f"Testing against Ollama at {OLLAMA_URL}")
    if is_ollama_available():
        pytest.main([__file__, "-v"])
    else:
        print(f"ERROR: Ollama server not available at {OLLAMA_HOST}:{OLLAMA_PORT}")
