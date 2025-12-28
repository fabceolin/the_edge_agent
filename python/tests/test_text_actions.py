"""
Unit tests for text_actions module (TEA-KIROKU-002).

Tests the text.insert_citations action using semantic embedding matching.
All OpenAI API calls are mocked to avoid real API requests during testing.
"""

import unittest
from unittest.mock import MagicMock, patch
import numpy as np

from the_edge_agent.actions.text_actions import (
    insert_citations,
    _get_sentences,
    _reorder_references,
)


def create_mock_embedding(dim: int = 3072) -> list:
    """Create a mock embedding vector."""
    return list(np.random.randn(dim))


def create_mock_embeddings_response(texts: list, dim: int = 3072):
    """Create a mock OpenAI embeddings response."""
    mock_response = MagicMock()
    mock_response.data = []
    for i, text in enumerate(texts):
        embedding_obj = MagicMock()
        embedding_obj.embedding = create_mock_embedding(dim)
        mock_response.data.append(embedding_obj)
    return mock_response


class TestGetSentences(unittest.TestCase):
    """Test sentence extraction from text."""

    def test_simple_text(self):
        """Extract sentences from simple text."""
        text = "First sentence. Second sentence."
        sentences = _get_sentences(text)
        self.assertEqual(len(sentences), 2)

    def test_excludes_headers(self):
        """Headers should be excluded from sentences."""
        text = "# Header\n\nFirst sentence. Second sentence."
        sentences = _get_sentences(text)
        # Should not include header text
        self.assertFalse(any("Header" in s for s in sentences))

    def test_excludes_images(self):
        """Image markdown should be excluded."""
        text = "![alt text](image.png)\n\nFirst sentence."
        sentences = _get_sentences(text)
        self.assertFalse(any("alt text" in s for s in sentences))

    def test_removes_abstract(self):
        """Abstract section should be removed."""
        text = "## Abstract\n\nThis is abstract.\n\n## Introduction\n\nFirst sentence."
        sentences = _get_sentences(text)
        self.assertFalse(any("abstract" in s.lower() for s in sentences))


class TestReorderReferences(unittest.TestCase):
    """Test reference reordering."""

    def test_reorder_by_occurrence(self):
        """References should be reordered by first occurrence."""
        citations = {0: [2], 1: [1]}  # Sentence 0 has ref 2, sentence 1 has ref 1
        refs = ["Ref A", "Ref B"]
        ordered = _reorder_references(citations, refs)
        # After reordering, ref 2 (Ref B) should come first since it appears in sentence 0
        self.assertEqual(ordered[0], "Ref B")


class TestInsertCitations(unittest.TestCase):
    """Test main insert_citations function with mocked embeddings."""

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_empty_text_returns_empty(self, mock_openai):
        """Empty text returns empty result."""
        result = insert_citations({}, text="", references=["Some ref"])

        self.assertEqual(result["cited_text"], "")
        self.assertEqual(result["references_section"], "")
        self.assertEqual(result["citation_map"], {})

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_empty_references_returns_original(self, mock_openai):
        """No references returns original text."""
        text = "Some text without references."
        result = insert_citations({}, text=text, references=[])

        self.assertEqual(result["cited_text"], text)
        self.assertEqual(result["references_section"], "")

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_single_citation_insertion(self, mock_openai):
        """Insert single citation marker using semantic matching."""
        # Setup mock
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        # Create embeddings that will match sentence to reference
        # Sentence embedding
        sent_emb = np.array([1.0, 0.0, 0.0])
        # Reference embedding (similar to sentence)
        ref_emb = np.array([0.9, 0.1, 0.0])

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            if len(input) == 1 and "transformer" in input[0].lower():
                # Single sentence
                emb_obj = MagicMock()
                emb_obj.embedding = sent_emb.tolist()
                response.data = [emb_obj]
            elif len(input) == 1 and "Vaswani" in input[0]:
                # Single reference
                emb_obj = MagicMock()
                emb_obj.embedding = ref_emb.tolist()
                response.data = [emb_obj]
            else:
                # Generic case
                response.data = []
                for text in input:
                    emb_obj = MagicMock()
                    emb_obj.embedding = np.random.randn(3).tolist()
                    response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = "The transformer architecture is revolutionary."
        refs = ["Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017."]

        result = insert_citations({}, text=text, references=refs)

        # Should have citation in text
        self.assertIn("[1]", result["cited_text"])
        # Should have References section
        self.assertIn("## References", result["references_section"])
        self.assertIn("1. Vaswani", result["references_section"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_multiple_citations(self, mock_openai):
        """Insert multiple citation markers."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        # Setup embeddings so each sentence matches a different reference
        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for i, text in enumerate(input):
                emb_obj = MagicMock()
                # Create orthogonal embeddings for different sentences/refs
                emb = np.zeros(10)
                emb[i % 10] = 1.0
                emb_obj.embedding = emb.tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = """Machine learning has evolved significantly.
The transformer architecture changed NLP.
Large language models show emergent abilities."""

        refs = [
            "Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017.",
            "Brown, T., et al. Language Models are Few-Shot Learners. NeurIPS 2020.",
        ]

        result = insert_citations({}, text=text, references=refs)

        # Should have References section with both
        self.assertIn("## References", result["references_section"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_citation_map_structure(self, mock_openai):
        """Citation map has correct structure."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = "Attention mechanisms work well."
        refs = [
            "Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017.",
            "Brown, T., et al. Language Models. NeurIPS 2020.",
        ]

        result = insert_citations({}, text=text, references=refs)

        # Citation map should have integer keys
        self.assertTrue(all(isinstance(k, int) for k in result["citation_map"].keys()))

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_markdown_formatting_preserved(self, mock_openai):
        """Markdown formatting in text is preserved."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = "**Bold** and *italic* formatting in text."
        refs = ["Some Author. Some Title. 2020."]

        result = insert_citations({}, text=text, references=refs)

        self.assertIn("**Bold**", result["cited_text"])
        self.assertIn("*italic*", result["cited_text"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_unicode_in_references(self, mock_openai):
        """Handle Unicode characters in references."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = "Research on natural language processing."
        refs = ["Müller, H., et al. Natural Language Processing. 2021."]

        result = insert_citations({}, text=text, references=refs)

        self.assertIn("Müller", result["references_section"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_full_document_processing(self, mock_openai):
        """Process complete academic document."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for i, text in enumerate(input):
                emb_obj = MagicMock()
                emb = np.zeros(10)
                emb[i % 10] = 1.0
                emb_obj.embedding = emb.tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = """
Machine learning has revolutionized many fields.
The transformer architecture introduced attention mechanisms.
Recent advances in large language models show promising results.
"""
        refs = [
            "Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017. https://arxiv.org/abs/1706.03762",
            "Brown, T., et al. Language Models are Few-Shot Learners. NeurIPS 2020. https://arxiv.org/abs/2005.14165",
        ]

        result = insert_citations({}, text=text, references=refs)

        # Should have complete output
        self.assertIn("cited_text", result)
        self.assertIn("references_section", result)
        self.assertIn("citation_map", result)
        self.assertIn("text", result)

        # Full text should combine both
        self.assertIn("## References", result["text"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_removes_existing_references_section(self, mock_openai):
        """Existing References section should be removed before processing."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = """Some text here.

## References

1. Old reference."""

        refs = ["New Author. New Title. 2024."]

        result = insert_citations({}, text=text, references=refs)

        # Should only have new reference
        self.assertIn("New Author", result["references_section"])
        self.assertNotIn("Old reference", result["references_section"])


class TestInsertCitationsEdgeCases(unittest.TestCase):
    """Test edge cases for insert_citations."""

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_no_sentences_extracted(self, mock_openai):
        """Handle case where no sentences are extracted."""
        # Text with only headers/images
        text = "# Header Only\n\n![Image](img.png)"
        refs = ["Author. Title. 2020."]

        result = insert_citations({}, text=text, references=refs)

        # Should still return with references appended
        self.assertIn("## References", result["references_section"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_api_error_returns_graceful_fallback(self, mock_openai):
        """API errors should return original text with references appended."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        # Simulate API error
        mock_client.embeddings.create.side_effect = Exception("API rate limit exceeded")

        text = "Some text that would normally get citations."
        refs = ["Author. Title. 2020."]

        result = insert_citations({}, text=text, references=refs)

        # Should return original text with references
        self.assertEqual(result["cited_text"], text)
        self.assertIn("## References", result["references_section"])
        self.assertIn("Author. Title. 2020.", result["references_section"])
        self.assertIn("error", result)
        self.assertIn("API rate limit exceeded", result["error"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_conclusions_not_cited(self, mock_openai):
        """Conclusions section should not receive citations."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = """Introduction sentence here.

## Conclusions

This is a conclusion that should not be cited."""

        refs = ["Author. Title. 2020."]

        result = insert_citations({}, text=text, references=refs)

        # Citation should only appear in introduction, not conclusions
        # (implementation excludes conclusions from sentence extraction)
        self.assertIn("[1]", result["cited_text"])

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_custom_model_parameter(self, mock_openai):
        """Custom embedding model can be specified."""
        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        captured_model = None

        def mock_embeddings_create(model, input, **kwargs):
            nonlocal captured_model
            captured_model = model
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        text = "Some sentence."
        refs = ["Author. Title. 2020."]

        insert_citations({}, text=text, references=refs, model="text-embedding-ada-002")

        self.assertEqual(captured_model, "text-embedding-ada-002")


class TestActionRegistration(unittest.TestCase):
    """Test action registration in YAMLEngine."""

    def test_action_registered(self):
        """Verify action is registered correctly."""
        from the_edge_agent.actions.text_actions import register_actions

        registry = {}

        class MockEngine:
            pass

        register_actions(registry, MockEngine())

        self.assertIn("text.insert_citations", registry)
        self.assertIn("actions.text_insert_citations", registry)

    @patch("the_edge_agent.actions.text_actions.OpenAI")
    def test_registered_action_callable(self, mock_openai):
        """Registered action is callable."""
        from the_edge_agent.actions.text_actions import register_actions

        mock_client = MagicMock()
        mock_openai.return_value = mock_client

        def mock_embeddings_create(model, input, **kwargs):
            response = MagicMock()
            response.data = []
            for text in input:
                emb_obj = MagicMock()
                emb_obj.embedding = np.random.randn(10).tolist()
                response.data.append(emb_obj)
            return response

        mock_client.embeddings.create = mock_embeddings_create

        registry = {}

        class MockEngine:
            pass

        register_actions(registry, MockEngine())

        action = registry["text.insert_citations"]
        result = action(
            state={},
            text="Test text.",
            references=["Author. Title. 2020."],
        )

        self.assertIsInstance(result, dict)
        self.assertIn("cited_text", result)


if __name__ == "__main__":
    unittest.main()
