"""
Text Actions for YAML Agents (TEA-KIROKU-002).

Provides text manipulation actions for academic and document processing:
- text.insert_citations: Insert citation markers using semantic embedding matching

This implementation uses the same algorithm as the original Kiroku project:
semantic similarity between sentences and references using OpenAI embeddings.

Usage in YAML:
    nodes:
      - name: add_citations
        uses: text.insert_citations
        with:
          text: "{{ state.draft }}"
          references: "{{ state.references }}"
          model: text-embedding-3-large

State Variables Set:
    cited_text: str   - Text with citation markers inserted
    references_section: str - Formatted references section
    citation_map: dict - Mapping of reference indices to original references
"""

import re
from typing import Any, Callable, Dict, List, Optional

# Lazy imports for optional dependencies
_np = None
_sent_tokenize = None
_OpenAI = None


def _ensure_dependencies():
    """Lazy-load optional dependencies for text actions."""
    global _np, _sent_tokenize, _OpenAI

    if _np is None:
        try:
            import numpy as np

            _np = np
        except ImportError:
            raise ImportError(
                "numpy is required for text.insert_citations. "
                "Install with: pip install 'the_edge_agent[rag]'"
            )

    if _sent_tokenize is None:
        try:
            from nltk import sent_tokenize

            _sent_tokenize = sent_tokenize
        except ImportError:
            raise ImportError(
                "nltk is required for text.insert_citations. "
                "Install with: pip install nltk"
            )

    if _OpenAI is None:
        try:
            from openai import OpenAI

            _OpenAI = OpenAI
        except ImportError:
            raise ImportError(
                "openai is required for text.insert_citations. "
                "Install with: pip install 'the_edge_agent[llm]'"
            )


def _get_sentences(text: str) -> List[str]:
    """
    Extract sentences from text, excluding headers and images.

    Based on original Kiroku implementation.

    Args:
        text: Markdown text to process

    Returns:
        List of sentences
    """
    # Remove abstract section if present
    search = re.search(r"## Abstract[^#]*", text)
    if search:
        l, r = search.span()
        text = text[:l] + text[r - 1 :]

    # Filter out headers and images, join paragraphs
    paragraphs = "\n".join(
        [
            p
            for p in text.split("\n")
            if p and not (p.startswith("#") or p.startswith("!["))
        ]
    )

    # Tokenize into sentences (use lazy-loaded nltk)
    sentences = [s.split("\n")[-1] for s in _sent_tokenize(paragraphs)]

    return sentences


def _reorder_references(
    citation_index: Dict[int, List[int]], references: List[str]
) -> List[str]:
    """
    Reorder references based on their first occurrence in text.

    Args:
        citation_index: Dict mapping sentence index to list of reference indices
        references: Original list of references

    Returns:
        Reordered list of references
    """
    new_references = []
    for key in sorted(citation_index.keys()):
        for i in range(len(citation_index[key])):
            j = len(new_references) + 1
            citation_index[key][i], j = j, citation_index[key][i]
            new_references.append(references[j - 1])
    return new_references


def insert_citations(
    state: Dict[str, Any],
    text: str,
    references: List[str],
    model: str = "text-embedding-3-large",
    api_key: Optional[str] = None,
    base_url: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Insert citation markers into text using semantic embedding matching.

    This action uses OpenAI embeddings to compute semantic similarity between
    sentences in the text and the provided references. Citations are inserted
    at the sentence with highest similarity to each reference.

    Algorithm (from original Kiroku):
    1. Tokenize text into sentences using NLTK
    2. Compute embeddings for sentences and references
    3. Calculate similarity matrix via dot product
    4. Insert citations at argmax positions
    5. Reorder references by first occurrence

    Args:
        state: Current workflow state
        text: Markdown text to process
        references: List of reference strings
        model: Embedding model (default: text-embedding-3-large)
        api_key: Optional OpenAI API key (uses OPENAI_API_KEY env var if not provided)
        base_url: Optional API base URL for compatible endpoints

    Returns:
        Dict with:
            - cited_text: str - Text with citation markers inserted
            - references_section: str - Formatted References section
            - citation_map: dict - Mapping {ref_index: original_reference}
            - text: str - Complete text with citations and references

    Example:
        >>> text = "The transformer architecture revolutionized NLP."
        >>> refs = ["Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017."]
        >>> result = insert_citations({}, text=text, references=refs)
        >>> # Citations inserted based on semantic similarity
    """
    if not text:
        return {
            "cited_text": "",
            "references_section": "",
            "citation_map": {},
            "text": "",
        }

    if not references:
        return {
            "cited_text": text,
            "references_section": "",
            "citation_map": {},
            "text": text,
        }

    # Ensure dependencies are loaded
    _ensure_dependencies()

    # Initialize OpenAI client
    client_kwargs = {}
    if api_key:
        client_kwargs["api_key"] = api_key
    if base_url:
        client_kwargs["base_url"] = base_url
    client = _OpenAI(**client_kwargs)

    # Remove existing References section if present
    paper = text.strip()
    search = re.search(r"## References", paper, re.IGNORECASE)
    if search:
        l, _ = search.span()
        paper = paper[:l].strip()

    # Remove Conclusions section from citation processing (don't cite in conclusions)
    paper_for_sentences = paper
    conclusions_match = re.search(r"## Conclusions?\n\n", paper, re.IGNORECASE)
    if conclusions_match:
        paper_for_sentences = paper[: conclusions_match.end()]

    # Extract sentences and clean references
    sentences = _get_sentences(paper_for_sentences)

    if not sentences:
        # No sentences found, return with references appended
        references_lines = ["## References", ""]
        for i, ref in enumerate(references):
            references_lines.append(f"{i + 1}. {ref}")
        references_section = "\n".join(references_lines)

        return {
            "cited_text": text,
            "references_section": references_section,
            "citation_map": {i + 1: ref for i, ref in enumerate(references)},
            "text": text.strip() + "\n\n" + references_section,
        }

    # Compute embeddings for sentences and references
    try:
        emb_response_sents = client.embeddings.create(
            model=model,
            input=sentences,
            timeout=30.0,
        )
        emb_sents = _np.array([e.embedding for e in emb_response_sents.data])

        emb_response_refs = client.embeddings.create(
            model=model,
            input=references,
            timeout=30.0,
        )
        emb_refs = _np.array([e.embedding for e in emb_response_refs.data])
    except Exception as e:
        # Return original text with references appended on API failure
        references_lines = ["## References", ""]
        for i, ref in enumerate(references):
            references_lines.append(f"{i + 1}. {ref}")
        references_section = "\n".join(references_lines)

        return {
            "cited_text": text,
            "references_section": references_section,
            "citation_map": {i + 1: ref for i, ref in enumerate(references)},
            "text": text.strip() + "\n\n" + references_section,
            "error": f"OpenAI API error: {str(e)}",
        }

    # Compute similarity matrix and find best matches
    # similarities[i, j] = similarity between sentence i and reference j
    similarities = _np.dot(emb_sents, emb_refs.T)

    # For each reference, find the sentence with highest similarity
    citation_inserts = _np.argmax(similarities, axis=0)

    # Group citations by sentence
    citations: Dict[int, List[int]] = {}
    for ref_idx in range(len(citation_inserts)):
        sent_idx = int(citation_inserts[ref_idx])
        if sent_idx in citations:
            citations[sent_idx].append(ref_idx + 1)
        else:
            citations[sent_idx] = [ref_idx + 1]

    # Reorder references by first occurrence
    ordered_refs = _reorder_references(citations, references)

    # Insert citations into paper
    cited_paper = paper
    for sent_idx in citations:
        sentence = sentences[sent_idx]
        pos = cited_paper.find(sentence)
        if pos >= 0:
            end_pos = pos + len(sentence)
            # Format citation markers
            cit_markers = ",".join(str(i) for i in citations[sent_idx])
            # Insert before final punctuation if present
            if cited_paper[end_pos - 1] in ".!?":
                cited_paper = (
                    cited_paper[: end_pos - 1]
                    + f" [{cit_markers}]"
                    + cited_paper[end_pos - 1 :]
                )
            else:
                cited_paper = (
                    cited_paper[:end_pos] + f" [{cit_markers}]" + cited_paper[end_pos:]
                )

    # Generate References section
    references_lines = ["## References", ""]
    for i, ref in enumerate(ordered_refs):
        references_lines.append(f"{i + 1}. {ref.strip()}")
    references_section = "\n".join(references_lines)

    # Build citation map
    citation_map = {i + 1: ref for i, ref in enumerate(ordered_refs)}

    # Combine text and references
    full_text = cited_paper.strip() + "\n\n" + references_section

    return {
        "cited_text": cited_paper,
        "references_section": references_section,
        "citation_map": citation_map,
        "text": full_text,
    }


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register text actions in the YAMLEngine registry.

    Actions registered:
        - text.insert_citations: Insert citation markers using semantic matching

    Args:
        registry: Action registry to populate
        engine: YAMLEngine instance (not used by text actions)
    """

    def text_insert_citations_action(
        state: Dict[str, Any],
        text: str = "",
        references: Optional[List[str]] = None,
        model: str = "text-embedding-3-large",
        api_key: Optional[str] = None,
        base_url: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Wrapper for insert_citations with state access.

        See insert_citations() for full documentation.
        """
        return insert_citations(
            state=state,
            text=text,
            references=references or [],
            model=model,
            api_key=api_key,
            base_url=base_url,
            **kwargs,
        )

    # Register actions
    registry["text.insert_citations"] = text_insert_citations_action
    registry["actions.text_insert_citations"] = text_insert_citations_action
