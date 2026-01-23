#!/usr/bin/env python3
"""Generate word embeddings Parquet file for TEA-GAME-001.3.

Uses sentence-transformers (all-MiniLM-L6-v2) to generate 384-dimensional
embeddings for common English words.

Usage:
    python generate_word_embeddings.py [--output data/words.parquet] [--words 10000]

Requirements:
    pip install sentence-transformers pyarrow nltk

AC-1: Pre-computed word embeddings loaded into DuckDB words table (common vocabulary ~10k words)
"""

import argparse
import logging
import sys
from pathlib import Path
from typing import Optional

import pyarrow as pa
import pyarrow.parquet as pq

try:
    from sentence_transformers import SentenceTransformer
except ImportError:
    print(
        "Error: sentence-transformers not installed. Run: pip install sentence-transformers"
    )
    sys.exit(1)

try:
    import nltk
    from nltk.corpus import words as nltk_words
except ImportError:
    print("Error: nltk not installed. Run: pip install nltk")
    sys.exit(1)

logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


def ensure_nltk_data() -> None:
    """Download NLTK word corpus if not present."""
    try:
        nltk_words.words()
    except LookupError:
        logger.info("Downloading NLTK words corpus...")
        nltk.download("words", quiet=True)


def get_common_words(max_words: int = 10000) -> list[str]:
    """Get common English words from NLTK corpus.

    Filters to:
    - Lowercase words only
    - Length 3-15 characters
    - Alphabetic only (no numbers or special chars)
    - Single words (no phrases)

    Args:
        max_words: Maximum number of words to return

    Returns:
        List of unique lowercase words
    """
    ensure_nltk_data()

    all_words = set(nltk_words.words())

    # Filter words
    filtered = []
    for word in all_words:
        word_lower = word.lower()
        # Skip if not alphabetic, too short/long, or has uppercase (proper nouns)
        if (
            word_lower.isalpha()
            and 3 <= len(word_lower) <= 15
            and word == word_lower  # Skip proper nouns (start with uppercase)
        ):
            filtered.append(word_lower)

    # Deduplicate and sort for reproducibility
    unique_words = sorted(set(filtered))

    logger.info(f"Found {len(unique_words)} unique words after filtering")

    # Take first N words (sorted alphabetically for reproducibility)
    return unique_words[:max_words]


def generate_embeddings(
    words: list[str],
    model_name: str = "all-MiniLM-L6-v2",
    batch_size: int = 256,
) -> list[list[float]]:
    """Generate embeddings for words using sentence-transformers.

    Args:
        words: List of words to embed
        model_name: Sentence transformer model name
        batch_size: Batch size for embedding generation

    Returns:
        List of 384-dimensional embedding vectors
    """
    logger.info(f"Loading model {model_name}...")
    model = SentenceTransformer(model_name)

    logger.info(f"Generating embeddings for {len(words)} words...")
    embeddings = model.encode(
        words,
        batch_size=batch_size,
        show_progress_bar=True,
        convert_to_numpy=True,
    )

    return embeddings.tolist()


def create_parquet_file(
    words: list[str],
    embeddings: list[list[float]],
    output_path: Path,
) -> None:
    """Create Parquet file with word embeddings.

    Schema:
    - id: string (UUID)
    - text: string (lowercase word)
    - embedding: list<float32>[384]
    - frequency: int64 (optional, for filtering)

    Args:
        words: List of words
        embeddings: List of embedding vectors
        output_path: Output Parquet file path
    """
    import uuid

    logger.info(f"Creating Parquet file at {output_path}...")

    # Create data arrays
    ids = [str(uuid.uuid4()) for _ in words]
    texts = words
    frequencies = [
        len(words) - i for i, _ in enumerate(words)
    ]  # Simple frequency proxy

    # Create PyArrow table
    table = pa.table(
        {
            "id": pa.array(ids, type=pa.string()),
            "text": pa.array(texts, type=pa.string()),
            "embedding": pa.array(embeddings, type=pa.list_(pa.float32(), 384)),
            "frequency": pa.array(frequencies, type=pa.int64()),
        }
    )

    # Write to Parquet
    pq.write_table(table, output_path, compression="snappy")

    file_size = output_path.stat().st_size / (1024 * 1024)
    logger.info(f"Created {output_path} ({file_size:.2f} MB) with {len(words)} words")


def main(
    output_path: Optional[Path] = None,
    max_words: int = 10000,
) -> None:
    """Main entry point.

    Args:
        output_path: Output Parquet file path
        max_words: Maximum number of words to include
    """
    if output_path is None:
        output_path = Path(__file__).parent.parent / "data" / "words.parquet"

    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    logger.info(f"Generating word embeddings ({max_words} words max)...")

    # Get common words
    words = get_common_words(max_words)
    logger.info(f"Selected {len(words)} words for embedding")

    # Generate embeddings
    embeddings = generate_embeddings(words)

    # Create Parquet file
    create_parquet_file(words, embeddings, output_path)

    logger.info("Done!")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate word embeddings Parquet file for TEA-GAME-001.3"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Output Parquet file path (default: data/words.parquet)",
    )
    parser.add_argument(
        "--words",
        type=int,
        default=10000,
        help="Maximum number of words to include (default: 10000)",
    )

    args = parser.parse_args()
    main(output_path=args.output, max_words=args.words)
