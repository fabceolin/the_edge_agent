#!/usr/bin/env python3
"""
Generate game phrases for "Know Your Model" using Claude CLI.

Usage:
    python scripts/generate_game_phrases.py --batches 20 --output data/game_phrases.json

This script:
1. Runs claude -p with the phrase generation prompt
2. Parses JSON output from each batch
3. Deduplicates phrases
4. Saves consolidated output to JSON file
"""

import argparse
import json
import os
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path


PROMPT = """Generate 50 fill-in-the-blank phrases for a word-guessing game called "Know Your Model".

Output a JSON array with this exact structure:
[
  {
    "id": "phrase_001",
    "phrase": "The ___ rises in the east.",
    "correct_word": "sun",
    "distractors": ["moon", "star", "light", "day"],
    "difficulty": 0.2,
    "category": "nature"
  }
]

Requirements:
1. Each phrase has exactly ONE blank marked as ___
2. correct_word is the most natural, expected answer that an LLM would likely choose
3. 4 distractors: words that are plausible but less fitting (make them believable!)
4. difficulty scale:
   - 0.0-0.3 (easy): Obvious answer, strong context clues
   - 0.3-0.6 (medium): Multiple plausible answers, one clearly best
   - 0.6-1.0 (hard): Ambiguous context, several valid options
5. Categories: nature, emotions, daily_life, food, weather, science, animals, actions, objects, technology, relationships
6. Distribution: ~40% easy, ~35% medium, ~25% hard
7. Words must be common single English words (no proper nouns, no rare/archaic words)
8. Phrases should be 4-10 words long and grammatically correct
9. Vary sentence structures: statements, observations, descriptions
10. Distractors should be the SAME part of speech as correct_word

Output ONLY valid JSON array, no explanations or markdown formatting."""


def run_claude_prompt(prompt: str, batch_num: int) -> str:
    """Run claude -p with the given prompt and return output."""
    print(f"  Running Claude for batch {batch_num}...")

    try:
        result = subprocess.run(
            ["claude", "-p", prompt],
            capture_output=True,
            text=True,
            timeout=120,  # 2 minute timeout per batch
        )

        if result.returncode != 0:
            print(f"  Warning: Claude returned non-zero exit code: {result.returncode}")
            print(f"  stderr: {result.stderr[:500] if result.stderr else 'none'}")

        return result.stdout
    except subprocess.TimeoutExpired:
        print(f"  Error: Timeout waiting for Claude response")
        return ""
    except FileNotFoundError:
        print("  Error: 'claude' command not found. Make sure Claude CLI is installed.")
        sys.exit(1)


def extract_json_array(text: str) -> list:
    """Extract JSON array from text, handling markdown code blocks."""
    # Remove markdown code blocks if present
    text = re.sub(r"```json\s*", "", text)
    text = re.sub(r"```\s*", "", text)
    text = text.strip()

    # Find JSON array
    match = re.search(r"\[[\s\S]*\]", text)
    if not match:
        return []

    try:
        return json.loads(match.group())
    except json.JSONDecodeError as e:
        print(f"  Warning: JSON parse error: {e}")
        return []


def validate_phrase(phrase: dict) -> bool:
    """Validate a phrase entry has required fields."""
    required = ["phrase", "correct_word", "distractors", "difficulty"]

    for field in required:
        if field not in phrase:
            return False

    # Check phrase has blank
    if "___" not in phrase.get("phrase", ""):
        return False

    # Check distractors is list with 4 items
    distractors = phrase.get("distractors", [])
    if not isinstance(distractors, list) or len(distractors) < 4:
        return False

    # Check difficulty is numeric
    difficulty = phrase.get("difficulty")
    if not isinstance(difficulty, (int, float)) or not (0 <= difficulty <= 1):
        return False

    return True


def deduplicate_phrases(phrases: list) -> list:
    """Remove duplicate phrases based on the phrase text."""
    seen = set()
    unique = []

    for p in phrases:
        phrase_text = p.get("phrase", "").lower().strip()
        if phrase_text and phrase_text not in seen:
            seen.add(phrase_text)
            unique.append(p)

    return unique


def assign_ids(phrases: list) -> list:
    """Assign sequential IDs to phrases."""
    for i, p in enumerate(phrases, 1):
        p["id"] = f"phrase_{i:04d}"
    return phrases


def get_difficulty_distribution(phrases: list) -> dict:
    """Calculate difficulty distribution."""
    easy = sum(1 for p in phrases if p.get("difficulty", 0.5) < 0.3)
    medium = sum(1 for p in phrases if 0.3 <= p.get("difficulty", 0.5) < 0.6)
    hard = sum(1 for p in phrases if p.get("difficulty", 0.5) >= 0.6)

    total = len(phrases) or 1
    return {
        "easy": {"count": easy, "percent": round(easy / total * 100, 1)},
        "medium": {"count": medium, "percent": round(medium / total * 100, 1)},
        "hard": {"count": hard, "percent": round(hard / total * 100, 1)},
    }


def get_category_distribution(phrases: list) -> dict:
    """Calculate category distribution."""
    categories = {}
    for p in phrases:
        cat = p.get("category", "unknown")
        categories[cat] = categories.get(cat, 0) + 1
    return dict(sorted(categories.items(), key=lambda x: -x[1]))


def main():
    parser = argparse.ArgumentParser(
        description="Generate game phrases using Claude CLI"
    )
    parser.add_argument(
        "--batches",
        type=int,
        default=20,
        help="Number of batches to generate (50 phrases each)",
    )
    parser.add_argument(
        "--output",
        type=str,
        default="data/game_phrases.json",
        help="Output JSON file path",
    )
    parser.add_argument(
        "--batch-dir",
        type=str,
        default="data/phrase_batches",
        help="Directory to save individual batches",
    )
    parser.add_argument(
        "--resume", action="store_true", help="Resume from existing batches"
    )
    args = parser.parse_args()

    # Create directories
    output_path = Path(args.output)
    batch_dir = Path(args.batch_dir)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    batch_dir.mkdir(parents=True, exist_ok=True)

    all_phrases = []

    # Load existing batches if resuming
    if args.resume:
        existing = list(batch_dir.glob("batch_*.json"))
        print(f"Found {len(existing)} existing batches")
        for batch_file in sorted(existing):
            try:
                with open(batch_file) as f:
                    batch_phrases = json.load(f)
                    all_phrases.extend(batch_phrases)
                    print(
                        f"  Loaded {len(batch_phrases)} phrases from {batch_file.name}"
                    )
            except Exception as e:
                print(f"  Warning: Could not load {batch_file}: {e}")

        start_batch = len(existing) + 1
    else:
        start_batch = 1

    # Generate new batches
    print(
        f"\nGenerating {args.batches - start_batch + 1} batches of 50 phrases each..."
    )

    for batch_num in range(start_batch, args.batches + 1):
        print(f"\nBatch {batch_num}/{args.batches}:")

        # Check if batch already exists
        batch_file = batch_dir / f"batch_{batch_num:02d}.json"
        if batch_file.exists() and args.resume:
            print(f"  Skipping (already exists)")
            continue

        # Run Claude
        output = run_claude_prompt(PROMPT, batch_num)

        if not output:
            print(f"  Warning: Empty response, skipping batch")
            continue

        # Parse JSON
        phrases = extract_json_array(output)
        print(f"  Parsed {len(phrases)} phrases")

        # Validate
        valid_phrases = [p for p in phrases if validate_phrase(p)]
        invalid_count = len(phrases) - len(valid_phrases)
        if invalid_count > 0:
            print(f"  Warning: {invalid_count} invalid phrases removed")

        # Save batch
        with open(batch_file, "w") as f:
            json.dump(valid_phrases, f, indent=2)
        print(f"  Saved to {batch_file}")

        all_phrases.extend(valid_phrases)

    # Deduplicate
    print(f"\nDeduplicating {len(all_phrases)} total phrases...")
    unique_phrases = deduplicate_phrases(all_phrases)
    duplicates_removed = len(all_phrases) - len(unique_phrases)
    print(f"  Removed {duplicates_removed} duplicates")

    # Assign IDs
    unique_phrases = assign_ids(unique_phrases)

    # Calculate stats
    difficulty_dist = get_difficulty_distribution(unique_phrases)
    category_dist = get_category_distribution(unique_phrases)

    # Build final output
    output_data = {
        "version": "1.0",
        "generated_at": datetime.now().isoformat(),
        "total_phrases": len(unique_phrases),
        "difficulty_distribution": difficulty_dist,
        "category_distribution": category_dist,
        "phrases": unique_phrases,
    }

    # Save final output
    with open(output_path, "w") as f:
        json.dump(output_data, f, indent=2)

    print(f"\n{'='*60}")
    print(f"GENERATION COMPLETE")
    print(f"{'='*60}")
    print(f"Total phrases: {len(unique_phrases)}")
    print(f"Output file: {output_path}")
    print(f"\nDifficulty distribution:")
    print(
        f"  Easy (0.0-0.3):   {difficulty_dist['easy']['count']:4d} ({difficulty_dist['easy']['percent']}%)"
    )
    print(
        f"  Medium (0.3-0.6): {difficulty_dist['medium']['count']:4d} ({difficulty_dist['medium']['percent']}%)"
    )
    print(
        f"  Hard (0.6-1.0):   {difficulty_dist['hard']['count']:4d} ({difficulty_dist['hard']['percent']}%)"
    )
    print(f"\nTop categories:")
    for cat, count in list(category_dist.items())[:5]:
        print(f"  {cat}: {count}")


if __name__ == "__main__":
    main()
