"""LLM-based shape naming.

Batches unnamed shapes (n_observations ≥ ``min_observations``) and asks Claude
via subprocess for a one-word name per shape. The CLI invocation mirrors
TEA's ``shell_provider: claude`` pattern in ``llm_actions._execute_shell_provider``.

Names land in ``Shape.llm_name`` and feed into the name → concept classifier.
"""

from __future__ import annotations

import json
import os
import re
import subprocess

from .worldview import Worldview


SYSTEM_PROMPT = """\
You are looking at small pixel-art shapes from a grid puzzle game. Each shape is
shown as rows of '1' (filled) and '0' (empty). Some shapes are annotated with
"MOTION-CONTEXT: moved under N actions" — that strongly suggests they're agent-
like (the thing the player controls).

For each shape, give a single lowercase noun describing what it most resembles.
Bias your choice with the motion context:
  - moved under ≥ 3 distinct actions → 'avatar' or 'player'
  - moved under exactly 1-2 actions → 'block' or 'pusher'
  - never moved → 'wall', 'panel', 'target', 'border', 'background', 'corner',
                  'icon', 'arrow', etc. by shape

Respond with STRICT JSON only:
{
  "names": {
    "<shape_hash_full>": "<noun>",
    ...
  }
}
"""


def _format_batch_prompt(batch: list[tuple[str, str, int]]) -> str:
    """batch: list of (shape_hash, bitmap_ascii, n_distinct_actions_moved_under)."""
    parts = ["Name these shapes. Respond JSON only.\n"]
    for sh, bitmap, n_actions in batch:
        ctx = (
            f"MOTION-CONTEXT: moved under {n_actions} distinct action(s)"
            if n_actions > 0
            else "MOTION-CONTEXT: never observed moving"
        )
        parts.append(f"\nSHAPE {sh}:\n{ctx}\n{bitmap}")
    return SYSTEM_PROMPT + "\n\n" + "\n".join(parts)


def name_unnamed_shapes(
    worldview: Worldview,
    *,
    min_observations: int = 3,
    batch_size: int = 10,
    max_area: int = 200,
    cli: str = "claude",
    timeout: int = 120,
) -> int:
    """Call Claude to name shapes that have been seen ≥ min_observations and
    don't yet have a name. Returns the number of shapes named.

    Skips overly large shapes (area > max_area, typically the full-grid background)
    since they're rarely semantically useful and bloat the prompt.
    """
    if os.environ.get("ARC_AGI_DISABLE_LLM"):
        return 0

    res = worldview.conn.execute(
        """
        MATCH (s:Shape)
        WHERE s.llm_name = '' AND s.n_observations >= $min AND s.area <= $max_area
        RETURN s.shape_hash, s.bitmap_ascii
        ORDER BY s.n_observations DESC
        LIMIT $batch
        """,
        {"min": min_observations, "max_area": max_area, "batch": batch_size},
    )
    qr = res[0] if isinstance(res, list) else res
    raw: list[tuple[str, str]] = []
    while qr.has_next():
        row = qr.get_next()
        raw.append((row[0], row[1]))
    if not raw:
        return 0

    # Enrich each shape with motion context: number of distinct action codes
    # under which an entity of this shape was part of a MotionCluster.
    batch: list[tuple[str, str, int]] = []
    for sh, bitmap in raw:
        ctx_res = worldview.conn.execute(
            """
            MATCH (e:Entity {shape_hash: $sh})-[:PART_OF]->(cl:Cluster {kind: 'motion'})
            MATCH (o:Observation)-[t:TRANSITION]->(:Observation)-[:OBSERVED_AS]->(cl)
            RETURN count(DISTINCT t.action_code) AS n
            """,
            {"sh": sh},
        )
        ctx_qr = ctx_res[0] if isinstance(ctx_res, list) else ctx_res
        n_actions = 0
        if ctx_qr.has_next():
            row = ctx_qr.get_next()
            n_actions = int(row[0]) if row[0] is not None else 0
        batch.append((sh, bitmap, n_actions))

    prompt = _format_batch_prompt(batch)
    try:
        result = subprocess.run(
            [cli, "-p", prompt, "--dangerously-skip-permissions", "--no-session-persistence"],
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return 0
    if result.returncode != 0:
        return 0

    match = re.search(r"\{.*\}", result.stdout, re.DOTALL)
    if not match:
        return 0
    try:
        data = json.loads(match.group(0))
        names = data.get("names", {})
    except (json.JSONDecodeError, ValueError):
        return 0

    n_named = 0
    for shape_hash, name in names.items():
        if not isinstance(name, str) or not name.strip():
            continue
        clean = name.strip().lower()[:30]
        worldview.conn.execute(
            "MATCH (s:Shape {shape_hash: $sh}) "
            "SET s.llm_name = $name, s.llm_confidence = 0.7",
            {"sh": shape_hash, "name": clean},
        )
        n_named += 1
    return n_named
