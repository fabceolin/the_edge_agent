"""LLM fallback via the `claude` CLI subprocess (no API key needed).

Mirrors TEA's `shell_provider: claude` pattern in
``the_edge_agent.actions.llm_actions._execute_shell_provider``.

When the symbolic reasoner has no signal, this module sends Claude:
  - the compact grid (hex per cell)
  - the entity list
  - the rules already learned
and asks for a goal hypothesis + suggested action. The reply is parsed and
written into the worldview as a high-support Hypothesis so the reasoner can
exploit it.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
from dataclasses import dataclass

import numpy as np

from .sensors import Perception
from .worldview import Worldview


SYSTEM_PROMPT = """\
You are reasoning about an unknown grid-based puzzle game (ARC-AGI-3 style).
Each frame is a 64x64 grid of integer colors. The agent has these actions: ACTION1..ACTION4.
Based on the frame data the user gives you, infer:
  1. what configuration likely scores
  2. which action seems most useful right now

Respond with STRICT JSON only, no prose, no markdown fences:
{
  "goal_hypothesis": "<short natural-language description of what likely scores>",
  "suggested_action": <1|2|3|4>,
  "rationale": "<one short sentence>"
}
"""


@dataclass
class LLMSuggestion:
    goal: str
    suggested_action: int
    rationale: str


def _grid_to_compact_str(grid: np.ndarray) -> str:
    return "\n".join("".join(format(int(v), "x") for v in row) for row in grid)


def _entities_summary(perception: Perception) -> list[dict]:
    return [
        {
            "color": e.color,
            "bbox": list(e.bbox),
            "area": e.area,
            "shape_hash": e.shape_hash[:8],
        }
        for e in perception.entities[:32]
    ]


def _rules_summary(wv: Worldview, limit: int = 16) -> list[str]:
    res = wv.conn.execute(f"MATCH (r:Rule) RETURN r.prolog_src LIMIT {limit}")
    qr = res[0] if isinstance(res, list) else res
    out: list[str] = []
    while qr.has_next():
        out.append(qr.get_next()[0])
    return out


def _build_prompt(perception: Perception, worldview: Worldview) -> str:
    payload = {
        "grid_hex": _grid_to_compact_str(perception.grid),
        "entities": _entities_summary(perception),
        "learned_rules": _rules_summary(worldview),
    }
    return f"{SYSTEM_PROMPT}\n\nUSER:\n{json.dumps(payload)}"


def query_llm(
    perception: Perception,
    worldview: Worldview,
    *,
    cli: str = "claude",
    timeout: int = 120,
) -> LLMSuggestion | None:
    """Shell out to the `claude` CLI to get a goal/action suggestion."""
    if os.environ.get("ARC_AGI_DISABLE_LLM"):
        return None
    prompt = _build_prompt(perception, worldview)
    try:
        result = subprocess.run(
            [
                cli,
                "-p", prompt,
                "--dangerously-skip-permissions",
                "--no-session-persistence",
            ],
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return None
    if result.returncode != 0:
        return None

    text = result.stdout
    match = re.search(r"\{.*\}", text, re.DOTALL)
    if not match:
        return None
    try:
        data = json.loads(match.group(0))
        action = int(data.get("suggested_action", 1))
        if action not in (1, 2, 3, 4):
            return None
        return LLMSuggestion(
            goal=str(data.get("goal_hypothesis", "")),
            suggested_action=action,
            rationale=str(data.get("rationale", "")),
        )
    except (json.JSONDecodeError, ValueError, TypeError):
        return None


def write_suggestion_to_graph(
    worldview: Worldview,
    suggestion: LLMSuggestion,
    *,
    initial_support: int = 3,
) -> str:
    """Persist the LLM-proposed goal as a high-support hypothesis."""
    dsl = f"LLM_GOAL(action={suggestion.suggested_action}) -> {suggestion.goal!r}"
    hyp_id = worldview.upsert_hypothesis(dsl, suggestion.suggested_action)
    if initial_support > 1:
        worldview.conn.execute(
            "MATCH (h:Hypothesis {id: $id}) SET h.support = h.support + $boost",
            {"id": hyp_id, "boost": initial_support - 1},
        )
    return hyp_id
