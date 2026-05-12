"""Map LLM-given shape names → kernel role concepts.

Hybrid: keyword dictionary first (cheap, deterministic), LLM fallback for
unknown names. Once classified, the Shape is wired to its role concept via
the existing CLASSIFIED_AS-style edge, and any Entity with that shape inherits
the role.
"""

from __future__ import annotations

import json
import os
import re
import subprocess

from .worldview import Worldview


# Keyword → kernel concept name. Lowercase keys.
NAME_TO_CONCEPT: dict[str, str] = {
    # Agent / player
    "avatar": "Agent",
    "player": "Agent",
    "character": "Agent",
    "hero": "Agent",
    "creature": "Agent",
    "robot": "Agent",
    # Goals / targets
    "target": "Goal",
    "goal": "Goal",
    "flag": "Goal",
    "destination": "Goal",
    "checkpoint": "Goal",
    "marker": "Goal",
    # Collectibles
    "coin": "Collectible",
    "gem": "Collectible",
    "star": "Collectible",
    "key": "Collectible",
    "diamond": "Collectible",
    "fruit": "Collectible",
    "pickup": "Collectible",
    "token": "Collectible",
    # Obstacles
    "wall": "Obstacle",
    "block": "Obstacle",
    "stone": "Obstacle",
    "rock": "Obstacle",
    "brick": "Obstacle",
    "fence": "Obstacle",
    "barrier": "Obstacle",
    # Container / panel
    "box": "Container",
    "container": "Container",
    "panel": "Container",
    "frame": "Container",
    "room": "Container",
    # Triggers / toggles
    "switch": "Trigger",
    "lever": "Trigger",
    "button": "Trigger",
    "trigger": "Trigger",
    # PowerUp
    "powerup": "PowerUp",
    "boost": "PowerUp",
    "potion": "PowerUp",
    # Direction
    "arrow": "DirectionIndicator",
    "pointer": "DirectionIndicator",
    "cursor": "DirectionIndicator",
    "compass": "DirectionIndicator",
    # HUD
    "score": "HUDElement",
    "bar": "HUDElement",
    "indicator": "HUDElement",
    "meter": "HUDElement",
    "icon": "HUDElement",
    "label": "HUDElement",
    "text": "HUDElement",
    "digit": "HUDElement",
    "number": "HUDElement",
    # Background / border
    "background": "Background",
    "floor": "Background",
    "void": "Background",
    "empty": "Background",
    "pixel": "Background",
    "border": "Border",
    "edge": "Border",
    "corner": "Border",
    "line": "Border",
}


def classify_by_keyword(name: str) -> str | None:
    name = (name or "").strip().lower()
    if not name:
        return None
    return NAME_TO_CONCEPT.get(name)


def _llm_classify_batch(unknown_names: list[str], *, cli: str = "claude", timeout: int = 120) -> dict[str, str]:
    """Fallback LLM classification for names not in the keyword dict."""
    if os.environ.get("ARC_AGI_DISABLE_LLM"):
        return {}
    if not unknown_names:
        return {}

    valid_concepts = sorted(set(NAME_TO_CONCEPT.values()))
    prompt = (
        "Classify each of these short shape names into ONE of these kernel concepts:\n"
        f"{', '.join(valid_concepts)}.\n\n"
        "Use 'Background' if it's clearly a non-interactive visual element.\n"
        "Respond STRICT JSON only:\n"
        '{ "classifications": { "<name>": "<Concept>", ... } }\n\n'
        f"Names: {json.dumps(unknown_names)}"
    )
    try:
        result = subprocess.run(
            [cli, "-p", prompt, "--dangerously-skip-permissions", "--no-session-persistence"],
            capture_output=True, text=True, timeout=timeout, check=False,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return {}
    if result.returncode != 0:
        return {}
    m = re.search(r"\{.*\}", result.stdout, re.DOTALL)
    if not m:
        return {}
    try:
        data = json.loads(m.group(0)).get("classifications", {})
    except (json.JSONDecodeError, ValueError):
        return {}
    return {
        n: c for n, c in data.items()
        if isinstance(c, str) and c in valid_concepts
    }


def classify_shapes(worldview: Worldview) -> int:
    """For every named Shape without a role_concept, run keyword + LLM classification.
    Persists role_concept on Shape and creates CLASSIFIED_AS edges (via RULE_INSTANCE_OF
    style indirection — here we reuse the Concept linkage)."""
    res = worldview.conn.execute(
        "MATCH (s:Shape) "
        "WHERE s.llm_name <> '' AND s.role_concept = '' "
        "RETURN s.shape_hash, s.llm_name"
    )
    qr = res[0] if isinstance(res, list) else res
    pending: list[tuple[str, str]] = []
    while qr.has_next():
        row = qr.get_next()
        pending.append((row[0], row[1]))

    keyword_hits: dict[str, str] = {}
    unknown: list[str] = []
    for sh, name in pending:
        c = classify_by_keyword(name)
        if c is not None:
            keyword_hits[sh] = c
        else:
            unknown.append(name)

    llm_hits: dict[str, str] = {}
    if unknown:
        llm_names_to_concepts = _llm_classify_batch(list(set(unknown)))
        for sh, name in pending:
            if sh in keyword_hits:
                continue
            if name in llm_names_to_concepts:
                llm_hits[sh] = llm_names_to_concepts[name]

    all_hits = {**keyword_hits, **llm_hits}
    for shape_hash, concept_name in all_hits.items():
        worldview.conn.execute(
            "MATCH (s:Shape {shape_hash: $sh}) SET s.role_concept = $rc",
            {"sh": shape_hash, "rc": concept_name},
        )
    return len(all_hits)
