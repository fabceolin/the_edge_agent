"""Symbolic hypothesis induction from frame-to-frame diffs.

Generates candidate rules in a tiny DSL:

  TRANSLATE(action=N, shape_hash=H) -> (dy, dx)
      meaning: when ACTION N is applied, entities matching shape H shift by (dy, dx).

  RECOLOR(action=N, r=R, c=C) -> color
      meaning: when ACTION N is applied, cell (R, C) becomes `color`.

Hypotheses are intentionally narrow so that subsequent transitions can support or
refute them with high confidence.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .sensors import Entity, Perception


@dataclass(frozen=True)
class Hypothesis:
    dsl: str
    action_code: int


def _match_entities_by_shape(prev: list[Entity], curr: list[Entity]) -> list[tuple[Entity, Entity]]:
    pairs: list[tuple[Entity, Entity]] = []
    used: set[int] = set()
    for p in prev:
        candidates = [
            (j, c) for j, c in enumerate(curr)
            if j not in used and c.shape_hash == p.shape_hash and c.color == p.color
        ]
        if not candidates:
            continue
        j, c = min(
            candidates,
            key=lambda jc: (jc[1].centroid[0] - p.centroid[0]) ** 2
                          + (jc[1].centroid[1] - p.centroid[1]) ** 2,
        )
        used.add(j)
        pairs.append((p, c))
    return pairs


def induce(prev: Perception, action_code: int, curr: Perception) -> list[Hypothesis]:
    """Produce a small set of candidate hypotheses explaining prev → curr under action."""
    hyps: list[Hypothesis] = []

    # Translation hypotheses (per matched entity).
    for p, c in _match_entities_by_shape(prev.entities, curr.entities):
        dy = round(c.centroid[0] - p.centroid[0])
        dx = round(c.centroid[1] - p.centroid[1])
        if dy == 0 and dx == 0:
            continue
        hyps.append(Hypothesis(
            dsl=f"TRANSLATE(action={action_code}, shape_hash={p.shape_hash}) -> ({dy}, {dx})",
            action_code=action_code,
        ))

    # Recolor hypotheses: cells whose color changed.
    if prev.grid.shape == curr.grid.shape:
        diff = prev.grid != curr.grid
        # Cap to avoid hypothesis explosion on visual-only changes.
        ys, xs = np.where(diff)
        for y, x in list(zip(ys.tolist(), xs.tolist()))[:8]:
            hyps.append(Hypothesis(
                dsl=f"RECOLOR(action={action_code}, r={y}, c={x}) -> {int(curr.grid[y, x])}",
                action_code=action_code,
            ))

    return hyps
