"""Prolog-driven goal backtracking selector.

The Python side enumerates candidates and writes them as Prolog facts:

    candidate_goal(g1, 12, 36, foreground_entity).
    candidate_goal(g2, 61, 62, anchor_heuristic).
    candidate_goal(g3, 45, 25, diff_region).
    attempted(g1, no_reward).
    attempted(g2, no_reward).

A simple Prolog rule asks for the next candidate that hasn't been tried, and
``pyswip`` enumerates them via backtracking on successive queries.

Attempt outcomes are persisted as :GoalAttempt nodes in the Kuzu worldview so
the memory carries across replans and episodes.
"""

from __future__ import annotations

import hashlib
import uuid
from dataclasses import dataclass
from datetime import datetime, timezone

from pyswip import Prolog  # type: ignore[import-untyped]

from .agent_goal import GoalInfo
from .worldview import Worldview


_PROLOG = Prolog()
_PROLOG_READY = False


def _ensure_prolog_ready() -> None:
    global _PROLOG_READY
    if _PROLOG_READY:
        return
    # Declare dynamic predicates and the backtracking rule. Each assertz call
    # gets a single-line clause to avoid pyswip line-splitting issues.
    list(_PROLOG.query("dynamic(candidate_goal/4)"))
    list(_PROLOG.query("dynamic(attempted/2)"))
    _PROLOG.assertz(
        "next_to_try(G, Y, X, Source) :- "
        "candidate_goal(G, Y, X, Source), \\+ attempted(G, _)"
    )
    _PROLOG_READY = True


def _goal_signature(g: GoalInfo) -> str:
    raw = f"{g.source}:{round(g.centroid[0])}:{round(g.centroid[1])}:{g.color}"
    return hashlib.blake2b(raw.encode(), digest_size=6).hexdigest()


@dataclass
class NextGoal:
    sig: str
    centroid: tuple[float, float]
    source: str


def select_next_goal(
    wv: Worldview, candidates: list[GoalInfo]
) -> NextGoal | None:
    """Use Prolog backtracking to pick the next candidate that hasn't been
    marked as ``attempted`` in the worldview. Returns ``None`` if all
    candidates have been exhausted."""
    if not candidates:
        return None
    _ensure_prolog_ready()

    # Reset the per-call Prolog state.
    list(_PROLOG.query("retractall(candidate_goal(_, _, _, _))"))
    list(_PROLOG.query("retractall(attempted(_, _))"))

    by_sig: dict[str, GoalInfo] = {}
    for g in candidates:
        sig = _goal_signature(g)
        by_sig[sig] = g
        _PROLOG.assertz(
            f"candidate_goal('{sig}', {int(round(g.centroid[0]))}, "
            f"{int(round(g.centroid[1]))}, '{g.source}')"
        )

    # Replay past attempts from Kuzu.
    res = wv.conn.execute(
        "MATCH (a:GoalAttempt) WHERE a.status <> 'success' "
        "RETURN a.goal_signature, a.status"
    )
    qr = res[0] if isinstance(res, list) else res
    while qr.has_next():
        row = qr.get_next()
        sig, status = row[0], row[1]
        _PROLOG.assertz(f"attempted('{sig}', '{status}')")

    # Ask Prolog for the next candidate to try.
    for sol in _PROLOG.query("next_to_try(G, Y, X, Source)"):
        sig = str(sol["G"])
        if sig not in by_sig:
            continue
        return NextGoal(sig=sig, centroid=by_sig[sig].centroid,
                        source=by_sig[sig].source)
    return None


def record_attempt(
    wv: Worldview,
    *,
    sig: str,
    centroid: tuple[float, float],
    source: str,
    frames_invested: int,
    reward_delta: int,
) -> None:
    """Persist the outcome of a goal-attempt in :GoalAttempt."""
    attempt_id = f"att:{uuid.uuid4().hex[:10]}"
    status = "success" if reward_delta > 0 else "no_reward"
    wv.conn.execute(
        "CREATE (a:GoalAttempt {id: $id, goal_signature: $sig, "
        "centroid_y: $cy, centroid_x: $cx, source: $src, "
        "attempted_at: $ts, frames_invested: $fi, reward_delta: $rd, status: $st})",
        {
            "id": attempt_id, "sig": sig,
            "cy": centroid[0], "cx": centroid[1], "src": source,
            "ts": datetime.now(timezone.utc),
            "fi": frames_invested, "rd": reward_delta, "st": status,
        },
    )
