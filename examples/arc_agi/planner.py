"""A* planner over the learned Movable rules.

State space: 2D position of the Agent on the grid.
Actions: each Movable rule maps `action_code → (dy, dx)`.
Goal: any position whose Manhattan distance to a known goal centroid is 0.

The planner uses the **learned rules** as the transition model — there is no
hard-coded knowledge of grid mechanics. If the agent has only discovered
ACTION1 → (-5, 0) so far, A* can only plan ACTION1 moves.
"""

from __future__ import annotations

import heapq
import re
from dataclasses import dataclass

from .agent_goal import AgentInfo, GoalInfo
from .worldview import Worldview


@dataclass(frozen=True)
class PlanStep:
    action_code: int
    expected_position: tuple[int, int]


def _exec(wv: Worldview, q: str, p: dict | None = None) -> list:
    res = wv.conn.execute(q, p or {})
    qr = res[0] if isinstance(res, list) else res
    rows = []
    while qr.has_next():
        rows.append(qr.get_next())
    return rows


def load_movable_rules(wv: Worldview) -> list[tuple[int, int, int]]:
    """Return promoted Movable rules as a list of (action_code, dy, dx)."""
    rows = _exec(wv,
        """
        MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)-[:RULE_INSTANCE_OF]->(:Concept {name:'Movable'})
        WHERE h.dsl STARTS WITH 'TRANSLATE'
        RETURN DISTINCT h.dsl
        """)
    moves: set[tuple[int, int, int]] = set()
    for (dsl,) in rows:
        m = re.search(r"action=(\d+).*?->\s*\((-?\d+),\s*(-?\d+)\)", dsl)
        if m:
            moves.add((int(m.group(1)), int(m.group(2)), int(m.group(3))))
    return sorted(moves)


def plan_path(
    agent: AgentInfo,
    goals: list[GoalInfo],
    moves: list[tuple[int, int, int]],
    *,
    grid_size: int = 64,
    max_iterations: int = 5000,
    goal_radius: int = 2,
    visit_counts: dict[tuple[int, int], int] | None = None,
    revisit_penalty: float = 0.5,
    turn_penalty: float = 0.3,
    reverse_penalty: float = 3.0,
    right_turn_bonus: float = 0.5,
    last_action_code: int | None = None,
) -> list[PlanStep]:
    """A* from agent position to any goal. Returns ordered list of action codes.

    Soft preferences (all finite, so the path is still optimal w.r.t. reaching
    the goal — they just bias among ties):

      * ``revisit_penalty * visit_count(next_pos)`` — prefer fresh cells
      * ``turn_penalty`` added when the chosen action differs from the
        previous step's action — prefer continuing in the same direction
    """
    if agent.last_known_centroid is None or not goals or not moves:
        return []

    start = (int(round(agent.last_known_centroid[0])),
             int(round(agent.last_known_centroid[1])))
    goal_pts = [(int(round(g.centroid[0])), int(round(g.centroid[1]))) for g in goals]
    visits: dict[tuple[int, int], int] = visit_counts or {}

    def heuristic(p: tuple[int, int]) -> float:
        return min(abs(p[0] - gy) + abs(p[1] - gx) for gy, gx in goal_pts)

    def is_goal(p: tuple[int, int]) -> bool:
        return any(abs(p[0] - gy) + abs(p[1] - gx) <= goal_radius for gy, gx in goal_pts)

    from .directions import build_direction_index
    dirs = build_direction_index(moves)
    opposite = dirs["opposite"]
    right_of = dirs["right_of"]

    # Counter to break heap-priority ties without comparing PlanStep lists.
    counter = 0
    frontier: list[tuple[float, int, float, tuple[int, int], int, list[PlanStep]]] = []
    heapq.heappush(
        frontier,
        (heuristic(start), counter, 0.0, start,
         last_action_code if last_action_code is not None else -1, []),
    )
    # Track best cost per (pos, last_action) so different "incoming directions"
    # to the same cell get distinct entries.
    best_cost: dict[tuple[tuple[int, int], int], float] = {(start, -1): 0.0}
    iters = 0

    while frontier and iters < max_iterations:
        iters += 1
        _, _, cost, pos, prev_action, path = heapq.heappop(frontier)
        if is_goal(pos):
            return path
        for action_code, dy, dx in moves:
            ny, nx = pos[0] + dy, pos[1] + dx
            if not (0 <= ny < grid_size and 0 <= nx < grid_size):
                continue
            step_cost = 1.0 + revisit_penalty * float(visits.get((ny, nx), 0))
            if prev_action >= 0 and action_code != prev_action:
                # Direction change. Apply right-hand vs U-turn shaping.
                if action_code == opposite(prev_action):
                    step_cost += reverse_penalty
                elif action_code == right_of(prev_action):
                    step_cost -= right_turn_bonus
                else:
                    step_cost += turn_penalty
                # Floor at a small positive so each step still has cost > 0.
                step_cost = max(step_cost, 0.1)
            new_cost = cost + step_cost
            key = ((ny, nx), action_code)
            if key in best_cost and best_cost[key] <= new_cost:
                continue
            best_cost[key] = new_cost
            new_path = path + [
                PlanStep(action_code=action_code, expected_position=(ny, nx))
            ]
            counter += 1
            heapq.heappush(
                frontier,
                (new_cost + heuristic((ny, nx)), counter, new_cost,
                 (ny, nx), action_code, new_path),
            )
    return []
