"""Direction helpers derived from the learned movement model.

The Movable rules give us, for each action_code, a (dy, dx) translation vector.
From those we derive the geometric relationships between actions:

  opposite_of(A)   = action whose vector is (-dy, -dx)  → a U-turn
  right_of(A)      = action whose vector is the 90° clockwise rotation of A
                     i.e. (dy, dx) → (dx, -dy)
  left_of(A)       = action whose vector is the 90° counter-clockwise rotation
                     i.e. (dy, dx) → (-dx, dy)

In a 2D grid with axes (y down, x right):
  UP    = (-1, 0)
  DOWN  = ( 1, 0)
  LEFT  = ( 0,-1)
  RIGHT = ( 0, 1)

Right-of-UP    = RIGHT     ✓
Right-of-RIGHT = DOWN      ✓
Right-of-DOWN  = LEFT      ✓
Right-of-LEFT  = UP        ✓
"""

from __future__ import annotations


def build_direction_index(
    moves: list[tuple[int, int, int]],
) -> dict:
    """Given the learned Movable rules as ``[(action_code, dy, dx), ...]``,
    return a dict of helper maps the reasoner / planner can consult."""
    by_action: dict[int, tuple[int, int]] = {
        ac: (dy, dx) for ac, dy, dx in moves
    }

    def norm(v: tuple[int, int]) -> tuple[int, int]:
        """Normalize (dy, dx) to its sign components for direction comparison."""
        sy = 0 if v[0] == 0 else (1 if v[0] > 0 else -1)
        sx = 0 if v[1] == 0 else (1 if v[1] > 0 else -1)
        return sy, sx

    by_sign: dict[tuple[int, int], int] = {
        norm(v): ac for ac, v in by_action.items()
    }

    def opposite(action_code: int) -> int | None:
        v = by_action.get(action_code)
        if v is None:
            return None
        s = norm((-v[0], -v[1]))
        return by_sign.get(s)

    def right_of(action_code: int) -> int | None:
        v = by_action.get(action_code)
        if v is None:
            return None
        # 90° clockwise: (dy, dx) → (dx, -dy)
        s = norm((v[1], -v[0]))
        return by_sign.get(s)

    def left_of(action_code: int) -> int | None:
        v = by_action.get(action_code)
        if v is None:
            return None
        # 90° counter-clockwise: (dy, dx) → (-dx, dy)
        s = norm((-v[1], v[0]))
        return by_sign.get(s)

    return {
        "by_action": by_action,
        "opposite": opposite,
        "right_of": right_of,
        "left_of": left_of,
    }
