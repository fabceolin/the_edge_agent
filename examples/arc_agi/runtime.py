"""Process-level runtime holding non-serializable resources (env, worldview).

State dicts that flow through the TEA StateGraph stay JSON-friendly; this module
holds the heavy objects (arc_agi env, KuzuDB connection) that the graph nodes
reach into.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import arc_agi
from arc_agi import Arcade, OperationMode
from arcengine import GameAction

from . import GAME_ID
from .worldview import Worldview


@dataclass
class Runtime:
    arcade: Arcade
    env: Any  # LocalEnvironmentWrapper, not type-exported
    worldview: Worldview
    episode: int


_runtime: Runtime | None = None


def init_runtime(
    *,
    db_path: str = ":memory:",
    operation_mode: OperationMode = OperationMode.NORMAL,
    game_id: str = GAME_ID,
    episode: int = 0,
) -> Runtime:
    global _runtime
    arcade = Arcade(operation_mode=operation_mode)
    env = arcade.make(game_id)
    if env is None:
        raise RuntimeError(f"Failed to make env for game_id={game_id}")
    _runtime = Runtime(
        arcade=arcade,
        env=env,
        worldview=Worldview(db_path),
        episode=episode,
    )
    return _runtime


def get_runtime() -> Runtime:
    if _runtime is None:
        raise RuntimeError("Runtime not initialized; call init_runtime() first")
    return _runtime


def shutdown() -> None:
    global _runtime
    if _runtime is not None:
        _runtime.worldview.close()
        _runtime = None


__all__ = ["Runtime", "init_runtime", "get_runtime", "shutdown", "GameAction"]
