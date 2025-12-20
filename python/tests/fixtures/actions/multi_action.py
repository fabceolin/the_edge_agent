"""Action module with multiple actions."""

from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register multiple actions."""

    def action_one(state, **kwargs):
        return {"result": "one", "success": True}

    def action_two(state, **kwargs):
        return {"result": "two", "success": True}

    def action_three(state, **kwargs):
        return {"result": "three", "success": True}

    registry['action_one'] = action_one
    registry['action_two'] = action_two
    registry['action_three'] = action_three
