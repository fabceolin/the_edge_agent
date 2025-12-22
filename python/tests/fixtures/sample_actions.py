"""
Sample actions module for testing CLI actions loading.

This module follows the TEA actions module contract.
"""
from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register sample test actions."""

    def test_action(state, **kwargs):
        """Simple test action."""
        return {"test": "success"}

    def another_action(state, value, **kwargs):
        """Action with required parameter."""
        return {"result": value * 2}

    def greet_action(state, name="World", **kwargs):
        """Action with optional parameter."""
        return {"greeting": f"Hello, {name}!"}

    registry['test_action'] = test_action
    registry['another_action'] = another_action
    registry['greet_action'] = greet_action


__tea_actions__ = {
    "version": "1.0.0",
    "description": "Sample test actions",
    "actions": ["test_action", "another_action", "greet_action"],
}
