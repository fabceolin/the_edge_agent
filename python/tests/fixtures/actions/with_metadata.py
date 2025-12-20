"""Action module with __tea_actions__ metadata."""

from typing import Any, Callable, Dict


# Optional metadata for discovery and validation
__tea_actions__ = {
    "version": "1.2.3",
    "description": "Custom actions with metadata for testing",
    "actions": ["greet", "farewell"],
}


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""

    def greet(state, name=None, **kwargs):
        """Greet someone."""
        return {"greeting": f"Hello, {name or 'World'}!", "success": True}

    def farewell(state, name=None, **kwargs):
        """Say goodbye."""
        return {"farewell": f"Goodbye, {name or 'World'}!", "success": True}

    registry['greet'] = greet
    registry['farewell'] = farewell
