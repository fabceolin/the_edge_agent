"""Valid action module with proper register_actions function."""

from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""

    def transform(state, data=None, **kwargs):
        """Transform input data."""
        if data is None:
            data = state.get('data', '')
        return {"transformed": f"[{data}]", "success": True}

    def echo(state, message=None, **kwargs):
        """Echo a message."""
        return {"echoed": message or "default", "success": True}

    registry['transform'] = transform
    registry['echo'] = echo
