"""Learning and adaptation modules for The Edge Agent.

This package provides interfaces for prompt optimization and adaptive learning.
"""

from .textgrad_client import TextGradClient, TextGradConfig

__all__ = ["TextGradClient", "TextGradConfig"]
