"""
Reasoning module for The Edge Agent.

This module provides integrations with external reasoning frameworks:

- dspy_client: DSPy integration for declarative prompt optimization
"""

from .dspy_client import DSPyClient, DSPyConfig

__all__ = ["DSPyClient", "DSPyConfig"]
