"""
Input Validation Module for TEA YAML Agents (TEA-BUILTIN-015.4).

Provides declarative input validation schema for request validation:
- Type validation (str, int, float, bool, list, dict)
- Required/default field handling
- Constraint validation (min/max, length, pattern, choices)
- Nested object validation with path reporting
- Aggregated error handling

Usage in YAML:
    input_schema:
      query:
        type: str
        required: true
        min_length: 1
      max_results:
        type: int
        default: 5
        min: 1
        max: 100

Example:
    >>> from the_edge_agent.validation import InputSchema, validate_input
    >>> schema = InputSchema.from_yaml({
    ...     "query": {"type": "str", "required": True},
    ...     "limit": {"type": "int", "default": 10}
    ... })
    >>> validated = validate_input({"query": "hello"}, schema)
    >>> print(validated)  # {"query": "hello", "limit": 10}
"""

from .schema import InputSchema, InputSchemaField
from .validators import validate_input
from .errors import ValidationError, ValidationErrorDetail

__all__ = [
    "InputSchema",
    "InputSchemaField",
    "validate_input",
    "ValidationError",
    "ValidationErrorDetail",
]
