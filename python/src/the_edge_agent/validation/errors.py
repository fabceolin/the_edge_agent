"""
Validation Error Types for Input Schema Validation (TEA-BUILTIN-015.4).

Provides structured error handling for validation failures:
- ValidationErrorDetail: Single validation error with field path
- ValidationError: Exception containing all validation errors
"""

from typing import List, Any, Optional


class ValidationErrorDetail:
    """
    Details about a single validation error.

    Attributes:
        field: Dot-separated path to the field (e.g., "options.temperature")
        error: Error type (required, type, min_length, max_length, min, max, pattern, choices)
        message: Human-readable error message
        value: The actual value that failed validation (optional)
        constraint: The constraint value that was violated (optional)
        expected: Expected type/value description (optional)
    """

    def __init__(
        self,
        field: str,
        error: str,
        message: str,
        value: Optional[Any] = None,
        constraint: Optional[Any] = None,
        expected: Optional[str] = None,
    ):
        self.field = field
        self.error = error
        self.message = message
        self.value = value
        self.constraint = constraint
        self.expected = expected

    def to_dict(self) -> dict:
        """Convert to dictionary representation."""
        result = {
            "field": self.field,
            "error": self.error,
            "message": self.message,
        }
        if self.value is not None:
            result["value"] = self.value
        if self.constraint is not None:
            result["constraint"] = self.constraint
        if self.expected is not None:
            result["expected"] = self.expected
        return result

    def __repr__(self) -> str:
        return f"ValidationErrorDetail(field={self.field!r}, error={self.error!r}, message={self.message!r})"


class ValidationError(Exception):
    """
    Exception raised when input validation fails.

    Contains all validation errors collected during validation.
    Errors are aggregated (doesn't stop at first error).

    Attributes:
        errors: List of ValidationErrorDetail instances
    """

    def __init__(self, errors: List[ValidationErrorDetail]):
        self.errors = errors
        super().__init__(f"Validation failed: {len(errors)} error(s)")

    def to_dict(self) -> dict:
        """
        Convert to HTTP 422 response format.

        Returns:
            Dict with:
                - success: False
                - errors: List of error details
        """
        return {
            "success": False,
            "errors": [e.to_dict() for e in self.errors],
        }

    def __repr__(self) -> str:
        return f"ValidationError({len(self.errors)} errors)"
