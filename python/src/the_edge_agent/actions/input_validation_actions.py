"""
Input Validation Actions for YAML Agents (TEA-BUILTIN-015.4).

Provides the validate.input action for explicit mid-flow validation:
- Schema-based input validation
- Type coercion and constraint checking
- Structured error reporting

Usage in YAML:
    nodes:
      - name: validate_user_input
        uses: validate.input
        with:
          data: "{{ state.user_provided_data }}"
          schema:
            name:
              type: str
              required: true
            age:
              type: int
              min: 0
              max: 150
        output: validation_result
"""

from typing import Any, Callable, Dict, Optional

from ..validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register input validation actions in the YAMLEngine registry.

    Actions registered:
        - validate.input: Validate data against inline or referenced schema
        - validate.schema: Create a reusable schema validator

    Args:
        registry: Action registry to populate
        engine: YAMLEngine instance for accessing runtime and settings
    """

    def validate_input_action(
        state: Dict[str, Any],
        data: Optional[Dict[str, Any]] = None,
        schema: Optional[Dict[str, Any]] = None,
        raise_on_error: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Validate input data against a schema (AC10).

        This action allows explicit validation within an agent flow,
        useful for validating user-provided data mid-execution or
        validating data before processing.

        Args:
            state: Current workflow state
            data: Data to validate (defaults to entire state if not provided)
            schema: Schema definition dict (inline or from data section)
            raise_on_error: If True, raises ValidationError instead of returning result

        Returns:
            Dict with:
                - valid: bool - Whether validation passed
                - data: validated/coerced data (if valid)
                - errors: List of error dicts (if invalid)

        Example YAML:
            - name: validate_user_input
              uses: validate.input
              with:
                data: "{{ state.user_data }}"
                schema:
                  name:
                    type: str
                    required: true
                  email:
                    type: str
                    pattern: "^[\\w.-]+@[\\w.-]+\\.\\w+$"
              output: validation_result

            # Check result
            - name: check_validation
              condition: "{{ state.validation_result.valid }}"
        """
        # Use provided data or entire state
        if data is None:
            data = dict(state)

        # Schema is required
        if schema is None:
            return {
                "valid": False,
                "errors": [
                    {
                        "field": "__schema__",
                        "error": "missing",
                        "message": "Schema is required for validate.input action",
                    }
                ],
            }

        try:
            # Parse schema from dict
            input_schema = InputSchema.from_yaml(schema)

            # Validate the data
            validated_data = validate_input(data, input_schema)

            return {
                "valid": True,
                "data": validated_data,
            }

        except ValidationError as e:
            if raise_on_error:
                raise
            return {
                "valid": False,
                "errors": [err.to_dict() for err in e.errors],
            }
        except Exception as e:
            if raise_on_error:
                raise
            return {
                "valid": False,
                "errors": [
                    {
                        "field": "__schema__",
                        "error": "invalid_schema",
                        "message": f"Schema parsing error: {str(e)}",
                    }
                ],
            }

    def create_validator_action(
        state: Dict[str, Any],
        schema: Dict[str, Any],
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Create a reusable schema validator.

        Args:
            state: Current workflow state
            schema: Schema definition dict

        Returns:
            Dict with:
                - schema: Parsed InputSchema instance (for reuse)
        """
        try:
            input_schema = InputSchema.from_yaml(schema)
            return {"schema": input_schema, "valid": True}
        except Exception as e:
            return {
                "schema": None,
                "valid": False,
                "error": str(e),
            }

    # Register actions
    registry["validate.input"] = validate_input_action
    registry["actions.validate_input"] = validate_input_action
    registry["validate.schema"] = create_validator_action
    registry["actions.validate_schema"] = create_validator_action
