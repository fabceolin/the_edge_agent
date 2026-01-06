"""
Output Schema Models for Response Transformation.

TEA-BUILTIN-015.5: Defines Pydantic models for output_schema section
that maps internal state to API responses using Jinja2 templates.

Example YAML:
    output_schema:
      success: true
      session_id: "{{ state.session_id }}"
      answer:
        value: "{{ state.result }}"
        default: "No answer"
      conditional_field:
        value: "{{ state.data }}"
        include_if: "state.include_data"
"""

from typing import Any, Dict, List, Optional, Union
from pydantic import BaseModel, ConfigDict, Field


class OutputSchemaField(BaseModel):
    """
    Single field in output schema with optional conditional inclusion.

    Attributes:
        value: Jinja2 template string or static value for the field.
        include_if: Optional condition expression. If evaluates to false,
                   the field is omitted from the output.
        default: Default value to use when template evaluates to None/undefined.

    Example:
        # Simple value with default
        OutputSchemaField(value="{{ state.answer }}", default="N/A")

        # Conditional field
        OutputSchemaField(
            value="{{ state.error_msg }}",
            include_if="state.has_error"
        )
    """

    model_config = ConfigDict(extra="forbid")

    value: Any = Field(..., description="Jinja2 template or static value")
    include_if: Optional[str] = Field(
        None, description="Condition expression for inclusion (Jinja2)"
    )
    default: Optional[Any] = Field(
        None, description="Default value when template evaluates to None"
    )


class OutputSchema(BaseModel):
    """
    Output schema definition container.

    Parses the output_schema section from YAML and provides a structured
    representation of fields with their transformation rules.

    Attributes:
        fields: Dictionary mapping field names to their configurations.
                Values can be OutputSchemaField for complex fields,
                or simple values (str, int, bool, dict, list) for static
                or simple template expressions.

    Example:
        schema = OutputSchema.from_yaml({
            "success": True,
            "session_id": "{{ state.session_id }}",
            "result": {
                "value": "{{ state.data }}",
                "default": "No data"
            }
        })
    """

    model_config = ConfigDict(extra="forbid")

    fields: Dict[str, Union[OutputSchemaField, Any]] = Field(
        default_factory=dict, description="Field name to configuration mapping"
    )

    @classmethod
    def from_yaml(cls, yaml_dict: Dict[str, Any]) -> "OutputSchema":
        """
        Parse output_schema from YAML dictionary.

        Handles both simple values and complex field configurations:
        - Simple: `field: "{{ state.value }}"` or `field: true`
        - Complex: `field: {value: "...", include_if: "...", default: ...}`

        Args:
            yaml_dict: Dictionary from YAML output_schema section.

        Returns:
            OutputSchema instance with parsed fields.

        Example:
            >>> schema = OutputSchema.from_yaml({
            ...     "success": True,
            ...     "data": {"value": "{{ state.x }}", "default": 0}
            ... })
            >>> isinstance(schema.fields["success"], bool)
            True
            >>> isinstance(schema.fields["data"], OutputSchemaField)
            True
        """
        if not yaml_dict:
            return cls(fields={})

        fields: Dict[str, Union[OutputSchemaField, Any]] = {}

        for name, config in yaml_dict.items():
            if isinstance(config, dict):
                # Check if this is a complex field config (has value, include_if, or default)
                is_complex = any(
                    key in config for key in ("value", "include_if", "default")
                )
                if is_complex:
                    # Complex field with OutputSchemaField structure
                    fields[name] = OutputSchemaField(**config)
                else:
                    # Nested dict - treat as static nested object or template
                    fields[name] = config
            else:
                # Simple value: static (bool, int, str) or template string
                fields[name] = config

        return cls(fields=fields)

    def get_field_names(self) -> List[str]:
        """Get list of all field names in the schema."""
        return list(self.fields.keys())

    def is_conditional(self, field_name: str) -> bool:
        """Check if a field has conditional inclusion."""
        field = self.fields.get(field_name)
        if isinstance(field, OutputSchemaField):
            return field.include_if is not None
        return False

    def has_default(self, field_name: str) -> bool:
        """Check if a field has a default value."""
        field = self.fields.get(field_name)
        if isinstance(field, OutputSchemaField):
            return field.default is not None
        return False
