"""
Input Schema Models for YAML Agent Validation (TEA-BUILTIN-015.4).

Defines the schema structure for declarative input validation:
- InputSchemaField: Field definition with type and constraints
- InputSchema: Container for field schemas

Example YAML:
    input_schema:
      query:
        type: str
        required: true
        min_length: 1
        max_length: 1000
      options:
        type: dict
        properties:
          temperature:
            type: float
            default: 0.7
            min: 0.0
            max: 2.0
"""

from typing import Optional, List, Dict, Any, Union


class InputSchemaField:
    """
    Schema definition for a single input field.

    Supports types: str, int, float, bool, list, dict

    Attributes:
        type: Field type (str, int, float, bool, list, dict)
        required: Whether field is required (error if missing)
        default: Default value when not provided
        min_length: Minimum string length (str only)
        max_length: Maximum string length (str only)
        pattern: Regex pattern for validation (str only)
        min: Minimum value (int/float only)
        max: Maximum value (int/float only)
        choices: List of allowed values (enum validation)
        properties: Nested field schemas (dict only)
        items: Schema for list elements (list only)
    """

    VALID_TYPES = {"str", "int", "float", "bool", "list", "dict"}

    def __init__(
        self,
        type: str,
        required: bool = False,
        default: Any = None,
        min_length: Optional[int] = None,
        max_length: Optional[int] = None,
        pattern: Optional[str] = None,
        min: Optional[Union[int, float]] = None,
        max: Optional[Union[int, float]] = None,
        choices: Optional[List[Any]] = None,
        properties: Optional[Dict[str, Any]] = None,
        items: Optional[Any] = None,
    ):
        if type not in self.VALID_TYPES:
            raise ValueError(
                f"Invalid type '{type}'. Must be one of: {', '.join(self.VALID_TYPES)}"
            )

        self.type = type
        self.required = required
        self.default = default
        self.min_length = min_length
        self.max_length = max_length
        self.pattern = pattern
        self.min = min
        self.max = max
        self.choices = choices

        # Parse nested properties for dict type
        self.properties: Optional[Dict[str, "InputSchemaField"]] = None
        if properties is not None:
            self.properties = {}
            for name, prop_config in properties.items():
                if isinstance(prop_config, InputSchemaField):
                    self.properties[name] = prop_config
                elif isinstance(prop_config, dict):
                    self.properties[name] = InputSchemaField(**prop_config)
                else:
                    raise ValueError(
                        f"Invalid property config for '{name}': must be dict or InputSchemaField"
                    )

        # Parse items schema for list type
        self.items: Optional["InputSchemaField"] = None
        if items is not None:
            if isinstance(items, InputSchemaField):
                self.items = items
            elif isinstance(items, dict):
                self.items = InputSchemaField(**items)
            else:
                raise ValueError(
                    "Invalid items config: must be dict or InputSchemaField"
                )

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation."""
        result: Dict[str, Any] = {"type": self.type}
        if self.required:
            result["required"] = self.required
        if self.default is not None:
            result["default"] = self.default
        if self.min_length is not None:
            result["min_length"] = self.min_length
        if self.max_length is not None:
            result["max_length"] = self.max_length
        if self.pattern is not None:
            result["pattern"] = self.pattern
        if self.min is not None:
            result["min"] = self.min
        if self.max is not None:
            result["max"] = self.max
        if self.choices is not None:
            result["choices"] = self.choices
        if self.properties is not None:
            result["properties"] = {
                name: field.to_dict() for name, field in self.properties.items()
            }
        if self.items is not None:
            result["items"] = self.items.to_dict()
        return result

    def __repr__(self) -> str:
        attrs = [f"type={self.type!r}"]
        if self.required:
            attrs.append("required=True")
        if self.default is not None:
            attrs.append(f"default={self.default!r}")
        return f"InputSchemaField({', '.join(attrs)})"


class InputSchema:
    """
    Container for input field schemas.

    Attributes:
        fields: Dictionary mapping field names to InputSchemaField instances
    """

    def __init__(self, fields: Dict[str, InputSchemaField]):
        self.fields = fields

    @classmethod
    def from_yaml(cls, yaml_dict: Dict[str, Any]) -> "InputSchema":
        """
        Parse input_schema from YAML dict.

        Args:
            yaml_dict: Dictionary from YAML input_schema section

        Returns:
            InputSchema instance

        Example:
            >>> schema = InputSchema.from_yaml({
            ...     "query": {"type": "str", "required": True},
            ...     "limit": {"type": "int", "default": 10}
            ... })
        """
        fields = {}
        for name, config in yaml_dict.items():
            if isinstance(config, InputSchemaField):
                fields[name] = config
            elif isinstance(config, dict):
                fields[name] = InputSchemaField(**config)
            else:
                raise ValueError(
                    f"Invalid schema config for '{name}': must be dict or InputSchemaField"
                )
        return cls(fields=fields)

    def to_dict(self) -> Dict[str, Dict[str, Any]]:
        """Convert to dictionary representation."""
        return {name: field.to_dict() for name, field in self.fields.items()}

    def __repr__(self) -> str:
        return f"InputSchema(fields={list(self.fields.keys())})"
