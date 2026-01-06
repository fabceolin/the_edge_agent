"""
Response Transformation Module.

TEA-BUILTIN-015.5: Provides output schema definition and transformation
for mapping internal agent state to structured API responses.

Example:
    >>> from the_edge_agent.transformation import OutputSchema, transform_output
    >>> from jinja2 import Environment, BaseLoader
    >>>
    >>> schema = OutputSchema.from_yaml({
    ...     "success": True,
    ...     "data": "{{ state.result }}"
    ... })
    >>> env = Environment(loader=BaseLoader())
    >>> output = transform_output({"result": "Hello"}, schema, env)
    >>> print(output)
    {'success': True, 'data': 'Hello'}
"""

from .schema import OutputSchema, OutputSchemaField
from .transformer import transform_output

__all__ = [
    "OutputSchema",
    "OutputSchemaField",
    "transform_output",
]
