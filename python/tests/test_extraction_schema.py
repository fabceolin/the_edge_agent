"""
Unit tests for extraction schema validation (TEA-YAML-004 AC: 1-5, 20).

Test coverage:
- ExtractionSchema dataclass parsing (AC: 1)
- Entity required/optional field validation (AC: 2, 3)
- Relationship type and field validation (AC: 4)
- Error messages with field paths (AC: 5)
- Schema parse errors at load time (AC: 20)
"""

import pytest
from the_edge_agent.extraction_validation import (
    ExtractionSchema,
    EntitySpec,
    RelationshipSpec,
    StructuralValidator,
    ValidationResult,
)


class TestExtractionSchemaDataclass:
    """Tests for ExtractionSchema.from_dict() parsing."""

    def test_parse_minimal_schema(self):
        """Parse schema with no fields specified."""
        data = {}
        schema = ExtractionSchema.from_dict(data)

        assert schema.entities.required_fields == []
        assert schema.entities.optional_fields == []
        assert schema.relationships.types == []
        assert schema.relationships.required_fields == []
        assert schema.guide_extraction is False
        assert schema.confidence_tracking is False

    def test_parse_entity_fields(self):
        """Parse entity required and optional fields."""
        data = {
            "entities": {
                "required_fields": ["name", "type"],
                "optional_fields": ["birth_date", "death_date"],
            }
        }
        schema = ExtractionSchema.from_dict(data)

        assert schema.entities.required_fields == ["name", "type"]
        assert schema.entities.optional_fields == ["birth_date", "death_date"]

    def test_parse_relationship_types(self):
        """Parse allowed relationship types."""
        data = {
            "relationships": {
                "types": ["mother", "father", "affair"],
                "required_fields": ["type", "subject", "object"],
            }
        }
        schema = ExtractionSchema.from_dict(data)

        assert schema.relationships.types == ["mother", "father", "affair"]
        assert schema.relationships.required_fields == ["type", "subject", "object"]

    def test_parse_type_specific_requirements(self):
        """Parse type-specific required fields."""
        data = {
            "relationships": {
                "types": ["affair", "mother"],
                "required_fields": ["type", "subject", "object"],
                "type_requirements": {
                    "affair": ["start_date", "end_date"],
                },
            }
        }
        schema = ExtractionSchema.from_dict(data)

        assert schema.relationships.type_requirements == {
            "affair": ["start_date", "end_date"]
        }

    def test_parse_guide_extraction_flag(self):
        """Parse guide_extraction flag for schema-guided prompts."""
        data = {"guide_extraction": True}
        schema = ExtractionSchema.from_dict(data)

        assert schema.guide_extraction is True

    def test_parse_confidence_tracking_flag(self):
        """Parse confidence_tracking flag for probabilistic reasoning."""
        data = {"confidence_tracking": True}
        schema = ExtractionSchema.from_dict(data)

        assert schema.confidence_tracking is True

    def test_parse_full_schema(self):
        """Parse complete schema with all options."""
        data = {
            "entities": {
                "required_fields": ["name"],
                "optional_fields": ["birth_date"],
                "type_field": "entity_type",
            },
            "relationships": {
                "types": ["mother", "father", "affair"],
                "required_fields": ["type", "subject", "object"],
                "optional_fields": ["confidence"],
                "type_requirements": {
                    "affair": ["start_date", "end_date"],
                },
            },
            "guide_extraction": True,
            "confidence_tracking": True,
        }
        schema = ExtractionSchema.from_dict(data)

        assert schema.entities.required_fields == ["name"]
        assert schema.entities.type_field == "entity_type"
        assert schema.relationships.types == ["mother", "father", "affair"]
        assert schema.guide_extraction is True
        assert schema.confidence_tracking is True


class TestStructuralValidatorEntities:
    """Tests for entity structural validation (AC: 2-3)."""

    def test_valid_entity_passes(self):
        """Entity with all required fields passes validation."""
        schema = ExtractionSchema(entities=EntitySpec(required_fields=["name", "type"]))
        validator = StructuralValidator(schema)

        entities = [{"name": "Alice", "type": "person"}]
        result = validator.validate(entities=entities)

        assert result.valid is True
        assert result.errors == []

    def test_missing_required_field_fails(self):
        """Entity missing required field fails with field path."""
        schema = ExtractionSchema(entities=EntitySpec(required_fields=["name", "type"]))
        validator = StructuralValidator(schema)

        entities = [{"name": "Alice"}]  # Missing "type"
        result = validator.validate(entities=entities)

        assert result.valid is False
        assert len(result.errors) == 1
        error = result.errors[0]
        assert error["type"] == "missing_required_field"
        assert error["path"] == "entities[0].type"
        assert error["expected"] == "type"

    def test_none_value_treated_as_missing(self):
        """Entity with None value for required field fails."""
        schema = ExtractionSchema(entities=EntitySpec(required_fields=["name", "type"]))
        validator = StructuralValidator(schema)

        entities = [{"name": "Alice", "type": None}]
        result = validator.validate(entities=entities)

        assert result.valid is False
        assert result.errors[0]["path"] == "entities[0].type"

    def test_multiple_entities_validated(self):
        """All entities are validated, not just first."""
        schema = ExtractionSchema(entities=EntitySpec(required_fields=["name"]))
        validator = StructuralValidator(schema)

        entities = [
            {"name": "Alice"},
            {},  # Missing name
            {"name": "Bob"},
        ]
        result = validator.validate(entities=entities)

        assert result.valid is False
        assert len(result.errors) == 1
        assert result.errors[0]["path"] == "entities[1].name"

    def test_optional_fields_not_required(self):
        """Entity without optional fields passes validation."""
        schema = ExtractionSchema(
            entities=EntitySpec(
                required_fields=["name"],
                optional_fields=["birth_date", "death_date"],
            )
        )
        validator = StructuralValidator(schema)

        entities = [{"name": "Alice"}]  # No optional fields
        result = validator.validate(entities=entities)

        assert result.valid is True

    def test_empty_entities_list_passes(self):
        """Empty entities list passes validation."""
        schema = ExtractionSchema(entities=EntitySpec(required_fields=["name"]))
        validator = StructuralValidator(schema)

        result = validator.validate(entities=[])

        assert result.valid is True


class TestStructuralValidatorRelationships:
    """Tests for relationship structural validation (AC: 4)."""

    def test_valid_relationship_passes(self):
        """Relationship with all required fields passes."""
        schema = ExtractionSchema(
            relationships=RelationshipSpec(
                types=["mother", "father"],
                required_fields=["type", "subject", "object"],
            )
        )
        validator = StructuralValidator(schema)

        relationships = [{"type": "mother", "subject": "Alice", "object": "Bob"}]
        result = validator.validate(relationships=relationships)

        assert result.valid is True

    def test_missing_relationship_field_fails(self):
        """Relationship missing required field fails."""
        schema = ExtractionSchema(
            relationships=RelationshipSpec(
                required_fields=["type", "subject", "object"]
            )
        )
        validator = StructuralValidator(schema)

        relationships = [{"type": "mother", "subject": "Alice"}]  # Missing object
        result = validator.validate(relationships=relationships)

        assert result.valid is False
        assert result.errors[0]["path"] == "relationships[0].object"

    def test_invalid_relationship_type_fails(self):
        """Relationship with unknown type fails."""
        schema = ExtractionSchema(
            relationships=RelationshipSpec(
                types=["mother", "father"],
                required_fields=["type", "subject", "object"],
            )
        )
        validator = StructuralValidator(schema)

        relationships = [{"type": "sibling", "subject": "Alice", "object": "Bob"}]
        result = validator.validate(relationships=relationships)

        assert result.valid is False
        error = result.errors[0]
        assert error["type"] == "invalid_relationship_type"
        assert error["actual"] == "sibling"
        assert error["expected"] == ["mother", "father"]

    def test_no_type_restriction_allows_any(self):
        """When types list is empty, any type is allowed."""
        schema = ExtractionSchema(
            relationships=RelationshipSpec(
                types=[],  # No type restriction
                required_fields=["type", "subject", "object"],
            )
        )
        validator = StructuralValidator(schema)

        relationships = [{"type": "any_type", "subject": "Alice", "object": "Bob"}]
        result = validator.validate(relationships=relationships)

        assert result.valid is True

    def test_type_specific_requirements(self):
        """Type-specific required fields are enforced."""
        schema = ExtractionSchema(
            relationships=RelationshipSpec(
                types=["affair", "mother"],
                required_fields=["type", "subject", "object"],
                type_requirements={"affair": ["start_date", "end_date"]},
            )
        )
        validator = StructuralValidator(schema)

        # Affair without dates should fail
        relationships = [{"type": "affair", "subject": "Alice", "object": "Bob"}]
        result = validator.validate(relationships=relationships)

        assert result.valid is False
        errors = result.errors
        assert len(errors) == 2  # Missing start_date and end_date
        assert any("start_date" in e["path"] for e in errors)
        assert any("end_date" in e["path"] for e in errors)

    def test_type_specific_requirements_other_types_ok(self):
        """Type-specific requirements don't apply to other types."""
        schema = ExtractionSchema(
            relationships=RelationshipSpec(
                types=["affair", "mother"],
                required_fields=["type", "subject", "object"],
                type_requirements={"affair": ["start_date", "end_date"]},
            )
        )
        validator = StructuralValidator(schema)

        # Mother relationship doesn't need dates
        relationships = [{"type": "mother", "subject": "Alice", "object": "Bob"}]
        result = validator.validate(relationships=relationships)

        assert result.valid is True


class TestValidationResult:
    """Tests for ValidationResult structure."""

    def test_result_has_timestamp(self):
        """ValidationResult includes validated_at timestamp."""
        result = ValidationResult(valid=True)

        assert result.validated_at is not None
        assert "T" in result.validated_at  # ISO format

    def test_result_to_dict(self):
        """ValidationResult.to_dict() returns expected structure."""
        result = ValidationResult(
            valid=False,
            errors=[{"type": "test", "message": "test error"}],
        )

        d = result.to_dict()

        assert d["valid"] is False
        assert len(d["errors"]) == 1
        assert "validated_at" in d


class TestCombinedValidation:
    """Tests for combined entity and relationship validation."""

    def test_entities_and_relationships_both_validated(self):
        """Both entities and relationships are validated together."""
        schema = ExtractionSchema(
            entities=EntitySpec(required_fields=["name"]),
            relationships=RelationshipSpec(
                required_fields=["type", "subject", "object"]
            ),
        )
        validator = StructuralValidator(schema)

        entities = [{"name": "Alice"}]
        relationships = [{"type": "mother"}]  # Missing subject, object

        result = validator.validate(entities=entities, relationships=relationships)

        assert result.valid is False
        # Should have 2 errors: missing subject and object
        assert len(result.errors) == 2

    def test_no_schema_means_no_validation(self):
        """Empty schema passes everything."""
        schema = ExtractionSchema()
        validator = StructuralValidator(schema)

        entities = [{"any": "field"}]
        relationships = [{"any": "field"}]

        result = validator.validate(entities=entities, relationships=relationships)

        assert result.valid is True


class TestSchemaLoadTimeValidation:
    """Tests for schema validation at load time (AC: 20)."""

    def test_yaml_engine_validates_schema_at_load(self):
        """YAMLEngine validates extraction_schema at load time."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        # Valid schema should work
        config = {
            "extraction_schema": {
                "entities": {"required_fields": ["name"]},
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }
        graph = engine.load_from_dict(config)
        assert graph is not None

    def test_yaml_engine_rejects_invalid_constraints_language(self):
        """YAMLEngine rejects invalid validation_constraints language."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        config = {
            "validation_constraints": {
                "language": "javascript",  # Invalid
                "rules": "// some code",
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        with pytest.raises(ValueError, match="language must be 'prolog'"):
            engine.load_from_dict(config)

    def test_yaml_engine_rejects_invalid_probe_config(self):
        """YAMLEngine rejects semantic_probes without required fields."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        config = {
            "semantic_probes": [{"probe": "Is this valid?"}],  # Missing for_each
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        with pytest.raises(ValueError, match="missing required 'for_each' field"):
            engine.load_from_dict(config)

    def test_yaml_engine_sets_extraction_prompt_variable(self):
        """YAMLEngine sets extraction_prompt variable when guide_extraction is true."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        config = {
            "extraction_schema": {
                "entities": {"required_fields": ["name"]},
                "relationships": {"types": ["mother", "father"]},
                "guide_extraction": True,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        assert "extraction_prompt" in engine.variables
        prompt = engine.variables["extraction_prompt"]
        assert "ONTOLOGY" in prompt
        assert "name (required)" in prompt
        assert "mother" in prompt or "father" in prompt
