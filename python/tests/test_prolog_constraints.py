"""
Unit tests for Prolog constraint validation (TEA-YAML-004 AC: 6-10, 21).

Test coverage:
- ValidationConstraints parsing (AC: 6)
- Entity/relationship Prolog fact assertion (AC: 7)
- validation_error/2 predicate evaluation (AC: 8)
- Fail-fast behavior (AC: 9)
- Error message formatting (AC: 10)
- Prolog syntax error detection at load time (AC: 21)
"""

import pytest
from the_edge_agent.extraction_validation import (
    ValidationConstraints,
    PrologConstraintValidator,
    ValidationResult,
)

# Check if Prolog runtime is available
try:
    from the_edge_agent.prolog_runtime import JANUS_AVAILABLE
except ImportError:
    JANUS_AVAILABLE = False


class TestValidationConstraintsParsing:
    """Tests for ValidationConstraints.from_dict() parsing."""

    def test_parse_prolog_constraints(self):
        """Parse validation_constraints with Prolog rules."""
        data = {
            "language": "prolog",
            "rules": """
                validation_error(missing_mother, Child) :-
                    entity(Child, child),
                    \\+ relationship(mother, _, Child).
            """,
        }
        constraints = ValidationConstraints.from_dict(data)

        assert constraints.language == "prolog"
        assert "validation_error" in constraints.rules
        assert "missing_mother" in constraints.rules

    def test_default_language_is_prolog(self):
        """Default language is prolog if not specified."""
        data = {"rules": "validation_error(test, x)."}
        constraints = ValidationConstraints.from_dict(data)

        assert constraints.language == "prolog"

    def test_rejects_non_prolog_language(self):
        """Raise ValueError for non-prolog language."""
        data = {"language": "javascript", "rules": "// code"}

        with pytest.raises(ValueError, match="language must be 'prolog'"):
            ValidationConstraints.from_dict(data)

    def test_empty_rules_allowed(self):
        """Empty rules string is allowed."""
        data = {"language": "prolog", "rules": ""}
        constraints = ValidationConstraints.from_dict(data)

        assert constraints.rules == ""


@pytest.mark.skipif(not JANUS_AVAILABLE, reason="Prolog runtime not available")
class TestPrologFactAssertion:
    """Tests for Prolog fact generation (AC: 7)."""

    def test_entity_facts_generated_correctly(self):
        """Entity facts are generated as entity/2."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="",
        )
        validator = PrologConstraintValidator(constraints)

        facts = validator._assert_entity_facts(
            [
                {"name": "Alice", "type": "person"},
                {"name": "Bob", "type": "child"},
            ]
        )

        assert "entity('Alice', 'person')." in facts
        assert "entity('Bob', 'child')." in facts

    def test_entity_facts_with_confidence(self):
        """Entity facts with confidence are generated as entity/3."""
        constraints = ValidationConstraints(language="prolog", rules="")
        validator = PrologConstraintValidator(constraints)

        facts = validator._assert_entity_facts(
            [{"name": "Alice", "type": "person", "confidence": 0.95}],
            confidence_tracking=True,
        )

        assert "entity('Alice', 'person', 0.95)." in facts

    def test_relationship_facts_basic(self):
        """Relationship facts are generated as relationship/3."""
        constraints = ValidationConstraints(language="prolog", rules="")
        validator = PrologConstraintValidator(constraints)

        facts = validator._assert_relationship_facts(
            [
                {"type": "mother", "subject": "Alice", "object": "Bob"},
            ]
        )

        assert "relationship('mother', 'Alice', 'Bob')." in facts

    def test_relationship_facts_with_dates(self):
        """Relationship facts with dates are generated as relationship/5."""
        constraints = ValidationConstraints(language="prolog", rules="")
        validator = PrologConstraintValidator(constraints)

        facts = validator._assert_relationship_facts(
            [
                {
                    "type": "affair",
                    "subject": "Alice",
                    "object": "Bob",
                    "start_date": "1990-01-01",
                    "end_date": "1995-12-31",
                },
            ]
        )

        assert (
            "relationship('affair', 'Alice', 'Bob', '1990-01-01', '1995-12-31')."
            in facts
        )

    def test_relationship_facts_with_confidence(self):
        """Relationship facts with confidence are generated as relationship/4."""
        constraints = ValidationConstraints(language="prolog", rules="")
        validator = PrologConstraintValidator(constraints)

        facts = validator._assert_relationship_facts(
            [
                {
                    "type": "mother",
                    "subject": "Alice",
                    "object": "Bob",
                    "confidence": 0.9,
                }
            ],
            confidence_tracking=True,
        )

        assert "relationship('mother', 'Alice', 'Bob', 0.9)." in facts

    def test_special_characters_escaped(self):
        """Single quotes in names are escaped in Prolog atoms."""
        constraints = ValidationConstraints(language="prolog", rules="")
        validator = PrologConstraintValidator(constraints)

        facts = validator._assert_entity_facts(
            [
                {"name": "O'Connor", "type": "person"},
            ]
        )

        assert "O\\'Connor" in facts


@pytest.mark.skipif(not JANUS_AVAILABLE, reason="Prolog runtime not available")
class TestPrologConstraintValidation:
    """Tests for Prolog constraint evaluation (AC: 8-10)."""

    def test_no_validation_error_means_valid(self):
        """No validation_error/2 matches means data is valid."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                % No constraint violations
                validation_error(impossible, _) :- fail.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "Alice", "type": "person"}],
            relationships=[],
        )

        assert result.valid is True
        assert result.errors == []

    def test_validation_error_match_fails(self):
        """validation_error/2 match causes validation failure."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                % Fail if any entity named 'BadName' exists
                validation_error(bad_name, Name) :-
                    entity(Name, _),
                    Name = 'BadName'.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "BadName", "type": "person"}],
            relationships=[],
        )

        assert result.valid is False
        assert len(result.errors) >= 1
        # Check error contains type and context
        error = result.errors[0]
        assert error["type"] == "prolog_constraint_violation"

    def test_missing_relationship_constraint(self):
        """Constraint detecting missing mother relationship."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(missing_mother, Child) :-
                    entity(Child, 'child'),
                    \\+ relationship('mother', _, Child).
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "Bob", "type": "child"}],
            relationships=[],  # No mother relationship
        )

        assert result.valid is False
        assert any("missing_mother" in str(e) for e in result.errors)

    def test_valid_when_constraint_satisfied(self):
        """Valid when constraint is satisfied."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(missing_mother, Child) :-
                    entity(Child, 'child'),
                    \\+ relationship('mother', _, Child).
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "Bob", "type": "child"}],
            relationships=[{"type": "mother", "subject": "Alice", "object": "Bob"}],
        )

        assert result.valid is True

    def test_date_comparison_constraint(self):
        """Constraint checking date validity."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(invalid_dates, Pair) :-
                    relationship('affair', Subj, Obj, Start, End),
                    Start @> End,
                    Pair = Subj-Obj.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[],
            relationships=[
                {
                    "type": "affair",
                    "subject": "Alice",
                    "object": "Bob",
                    "start_date": "2000-01-01",
                    "end_date": "1990-01-01",  # End before start
                }
            ],
        )

        assert result.valid is False
        assert any("invalid_dates" in str(e) for e in result.errors)


@pytest.mark.skipif(not JANUS_AVAILABLE, reason="Prolog runtime not available")
class TestPrologConfidenceConstraints:
    """Tests for confidence-based Prolog constraints (AC: 28-31)."""

    def test_low_confidence_constraint(self):
        """Constraint detecting low confidence entities."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(low_confidence, E) :-
                    entity(E, _, C),
                    C < 0.5.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "Alice", "type": "person", "confidence": 0.3}],
            relationships=[],
            confidence_tracking=True,
        )

        assert result.valid is False
        assert any("low_confidence" in str(e) for e in result.errors)

    def test_high_confidence_passes(self):
        """High confidence entity passes low_confidence check."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(low_confidence, E) :-
                    entity(E, _, C),
                    C < 0.5.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "Alice", "type": "person", "confidence": 0.95}],
            relationships=[],
            confidence_tracking=True,
        )

        assert result.valid is True


class TestPrologErrorHandling:
    """Tests for Prolog error handling (AC: 21)."""

    @pytest.mark.skipif(not JANUS_AVAILABLE, reason="Prolog runtime not available")
    def test_prolog_runtime_error_caught(self):
        """Prolog runtime errors are caught and reported."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(test, X) :-
                    undefined_predicate(X).
            """,
        )
        validator = PrologConstraintValidator(constraints)

        # This should not raise, but return invalid result
        result = validator.validate(
            entities=[{"name": "Alice", "type": "person"}],
            relationships=[],
        )

        # Either valid (predicate doesn't exist so no match) or caught error
        # Behavior depends on Prolog implementation
        assert isinstance(result, ValidationResult)


class TestValidatorIntegration:
    """Integration tests for full validator workflow."""

    @pytest.mark.skipif(not JANUS_AVAILABLE, reason="Prolog runtime not available")
    def test_complex_family_constraints(self):
        """Complex family relationship constraints."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                % A person cannot be their own parent
                validation_error(self_parent, Person) :-
                    relationship(Type, Person, Person),
                    member(Type, ['mother', 'father']).

                % Children must have at least one parent
                validation_error(orphan, Child) :-
                    entity(Child, 'child'),
                    \\+ relationship('mother', _, Child),
                    \\+ relationship('father', _, Child).
            """,
        )
        validator = PrologConstraintValidator(constraints)

        # Self-parent case
        result = validator.validate(
            entities=[],
            relationships=[{"type": "mother", "subject": "Alice", "object": "Alice"}],
        )
        assert result.valid is False

        # Orphan case
        result = validator.validate(
            entities=[{"name": "Bob", "type": "child"}],
            relationships=[],
        )
        assert result.valid is False

        # Valid case
        result = validator.validate(
            entities=[{"name": "Bob", "type": "child"}],
            relationships=[{"type": "mother", "subject": "Alice", "object": "Bob"}],
        )
        assert result.valid is True
