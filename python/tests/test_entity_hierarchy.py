"""
Tests for EntityHierarchy (TEA-LTM-013).

Test design based on: docs/qa/assessments/TEA-LTM-013-test-design-20260117.md

Test organization:
- Unit tests: Schema, validation, ID generation, response format
- Integration tests: Database operations, closure table, queries
- E2E tests: Full hierarchy traversal, cascade delete

Runs with SQLite in-memory for speed. PostgreSQL integration tests
are marked with `@pytest.mark.integration`.
"""

import pytest
import unittest
from datetime import datetime

# Skip all tests if SQLAlchemy is not available
try:
    from the_edge_agent.memory.entity_hierarchy import (
        EntityHierarchy,
        check_hierarchy_available,
        SQLALCHEMY_AVAILABLE,
    )
except ImportError:
    SQLALCHEMY_AVAILABLE = False


@pytest.mark.skipif(not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed")
class TestEntityHierarchyUnit(unittest.TestCase):
    """Unit tests for EntityHierarchy (no database operations)."""

    # ========================================================================
    # AC-2: Programmatic Configuration
    # ========================================================================

    def test_create_with_valid_levels(self):
        """LTM-013-UNIT-004: Create EntityHierarchy with valid levels."""
        hierarchy = EntityHierarchy(
            levels=["org", "project", "user", "session"],
            url="sqlite:///:memory:",
            lazy=True,
        )
        self.assertEqual(hierarchy.levels, ["org", "project", "user", "session"])

    def test_reject_empty_levels(self):
        """LTM-013-UNIT-005: Reject empty levels list (P0)."""
        with self.assertRaises(ValueError) as ctx:
            EntityHierarchy(levels=[], url="sqlite:///:memory:", lazy=True)
        self.assertIn("empty", str(ctx.exception).lower())

    def test_reject_duplicate_levels(self):
        """LTM-013-UNIT-002: Reject duplicate levels."""
        with self.assertRaises(ValueError) as ctx:
            EntityHierarchy(
                levels=["org", "project", "org"],
                url="sqlite:///:memory:",
                lazy=True,
            )
        self.assertIn("duplicate", str(ctx.exception).lower())

    def test_build_level_index(self):
        """LTM-013-UNIT-006: Build level_index correctly."""
        hierarchy = EntityHierarchy(
            levels=["org", "project", "user", "session"],
            url="sqlite:///:memory:",
            lazy=True,
        )
        # Access internal _level_index
        self.assertEqual(hierarchy._level_index["org"], 0)
        self.assertEqual(hierarchy._level_index["project"], 1)
        self.assertEqual(hierarchy._level_index["user"], 2)
        self.assertEqual(hierarchy._level_index["session"], 3)

    # ========================================================================
    # AC-6: Entity ID Generation
    # ========================================================================

    def test_entity_id_format(self):
        """LTM-013-UNIT-012: Generate full entity ID format 'type:id'."""
        hierarchy = EntityHierarchy(
            levels=["org", "project"],
            url="sqlite:///:memory:",
            lazy=True,
        )
        full_id = hierarchy._make_entity_id("org", "acme")
        self.assertEqual(full_id, "org:acme")

    # ========================================================================
    # AC-14: Level Validation
    # ========================================================================

    def test_reject_unknown_entity_type(self):
        """LTM-013-UNIT-013: Reject unknown entity_type (P0)."""
        hierarchy = EntityHierarchy(
            levels=["org", "project"],
            url="sqlite:///:memory:",
        )
        with self.assertRaises(ValueError) as ctx:
            hierarchy.register_entity("invalid_type", "test")
        self.assertIn("Unknown entity type", str(ctx.exception))

    def test_reject_wrong_parent_level(self):
        """LTM-013-UNIT-014: Reject parent with wrong level (P0)."""
        hierarchy = EntityHierarchy(
            levels=["org", "project", "user"],
            url="sqlite:///:memory:",
        )
        # Register org first
        hierarchy.register_entity("org", "acme")

        # Try to register user with org as parent (should be project)
        with self.assertRaises(ValueError) as ctx:
            hierarchy.register_entity("user", "alice", parent=("org", "acme"))
        self.assertIn("must be 'project'", str(ctx.exception))

    def test_reject_root_with_parent(self):
        """LTM-013-UNIT-029: Reject root entity with parent (P0)."""
        hierarchy = EntityHierarchy(
            levels=["org", "project"],
            url="sqlite:///:memory:",
        )
        with self.assertRaises(ValueError) as ctx:
            hierarchy.register_entity("org", "acme", parent=("org", "other"))
        self.assertIn("cannot have a parent", str(ctx.exception))

    def test_reject_non_root_without_parent(self):
        """LTM-013-UNIT-030: Reject non-root entity without parent (P0)."""
        hierarchy = EntityHierarchy(
            levels=["org", "project"],
            url="sqlite:///:memory:",
        )
        with self.assertRaises(ValueError) as ctx:
            hierarchy.register_entity("project", "alpha")
        self.assertIn("requires a parent", str(ctx.exception))

    # ========================================================================
    # AC-8: Response Format
    # ========================================================================

    def test_get_entries_response_structure(self):
        """LTM-013-UNIT-017: Return structure has entries, total_count, has_more."""
        hierarchy = EntityHierarchy(
            levels=["org", "project"],
            url="sqlite:///:memory:",
        )
        hierarchy.register_entity("org", "acme")
        result = hierarchy.get_entries_for_entity("org", "acme")

        self.assertIn("entries", result)
        self.assertIn("total_count", result)
        self.assertIn("has_more", result)
        self.assertIsInstance(result["entries"], list)
        self.assertIsInstance(result["total_count"], int)
        self.assertIsInstance(result["has_more"], bool)


@pytest.mark.skipif(not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed")
class TestEntityHierarchyIntegration(unittest.TestCase):
    """Integration tests for EntityHierarchy (database operations)."""

    def setUp(self):
        """Create fresh hierarchy for each test."""
        self.hierarchy = EntityHierarchy(
            levels=["org", "project", "user", "session"],
            url="sqlite:///:memory:",
        )

    def tearDown(self):
        """Close hierarchy after each test."""
        if hasattr(self, "hierarchy"):
            self.hierarchy.close()

    # ========================================================================
    # AC-2: Schema Creation
    # ========================================================================

    def test_creates_tables_on_init(self):
        """LTM-013-INT-002: EntityHierarchy creates tables on init."""
        # Tables should exist after init
        session = self.hierarchy._get_session()
        try:
            # Query entities table
            entities = session.query(self.hierarchy._ltm_entity).all()
            self.assertEqual(len(entities), 0)
        finally:
            session.close()

    # ========================================================================
    # AC-3: Entity Registry Table
    # ========================================================================

    def test_insert_entity_with_metadata(self):
        """LTM-013-INT-003: Insert entity with metadata to database (P0)."""
        metadata = {"display_name": "ACME Corp", "tier": "enterprise"}
        full_id = self.hierarchy.register_entity("org", "acme", metadata=metadata)

        entity = self.hierarchy.get_entity("org", "acme")
        self.assertIsNotNone(entity)
        self.assertEqual(entity["id"], "org:acme")
        self.assertEqual(entity["metadata"]["display_name"], "ACME Corp")
        self.assertEqual(entity["metadata"]["tier"], "enterprise")

    def test_auto_populate_timestamps(self):
        """LTM-013-INT-004: Auto-populate created_at and updated_at."""
        self.hierarchy.register_entity("org", "acme")
        entity = self.hierarchy.get_entity("org", "acme")

        self.assertIsNotNone(entity["created_at"])
        self.assertIsNotNone(entity["updated_at"])

    # ========================================================================
    # AC-6: Entity Registration
    # ========================================================================

    def test_register_root_entity(self):
        """LTM-013-INT-008: Register root entity (no parent) (P0)."""
        full_id = self.hierarchy.register_entity("org", "acme")
        self.assertEqual(full_id, "org:acme")

        entity = self.hierarchy.get_entity("org", "acme")
        self.assertIsNotNone(entity)
        self.assertIsNone(entity["parent_id"])

    def test_register_child_entity(self):
        """LTM-013-INT-009: Register child entity with parent (P0)."""
        self.hierarchy.register_entity("org", "acme")
        full_id = self.hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.assertEqual(full_id, "project:alpha")

        entity = self.hierarchy.get_entity("project", "alpha")
        self.assertIsNotNone(entity)
        self.assertEqual(entity["parent_id"], "org:acme")

    def test_register_entity_idempotent(self):
        """Registering same entity twice updates metadata."""
        self.hierarchy.register_entity("org", "acme", metadata={"v": 1})
        self.hierarchy.register_entity("org", "acme", metadata={"v": 2})

        entity = self.hierarchy.get_entity("org", "acme")
        self.assertEqual(entity["metadata"]["v"], 2)

    def test_register_with_nonexistent_parent(self):
        """Reject registration with nonexistent parent."""
        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.register_entity(
                "project", "alpha", parent=("org", "nonexistent")
            )
        self.assertIn("not found", str(ctx.exception))

    # ========================================================================
    # AC-7: Entry Association
    # ========================================================================

    def test_associate_entry_with_leaf_entity(self):
        """LTM-013-INT-011: Associate entry with leaf entity (P0)."""
        self._setup_full_hierarchy()

        result = self.hierarchy.associate_entry("entry_001", "session", "s1")
        self.assertTrue(result)

        # Verify association
        entries = self.hierarchy.get_entries_for_entity("session", "s1")
        self.assertEqual(entries["total_count"], 1)
        self.assertEqual(entries["entries"][0]["entry_id"], "entry_001")

    def test_associate_entry_with_intermediate_entity(self):
        """LTM-013-INT-012: Associate entry with intermediate entity."""
        self._setup_full_hierarchy()

        result = self.hierarchy.associate_entry("entry_002", "project", "alpha")
        self.assertTrue(result)

        entries = self.hierarchy.get_entries_for_entity(
            "project", "alpha", include_descendants=False
        )
        self.assertEqual(entries["total_count"], 1)

    def test_associate_entry_idempotent(self):
        """LTM-013-UNIT-016: Idempotent re-association returns True."""
        self._setup_full_hierarchy()

        self.hierarchy.associate_entry("entry_001", "session", "s1")
        result = self.hierarchy.associate_entry("entry_001", "session", "s1")
        self.assertTrue(result)

        # Should only have one entry
        entries = self.hierarchy.get_entries_for_entity("session", "s1")
        self.assertEqual(entries["total_count"], 1)

    def test_associate_entry_nonexistent_entity(self):
        """LTM-013-UNIT-015: Validate entity exists before association."""
        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.associate_entry("entry_001", "session", "nonexistent")
        self.assertIn("not found", str(ctx.exception))

    # ========================================================================
    # AC-8: Hierarchy Queries
    # ========================================================================

    def test_query_entries_with_descendants(self):
        """LTM-013-INT-013: Query entries for root entity (all descendants) (P0)."""
        self._setup_full_hierarchy()

        # Associate entries at different levels
        self.hierarchy.associate_entry("entry_org", "org", "acme")
        self.hierarchy.associate_entry("entry_project", "project", "alpha")
        self.hierarchy.associate_entry("entry_user", "user", "alice")
        self.hierarchy.associate_entry("entry_session", "session", "s1")

        # Query from org should get all 4 entries
        result = self.hierarchy.get_entries_for_entity("org", "acme")
        self.assertEqual(result["total_count"], 4)

    def test_query_entries_without_descendants(self):
        """LTM-013-INT-014: Query entries with include_descendants=False (P0)."""
        self._setup_full_hierarchy()

        self.hierarchy.associate_entry("entry_org", "org", "acme")
        self.hierarchy.associate_entry("entry_project", "project", "alpha")

        # Query org without descendants should only get 1 entry
        result = self.hierarchy.get_entries_for_entity(
            "org", "acme", include_descendants=False
        )
        self.assertEqual(result["total_count"], 1)
        self.assertEqual(result["entries"][0]["entry_id"], "entry_org")

    def test_query_entries_pagination(self):
        """LTM-013-INT-015: Pagination with limit/offset returns correct slice."""
        self._setup_full_hierarchy()

        # Associate multiple entries
        for i in range(10):
            self.hierarchy.associate_entry(f"entry_{i:03d}", "session", "s1")

        # First page
        result1 = self.hierarchy.get_entries_for_entity(
            "session", "s1", limit=3, offset=0
        )
        self.assertEqual(len(result1["entries"]), 3)
        self.assertEqual(result1["total_count"], 10)
        self.assertTrue(result1["has_more"])

        # Second page
        result2 = self.hierarchy.get_entries_for_entity(
            "session", "s1", limit=3, offset=3
        )
        self.assertEqual(len(result2["entries"]), 3)
        self.assertTrue(result2["has_more"])

        # Last page
        result3 = self.hierarchy.get_entries_for_entity(
            "session", "s1", limit=3, offset=9
        )
        self.assertEqual(len(result3["entries"]), 1)
        self.assertFalse(result3["has_more"])

    # ========================================================================
    # AC-9: Entity Listing
    # ========================================================================

    def test_list_entities_by_type(self):
        """LTM-013-INT-016: List all projects in org."""
        self._setup_full_hierarchy()
        # Add another project
        self.hierarchy.register_entity("project", "beta", parent=("org", "acme"))

        projects = self.hierarchy.list_entities(entity_type="project")
        self.assertEqual(len(projects), 2)
        project_ids = [p["entity_id"] for p in projects]
        self.assertIn("alpha", project_ids)
        self.assertIn("beta", project_ids)

    def test_list_entities_by_parent(self):
        """LTM-013-UNIT-019: Filter by parent tuple."""
        self._setup_full_hierarchy()

        # List direct children of org:acme
        children = self.hierarchy.list_entities(parent=("org", "acme"))
        self.assertEqual(len(children), 1)
        self.assertEqual(children[0]["id"], "project:alpha")

    def test_list_entities_with_limit(self):
        """LTM-013-INT-017: List with limit respects bound."""
        self._setup_full_hierarchy()
        # Add more users
        self.hierarchy.register_entity("user", "bob", parent=("project", "alpha"))
        self.hierarchy.register_entity("user", "carol", parent=("project", "alpha"))

        users = self.hierarchy.list_entities(entity_type="user", limit=2)
        self.assertEqual(len(users), 2)

    # ========================================================================
    # AC-10: Get Ancestors/Descendants
    # ========================================================================

    def test_get_ancestors_ordered(self):
        """LTM-013-INT-018: Get ancestors of session includes all levels."""
        self._setup_full_hierarchy()

        ancestors = self.hierarchy.get_ancestors("session", "s1")
        self.assertEqual(len(ancestors), 3)

        # Should be ordered by depth (closest ancestor first)
        self.assertEqual(ancestors[0]["id"], "user:alice")
        self.assertEqual(ancestors[1]["id"], "project:alpha")
        self.assertEqual(ancestors[2]["id"], "org:acme")

    def test_get_descendants_ordered(self):
        """LTM-013-INT-019: Get descendants of org includes all nested."""
        self._setup_full_hierarchy()

        descendants = self.hierarchy.get_descendants("org", "acme")
        self.assertEqual(len(descendants), 3)

        # Should be ordered by depth (closest descendant first)
        self.assertEqual(descendants[0]["id"], "project:alpha")
        self.assertEqual(descendants[1]["id"], "user:alice")
        self.assertEqual(descendants[2]["id"], "session:s1")

    def test_root_has_no_ancestors(self):
        """LTM-013-UNIT-022: Root entity has no ancestors."""
        self.hierarchy.register_entity("org", "acme")
        ancestors = self.hierarchy.get_ancestors("org", "acme")
        self.assertEqual(len(ancestors), 0)

    def test_leaf_has_no_descendants(self):
        """LTM-013-UNIT-023: Leaf entity has no descendants."""
        self._setup_full_hierarchy()
        descendants = self.hierarchy.get_descendants("session", "s1")
        self.assertEqual(len(descendants), 0)

    # ========================================================================
    # AC-11: Closure Table Auto-populate
    # ========================================================================

    def test_register_root_creates_self_reference(self):
        """LTM-013-INT-020: Register root creates self-reference (depth=0) (P0)."""
        self.hierarchy.register_entity("org", "acme")

        # Check closure table directly
        session = self.hierarchy._get_session()
        try:
            closure = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(ancestor_id="org:acme", descendant_id="org:acme")
                .first()
            )
            self.assertIsNotNone(closure)
            self.assertEqual(closure.depth, 0)
        finally:
            session.close()

    def test_register_child_creates_ancestor_rows(self):
        """LTM-013-INT-021: Register child creates ancestor rows (P0)."""
        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))

        session = self.hierarchy._get_session()
        try:
            # project:alpha should have 2 closure entries
            closures = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(descendant_id="project:alpha")
                .all()
            )
            self.assertEqual(len(closures), 2)

            # Self-reference
            self_ref = next(c for c in closures if c.ancestor_id == "project:alpha")
            self.assertEqual(self_ref.depth, 0)

            # Parent reference
            parent_ref = next(c for c in closures if c.ancestor_id == "org:acme")
            self.assertEqual(parent_ref.depth, 1)
        finally:
            session.close()

    def test_four_level_hierarchy_closure_count(self):
        """LTM-013-INT-022: 4-level hierarchy has correct closure count (P0)."""
        self._setup_full_hierarchy()

        session = self.hierarchy._get_session()
        try:
            total_closures = session.query(self.hierarchy._ltm_closure).count()
            # org:acme -> 1 (self)
            # project:alpha -> 2 (self + org)
            # user:alice -> 3 (self + project + org)
            # session:s1 -> 4 (self + user + project + org)
            # Total = 1 + 2 + 3 + 4 = 10
            self.assertEqual(total_closures, 10)
        finally:
            session.close()

    # ========================================================================
    # AC-12: Cascade Delete
    # ========================================================================

    def test_delete_removes_entity(self):
        """LTM-013-UNIT-024: Delete removes entity from entities table (P0)."""
        self.hierarchy.register_entity("org", "acme")
        result = self.hierarchy.delete_entity("org", "acme")
        self.assertTrue(result)

        entity = self.hierarchy.get_entity("org", "acme")
        self.assertIsNone(entity)

    def test_delete_removes_closure_rows(self):
        """LTM-013-INT-023: Delete removes closure table rows (P0)."""
        self._setup_full_hierarchy()

        # Delete session
        self.hierarchy.delete_entity("session", "s1")

        session = self.hierarchy._get_session()
        try:
            closures = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(descendant_id="session:s1")
                .all()
            )
            self.assertEqual(len(closures), 0)
        finally:
            session.close()

    def test_delete_removes_entry_associations(self):
        """LTM-013-INT-024: Delete removes entry_owners associations (P0)."""
        self._setup_full_hierarchy()
        self.hierarchy.associate_entry("entry_001", "session", "s1")

        self.hierarchy.delete_entity("session", "s1")

        session = self.hierarchy._get_session()
        try:
            owners = (
                session.query(self.hierarchy._ltm_entry_owner)
                .filter_by(entity_id="session:s1")
                .all()
            )
            self.assertEqual(len(owners), 0)
        finally:
            session.close()

    def test_cascade_delete_descendants(self):
        """LTM-013-INT-025: Cascade delete descendants when configured."""
        self._setup_full_hierarchy()

        # Delete user with cascade
        self.hierarchy.delete_entity("user", "alice", cascade=True)

        # Session should be gone too
        entity = self.hierarchy.get_entity("session", "s1")
        self.assertIsNone(entity)

    def test_non_cascade_delete_orphans_descendants(self):
        """LTM-013-UNIT-025: Non-cascade delete orphans descendants."""
        self._setup_full_hierarchy()

        # Delete user without cascade
        self.hierarchy.delete_entity("user", "alice", cascade=False)

        # Session should still exist but be orphaned
        entity = self.hierarchy.get_entity("session", "s1")
        self.assertIsNotNone(entity)
        self.assertIsNone(entity["parent_id"])

    def test_delete_nonexistent_entity(self):
        """Delete nonexistent entity returns False."""
        result = self.hierarchy.delete_entity("org", "nonexistent")
        self.assertFalse(result)

    # ========================================================================
    # Helper Methods
    # ========================================================================

    def _setup_full_hierarchy(self):
        """Set up a standard 4-level hierarchy fixture."""
        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
        self.hierarchy.register_entity("user", "alice", parent=("project", "alpha"))
        self.hierarchy.register_entity("session", "s1", parent=("user", "alice"))


@pytest.mark.skipif(not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed")
class TestEntityHierarchyMove(unittest.TestCase):
    """Tests for move_entity() (TEA-LTM-014)."""

    def setUp(self):
        """Create fresh hierarchy for each test."""
        self.hierarchy = EntityHierarchy(
            levels=["org", "project", "user", "session"],
            url="sqlite:///:memory:",
        )

    def tearDown(self):
        """Close hierarchy after each test."""
        if hasattr(self, "hierarchy"):
            self.hierarchy.close()

    def _setup_full_hierarchy(self):
        """Set up a standard 4-level hierarchy fixture."""
        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
        self.hierarchy.register_entity("user", "alice", parent=("project", "alpha"))
        self.hierarchy.register_entity("session", "s1", parent=("user", "alice"))

    def _setup_multi_project_hierarchy(self):
        """Set up hierarchy with multiple projects for move testing."""
        # org:acme
        #   project:alpha
        #     user:alice
        #       session:s1
        #   project:beta (empty, for moving to)
        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
        self.hierarchy.register_entity("project", "beta", parent=("org", "acme"))
        self.hierarchy.register_entity("user", "alice", parent=("project", "alpha"))
        self.hierarchy.register_entity("session", "s1", parent=("user", "alice"))

    def _setup_multi_org_hierarchy(self):
        """Set up hierarchy with multiple orgs for project move testing."""
        # org:acme
        #   project:alpha
        #     user:alice
        # org:bigcorp (empty, for moving to)
        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("org", "bigcorp")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
        self.hierarchy.register_entity("user", "alice", parent=("project", "alpha"))

    # ========================================================================
    # AC-1: Move Entity API
    # ========================================================================

    def test_move_user_between_projects(self):
        """LTM-014-001: Move user from one project to another (P0)."""
        self._setup_multi_project_hierarchy()

        # Move user:alice from project:alpha to project:beta
        result = self.hierarchy.move_entity("user", "alice", ("project", "beta"))
        self.assertTrue(result)

        # Verify parent changed
        entity = self.hierarchy.get_entity("user", "alice")
        self.assertEqual(entity["parent_id"], "project:beta")

        # Verify ancestors updated
        ancestors = self.hierarchy.get_ancestors("user", "alice")
        ancestor_ids = [a["id"] for a in ancestors]
        self.assertIn("project:beta", ancestor_ids)
        self.assertNotIn("project:alpha", ancestor_ids)

    def test_move_project_between_orgs(self):
        """LTM-014-002: Move project from one org to another (P0)."""
        self._setup_multi_org_hierarchy()

        # Move project:alpha from org:acme to org:bigcorp
        result = self.hierarchy.move_entity("project", "alpha", ("org", "bigcorp"))
        self.assertTrue(result)

        # Verify parent changed
        entity = self.hierarchy.get_entity("project", "alpha")
        self.assertEqual(entity["parent_id"], "org:bigcorp")

        # Verify ancestors updated
        ancestors = self.hierarchy.get_ancestors("project", "alpha")
        ancestor_ids = [a["id"] for a in ancestors]
        self.assertIn("org:bigcorp", ancestor_ids)
        self.assertNotIn("org:acme", ancestor_ids)

    def test_move_leaf_entity(self):
        """LTM-014-003: Move session to new user (simple leaf case)."""
        self._setup_multi_project_hierarchy()
        # Add another user
        self.hierarchy.register_entity("user", "bob", parent=("project", "beta"))

        # Move session:s1 from user:alice to user:bob
        result = self.hierarchy.move_entity("session", "s1", ("user", "bob"))
        self.assertTrue(result)

        # Verify parent changed
        entity = self.hierarchy.get_entity("session", "s1")
        self.assertEqual(entity["parent_id"], "user:bob")

    # ========================================================================
    # AC-2: Level Validation
    # ========================================================================

    def test_reject_wrong_parent_level(self):
        """LTM-014-004: Reject move to wrong parent level (P0)."""
        self._setup_full_hierarchy()
        # Add org for invalid move attempt
        self.hierarchy.register_entity("org", "bigcorp")

        # Try to move user directly to org (should require project)
        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.move_entity("user", "alice", ("org", "bigcorp"))
        self.assertIn("must be 'project'", str(ctx.exception))

    def test_reject_root_entity_move(self):
        """LTM-014-005: Root-level entity cannot be moved (P0)."""
        self._setup_full_hierarchy()

        # Try to move org (root level)
        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.move_entity("org", "acme", ("org", "other"))
        self.assertIn("cannot be moved", str(ctx.exception))

    def test_reject_nonexistent_entity(self):
        """LTM-014-006: Reject move of nonexistent entity (P0)."""
        self._setup_multi_project_hierarchy()

        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.move_entity("user", "nonexistent", ("project", "beta"))
        self.assertIn("not found", str(ctx.exception))

    def test_reject_nonexistent_new_parent(self):
        """LTM-014-007: Reject move to nonexistent parent (P0)."""
        self._setup_full_hierarchy()

        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.move_entity("user", "alice", ("project", "nonexistent"))
        self.assertIn("not found", str(ctx.exception))

    def test_reject_unknown_entity_type(self):
        """LTM-014-008: Reject unknown entity type."""
        self._setup_full_hierarchy()

        with self.assertRaises(ValueError) as ctx:
            self.hierarchy.move_entity("invalid", "alice", ("project", "alpha"))
        self.assertIn("Unknown entity type", str(ctx.exception))

    # ========================================================================
    # AC-3: Closure Table Rebuild
    # ========================================================================

    def test_closure_updated_after_move(self):
        """LTM-014-009: Closure table reflects new ancestry after move (P0)."""
        self._setup_multi_project_hierarchy()

        # Move user:alice from project:alpha to project:beta
        self.hierarchy.move_entity("user", "alice", ("project", "beta"))

        # Verify closure - user:alice should have project:beta and org:acme as ancestors
        session = self.hierarchy._get_session()
        try:
            closures = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(descendant_id="user:alice")
                .all()
            )
            ancestor_ids = [c.ancestor_id for c in closures]

            # Should have: self, project:beta, org:acme
            self.assertIn("user:alice", ancestor_ids)  # self
            self.assertIn("project:beta", ancestor_ids)  # new parent
            self.assertIn("org:acme", ancestor_ids)  # grandparent
            self.assertNotIn("project:alpha", ancestor_ids)  # old parent removed
        finally:
            session.close()

    def test_depth_recalculated_after_move(self):
        """LTM-014-010: Depth values recalculated correctly after move (P0)."""
        self._setup_multi_project_hierarchy()

        # Move user:alice from project:alpha to project:beta
        self.hierarchy.move_entity("user", "alice", ("project", "beta"))

        session = self.hierarchy._get_session()
        try:
            # Check depths for user:alice
            closures = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(descendant_id="user:alice")
                .all()
            )
            depth_map = {c.ancestor_id: c.depth for c in closures}

            self.assertEqual(depth_map["user:alice"], 0)  # self
            self.assertEqual(depth_map["project:beta"], 1)  # parent
            self.assertEqual(depth_map["org:acme"], 2)  # grandparent
        finally:
            session.close()

    # ========================================================================
    # AC-4: Atomic Transaction
    # ========================================================================

    def test_move_same_parent_noop(self):
        """LTM-014-011: Move to same parent is a no-op, returns True."""
        self._setup_full_hierarchy()

        # Move to same parent
        result = self.hierarchy.move_entity("user", "alice", ("project", "alpha"))
        self.assertTrue(result)

        # Verify nothing changed
        entity = self.hierarchy.get_entity("user", "alice")
        self.assertEqual(entity["parent_id"], "project:alpha")

    # ========================================================================
    # AC-5: Descendant Handling
    # ========================================================================

    def test_move_with_descendants(self):
        """LTM-014-012: Move entity with descendants moves entire subtree (P0)."""
        self._setup_multi_project_hierarchy()

        # Move user:alice (with session:s1) from project:alpha to project:beta
        self.hierarchy.move_entity("user", "alice", ("project", "beta"))

        # Verify user parent changed
        user = self.hierarchy.get_entity("user", "alice")
        self.assertEqual(user["parent_id"], "project:beta")

        # Verify session still has user:alice as parent (unchanged)
        session_entity = self.hierarchy.get_entity("session", "s1")
        self.assertEqual(session_entity["parent_id"], "user:alice")

        # Verify session ancestors include new hierarchy
        ancestors = self.hierarchy.get_ancestors("session", "s1")
        ancestor_ids = [a["id"] for a in ancestors]
        self.assertEqual(ancestor_ids, ["user:alice", "project:beta", "org:acme"])

    def test_move_branch_with_full_subtree(self):
        """LTM-014-013: Move project with all nested entities."""
        self._setup_multi_org_hierarchy()

        # Move project:alpha (with user:alice) from org:acme to org:bigcorp
        self.hierarchy.move_entity("project", "alpha", ("org", "bigcorp"))

        # Verify project parent changed
        project = self.hierarchy.get_entity("project", "alpha")
        self.assertEqual(project["parent_id"], "org:bigcorp")

        # Verify user:alice ancestors now go through org:bigcorp
        ancestors = self.hierarchy.get_ancestors("user", "alice")
        ancestor_ids = [a["id"] for a in ancestors]
        self.assertIn("org:bigcorp", ancestor_ids)
        self.assertNotIn("org:acme", ancestor_ids)

    def test_descendant_closure_updated(self):
        """LTM-014-014: Descendant closure entries updated after parent move (P0)."""
        self._setup_multi_project_hierarchy()

        # Move user:alice to project:beta
        self.hierarchy.move_entity("user", "alice", ("project", "beta"))

        session = self.hierarchy._get_session()
        try:
            # Check session:s1 closure reflects new ancestry
            closures = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(descendant_id="session:s1")
                .all()
            )
            ancestor_ids = [c.ancestor_id for c in closures]

            # Should include: self, user:alice, project:beta, org:acme
            self.assertIn("session:s1", ancestor_ids)
            self.assertIn("user:alice", ancestor_ids)
            self.assertIn("project:beta", ancestor_ids)
            self.assertIn("org:acme", ancestor_ids)
            self.assertNotIn("project:alpha", ancestor_ids)
        finally:
            session.close()

    # ========================================================================
    # AC-6: Entry Associations Preserved
    # ========================================================================

    def test_entry_associations_preserved(self):
        """LTM-014-015: Entry associations remain intact after move (P0)."""
        self._setup_multi_project_hierarchy()

        # Associate entries before move
        self.hierarchy.associate_entry("entry_user", "user", "alice")
        self.hierarchy.associate_entry("entry_session", "session", "s1")

        # Move user:alice to project:beta
        self.hierarchy.move_entity("user", "alice", ("project", "beta"))

        # Verify entries still queryable via moved entity
        entries = self.hierarchy.get_entries_for_entity("user", "alice")
        self.assertEqual(entries["total_count"], 2)

        # Verify entries queryable via new parent
        entries = self.hierarchy.get_entries_for_entity("project", "beta")
        self.assertEqual(entries["total_count"], 2)

        # Verify entries NOT queryable via old parent
        entries = self.hierarchy.get_entries_for_entity("project", "alpha")
        self.assertEqual(entries["total_count"], 0)

    def test_entries_queryable_via_new_ancestors(self):
        """LTM-014-016: Entries queryable via new ancestry after move."""
        self._setup_multi_org_hierarchy()

        # Associate entry with user
        self.hierarchy.associate_entry("entry_001", "user", "alice")

        # Move project:alpha to org:bigcorp
        self.hierarchy.move_entity("project", "alpha", ("org", "bigcorp"))

        # Entry should be queryable via org:bigcorp
        entries = self.hierarchy.get_entries_for_entity("org", "bigcorp")
        self.assertEqual(entries["total_count"], 1)

        # Entry should NOT be queryable via org:acme anymore
        entries = self.hierarchy.get_entries_for_entity("org", "acme")
        self.assertEqual(entries["total_count"], 0)


@pytest.mark.skipif(not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed")
class TestEntityHierarchyE2E(unittest.TestCase):
    """End-to-end tests for EntityHierarchy."""

    def setUp(self):
        """Create hierarchy with multiple entities."""
        self.hierarchy = EntityHierarchy(
            levels=["org", "project", "user", "session"],
            url="sqlite:///:memory:",
        )
        self._setup_large_hierarchy()

    def tearDown(self):
        """Close hierarchy."""
        if hasattr(self, "hierarchy"):
            self.hierarchy.close()

    def _setup_large_hierarchy(self):
        """Set up hierarchy with multiple branches."""
        # org:acme
        #   project:alpha
        #     user:alice
        #       session:s1
        #       session:s2
        #     user:bob
        #       session:s3
        #   project:beta
        #     user:carol
        #       session:s4

        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
        self.hierarchy.register_entity("project", "beta", parent=("org", "acme"))
        self.hierarchy.register_entity("user", "alice", parent=("project", "alpha"))
        self.hierarchy.register_entity("user", "bob", parent=("project", "alpha"))
        self.hierarchy.register_entity("user", "carol", parent=("project", "beta"))
        self.hierarchy.register_entity("session", "s1", parent=("user", "alice"))
        self.hierarchy.register_entity("session", "s2", parent=("user", "alice"))
        self.hierarchy.register_entity("session", "s3", parent=("user", "bob"))
        self.hierarchy.register_entity("session", "s4", parent=("user", "carol"))

    def test_full_hierarchy_query(self):
        """LTM-013-E2E-001: Query org entries returns all nested data (P0)."""
        # Associate entries at various levels
        self.hierarchy.associate_entry("e1", "session", "s1")
        self.hierarchy.associate_entry("e2", "session", "s2")
        self.hierarchy.associate_entry("e3", "session", "s3")
        self.hierarchy.associate_entry("e4", "session", "s4")
        self.hierarchy.associate_entry("e5", "user", "alice")
        self.hierarchy.associate_entry("e6", "project", "alpha")

        # Query from org should get all 6 entries
        result = self.hierarchy.get_entries_for_entity("org", "acme")
        self.assertEqual(result["total_count"], 6)

        # Query from project:alpha should get 4 entries (e1, e2, e3, e5, e6)
        result = self.hierarchy.get_entries_for_entity("project", "alpha")
        self.assertEqual(result["total_count"], 5)

        # Query from user:alice should get 3 entries (e1, e2, e5)
        result = self.hierarchy.get_entries_for_entity("user", "alice")
        self.assertEqual(result["total_count"], 3)

    def test_closure_table_complete(self):
        """LTM-013-E2E-002: Register full hierarchy, verify closure table complete."""
        session = self.hierarchy._get_session()
        try:
            # Count all closure entries
            total = session.query(self.hierarchy._ltm_closure).count()

            # Expected counts:
            # org:acme -> 1 (self)
            # project:alpha -> 2, project:beta -> 2
            # user:alice -> 3, user:bob -> 3, user:carol -> 3
            # session:s1 -> 4, session:s2 -> 4, session:s3 -> 4, session:s4 -> 4
            # Total = 1 + 2 + 2 + 3 + 3 + 3 + 4 + 4 + 4 + 4 = 30
            self.assertEqual(total, 30)

            # Verify org:acme has 9 descendants (itself)
            org_descendants = (
                session.query(self.hierarchy._ltm_closure)
                .filter_by(ancestor_id="org:acme")
                .count()
            )
            self.assertEqual(org_descendants, 10)  # self + 9 others

        finally:
            session.close()

    def test_cascade_delete_through_hierarchy(self):
        """LTM-013-E2E-003: Delete org cascades through entire hierarchy."""
        # Associate entries
        self.hierarchy.associate_entry("e1", "session", "s1")
        self.hierarchy.associate_entry("e2", "project", "alpha")

        # Delete org with cascade
        self.hierarchy.delete_entity("org", "acme", cascade=True)

        # All entities should be gone
        self.assertIsNone(self.hierarchy.get_entity("org", "acme"))
        self.assertIsNone(self.hierarchy.get_entity("project", "alpha"))
        self.assertIsNone(self.hierarchy.get_entity("user", "alice"))
        self.assertIsNone(self.hierarchy.get_entity("session", "s1"))

        # Tables should be empty
        session = self.hierarchy._get_session()
        try:
            entities = session.query(self.hierarchy._ltm_entity).count()
            closures = session.query(self.hierarchy._ltm_closure).count()
            owners = session.query(self.hierarchy._ltm_entry_owner).count()

            self.assertEqual(entities, 0)
            self.assertEqual(closures, 0)
            self.assertEqual(owners, 0)
        finally:
            session.close()


@pytest.mark.skipif(not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed")
class TestEntityHierarchyContextManager(unittest.TestCase):
    """Test context manager functionality."""

    def test_context_manager_closes(self):
        """Context manager closes hierarchy on exit."""
        with EntityHierarchy(
            levels=["org", "project"],
            url="sqlite:///:memory:",
        ) as hierarchy:
            hierarchy.register_entity("org", "acme")

        # After exiting, engine should be disposed
        self.assertFalse(hierarchy._initialized)

    def test_context_manager_on_exception(self):
        """Context manager closes hierarchy even on exception."""
        hierarchy = None
        try:
            with EntityHierarchy(
                levels=["org", "project"],
                url="sqlite:///:memory:",
            ) as h:
                hierarchy = h
                raise ValueError("Test exception")
        except ValueError:
            pass

        self.assertFalse(hierarchy._initialized)


@pytest.mark.skipif(not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed")
@pytest.mark.integration
class TestEntityHierarchyPostgreSQL(unittest.TestCase):
    """
    Integration tests with PostgreSQL.

    Requires PostgreSQL container:
        docker run -d --name tea-postgres-test \
          -e POSTGRES_USER=tea \
          -e POSTGRES_PASSWORD=tea \
          -e POSTGRES_DB=tea_test \
          -p 5433:5432 \
          postgres:15

    Run with:
        DATABASE_URL=postgresql://tea:tea@localhost:5433/tea_test \
          pytest tests/test_entity_hierarchy.py -v -m integration
    """

    @pytest.fixture(autouse=True)
    def setup_postgres(self):
        """Set up PostgreSQL connection if available."""
        import os

        self.db_url = os.environ.get("DATABASE_URL")
        if not self.db_url:
            pytest.skip("DATABASE_URL not set for PostgreSQL tests")

        # Clean up any existing tables
        try:
            from sqlalchemy import create_engine, text

            engine = create_engine(self.db_url)
            with engine.connect() as conn:
                conn.execute(text("DROP TABLE IF EXISTS ltm_entry_owners CASCADE"))
                conn.execute(text("DROP TABLE IF EXISTS ltm_entity_closure CASCADE"))
                conn.execute(text("DROP TABLE IF EXISTS ltm_entities CASCADE"))
                conn.commit()
            engine.dispose()
        except Exception:
            pass

        self.hierarchy = EntityHierarchy(
            levels=["org", "project", "user", "session"],
            url=self.db_url,
        )

        yield

        self.hierarchy.close()

    def test_postgres_full_hierarchy(self):
        """Test full hierarchy operations on PostgreSQL."""
        # Register entities
        self.hierarchy.register_entity("org", "acme")
        self.hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
        self.hierarchy.register_entity("user", "alice", parent=("project", "alpha"))
        self.hierarchy.register_entity("session", "s1", parent=("user", "alice"))

        # Associate entries
        self.hierarchy.associate_entry("entry_001", "session", "s1")

        # Query
        result = self.hierarchy.get_entries_for_entity("org", "acme")
        self.assertEqual(result["total_count"], 1)

        # Get ancestors
        ancestors = self.hierarchy.get_ancestors("session", "s1")
        self.assertEqual(len(ancestors), 3)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
