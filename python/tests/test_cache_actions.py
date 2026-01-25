"""
Tests for Cache and Memoization Actions (TEA-BUILTIN-010).

Tests cover:
- cache.wrap action with various key strategies
- Cache hit/miss logic
- TTL expiration handling
- Graceful LTM failure handling
- cache.get action
- cache.invalidate action
- storage.hash action
- sha256 Jinja filter
- Automatic cleanup with probability
"""

import hashlib
import json
import os
import tempfile
import time
import unittest
from datetime import datetime, timedelta, timezone
from unittest.mock import MagicMock, patch

from the_edge_agent.actions.cache_actions import (
    _compute_cache_key,
    _compute_ttl_seconds,
    _get_agent_name,
    _is_expired,
    _pattern_matches,
    _run_cleanup,
    register_actions,
    register_jinja_filters,
)


class MockLTMBackend:
    """Mock LTM backend for testing cache actions."""

    def __init__(self):
        self.storage = {}
        self.metadata = {}

    def store(self, key, value, metadata=None):
        self.storage[key] = value
        self.metadata[key] = metadata or {}
        return {"success": True, "stored": True, "key": key, "created": True}

    def retrieve(self, key, default=None):
        if key in self.storage:
            return {
                "success": True,
                "found": True,
                "value": self.storage[key],
                "metadata": self.metadata.get(key, {}),
            }
        return {
            "success": True,
            "found": False,
            "value": default,
            "metadata": None,
        }

    def delete(self, key):
        if key in self.storage:
            del self.storage[key]
            if key in self.metadata:
                del self.metadata[key]
            return {"success": True, "deleted": True, "key": key}
        return {"success": True, "deleted": False, "key": key}

    def search(self, query=None, metadata_filter=None, limit=10):
        results = []
        for key, value in list(self.storage.items())[:limit]:
            if metadata_filter:
                meta = self.metadata.get(key, {})
                match = all(meta.get(k) == v for k, v in metadata_filter.items())
                if not match:
                    continue
            results.append(
                {
                    "key": key,
                    "value": value,
                    "metadata": self.metadata.get(key, {}),
                }
            )
        return {"success": True, "results": results, "count": len(results)}


class MockEngine:
    """Mock YAMLEngine for testing."""

    def __init__(self, ltm_backend=None):
        self._ltm_backend = ltm_backend


class TestCacheKeyComputation(unittest.TestCase):
    """Tests for cache key generation strategies."""

    def test_custom_key_with_prefix(self):
        """Custom key already prefixed should be preserved."""
        key = _compute_cache_key(
            action="test.action",
            args={"a": 1},
            key="cache:custom:key",
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
        )
        self.assertEqual(key, "cache:custom:key")

    def test_custom_key_without_prefix(self):
        """Custom key without prefix should get cache: prefix."""
        key = _compute_cache_key(
            action="test.action",
            args={"a": 1},
            key="custom:key",
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
        )
        self.assertEqual(key, "cache:custom:key")

    def test_args_strategy_deterministic(self):
        """Args strategy should produce deterministic keys."""
        args = {"model": "gpt-4", "text": "hello"}
        key1 = _compute_cache_key(
            action="llm.call",
            args=args,
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
        )
        key2 = _compute_cache_key(
            action="llm.call",
            args=args,
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
        )
        self.assertEqual(key1, key2)
        self.assertTrue(key1.startswith("cache:llm.call:"))

    def test_args_strategy_different_args(self):
        """Different args should produce different keys."""
        key1 = _compute_cache_key(
            action="llm.call",
            args={"text": "hello"},
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
        )
        key2 = _compute_cache_key(
            action="llm.call",
            args={"text": "world"},
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
        )
        self.assertNotEqual(key1, key2)

    def test_sha256_strategy_with_key_source(self):
        """SHA256 strategy should hash the key_source value."""
        key = _compute_cache_key(
            action="test.action",
            args={"content": "hello world"},
            key=None,
            key_strategy="sha256",
            key_source="content",
            registry={},
            state={},
        )
        expected_hash = hashlib.sha256(b"hello world").hexdigest()
        self.assertEqual(key, f"cache:test.action:{expected_hash}")

    def test_sha256_strategy_bytes_input(self):
        """SHA256 strategy should handle bytes input."""
        key = _compute_cache_key(
            action="test.action",
            args={"content": b"binary data"},
            key=None,
            key_strategy="sha256",
            key_source="content",
            registry={},
            state={},
        )
        expected_hash = hashlib.sha256(b"binary data").hexdigest()
        self.assertEqual(key, f"cache:test.action:{expected_hash}")


class TestTTLComputation(unittest.TestCase):
    """Tests for TTL computation."""

    def test_default_ttl(self):
        """Default TTL should be 60 days."""
        ttl = _compute_ttl_seconds(None, None, None)
        self.assertEqual(ttl, 60 * 86400)

    def test_ttl_days(self):
        """TTL days should be converted to seconds."""
        ttl = _compute_ttl_seconds(30, None, None)
        self.assertEqual(ttl, 30 * 86400)

    def test_ttl_hours_overrides_days(self):
        """TTL hours should override days."""
        ttl = _compute_ttl_seconds(30, 24, None)
        self.assertEqual(ttl, 24 * 3600)

    def test_ttl_seconds_overrides_all(self):
        """TTL seconds should override everything."""
        ttl = _compute_ttl_seconds(30, 24, 3600)
        self.assertEqual(ttl, 3600)


class TestExpiration(unittest.TestCase):
    """Tests for expiration checking."""

    def test_expired_past_date(self):
        """Past date should be expired."""
        past = (datetime.now(timezone.utc) - timedelta(hours=1)).isoformat() + "Z"
        self.assertTrue(_is_expired(past))

    def test_not_expired_future_date(self):
        """Future date should not be expired."""
        future = (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat() + "Z"
        self.assertFalse(_is_expired(future))

    def test_expired_none(self):
        """None should be treated as expired."""
        self.assertTrue(_is_expired(None))

    def test_expired_invalid_format(self):
        """Invalid format should be treated as expired."""
        self.assertTrue(_is_expired("not-a-date"))


class TestPatternMatching(unittest.TestCase):
    """Tests for cache key pattern matching."""

    def test_exact_match(self):
        """Exact match should work."""
        self.assertTrue(_pattern_matches("cache:test", "cache:test"))

    def test_wildcard_suffix(self):
        """Wildcard at end should match."""
        self.assertTrue(_pattern_matches("cache:llm.call:abc123", "cache:llm.call:*"))

    def test_wildcard_prefix(self):
        """Wildcard at beginning should match."""
        self.assertTrue(_pattern_matches("cache:test:key", "*:key"))

    def test_wildcard_middle(self):
        """Wildcard in middle should match."""
        self.assertTrue(_pattern_matches("cache:llm:call:abc", "cache:*:abc"))

    def test_no_match(self):
        """Non-matching pattern should return False."""
        self.assertFalse(_pattern_matches("cache:test", "cache:other"))


class TestCacheWrapAction(unittest.TestCase):
    """Tests for cache.wrap action."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.registry = {}
        register_actions(self.registry, self.engine)

        # Add a test action to wrap
        self.call_count = 0

        def test_action(state, value=None, **kwargs):
            self.call_count += 1
            return {"success": True, "result": f"processed:{value}"}

        self.registry["test.action"] = test_action

    def test_cache_miss_executes_action(self):
        """First call should execute wrapped action."""
        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )
        self.assertTrue(result["success"])
        self.assertFalse(result["_cache_hit"])
        self.assertEqual(self.call_count, 1)

    def test_cache_hit_returns_cached(self):
        """Second call should return cached result."""
        # First call
        result1 = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        # Second call with same args
        result2 = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertTrue(result2["_cache_hit"])
        self.assertEqual(result2["result"], result1["result"])
        self.assertEqual(self.call_count, 1)  # Action called only once

    def test_skip_cache_bypasses_lookup(self):
        """skip_cache=True should always execute."""
        # First call
        self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        # Second call with skip_cache
        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
            skip_cache=True,
        )

        self.assertFalse(result["_cache_hit"])
        self.assertEqual(self.call_count, 2)  # Action called twice

    def test_cache_disabled_no_caching(self):
        """cache_enabled=False should disable all caching."""
        # First call
        self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
            cache_enabled=False,
        )

        # Second call
        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
            cache_enabled=False,
        )

        self.assertFalse(result["_cache_hit"])
        self.assertIsNone(result["_cache_key"])
        self.assertEqual(self.call_count, 2)

    def test_expired_cache_is_miss(self):
        """Expired entries should trigger fresh execution."""
        # Store expired entry directly
        cache_key = (
            "cache:test.action:"
            + hashlib.sha256(
                json.dumps({"value": "hello"}, sort_keys=True).encode()
            ).hexdigest()
        )

        past = (datetime.now(timezone.utc) - timedelta(hours=1)).isoformat() + "Z"
        self.ltm.store(
            key=cache_key,
            value={"result": {"success": True, "result": "old"}},
            metadata={"_cache_expires_at": past},
        )

        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertFalse(result["_cache_hit"])
        self.assertEqual(self.call_count, 1)

    def test_action_not_found_error(self):
        """Non-existent action should return error."""
        result = self.registry["cache.wrap"](
            state={},
            action="nonexistent.action",
            args={},
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "action_not_found")

    def test_action_exception_handled(self):
        """Action exceptions should be caught and returned."""

        def failing_action(state, **kwargs):
            raise ValueError("Test error")

        self.registry["failing.action"] = failing_action

        result = self.registry["cache.wrap"](
            state={},
            action="failing.action",
            args={},
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "action_error")
        self.assertIn("Test error", result["error"])


class TestCacheWrapGracefulDegradation(unittest.TestCase):
    """Tests for graceful LTM failure handling (AC-17)."""

    def setUp(self):
        self.registry = {}
        self.call_count = 0

        def test_action(state, value=None, **kwargs):
            self.call_count += 1
            return {"success": True, "result": f"processed:{value}"}

        self.registry["test.action"] = test_action

    def test_ltm_not_configured(self):
        """Action should work without LTM backend."""
        engine = MockEngine(ltm_backend=None)
        register_actions(self.registry, engine)

        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["_cache_hit"])
        self.assertEqual(self.call_count, 1)

    def test_ltm_retrieve_failure(self):
        """LTM retrieve failure should not fail the action."""

        class FailingLTM:
            def retrieve(self, key):
                raise Exception("LTM retrieve failed")

            def store(self, key, value, metadata=None):
                return {"success": True}

        engine = MockEngine(ltm_backend=FailingLTM())
        register_actions(self.registry, engine)

        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["_cache_hit"])

    def test_ltm_store_failure(self):
        """LTM store failure should not fail the action."""

        class FailingStoreLTM:
            def retrieve(self, key):
                return {"found": False}

            def store(self, key, value, metadata=None):
                raise Exception("LTM store failed")

        engine = MockEngine(ltm_backend=FailingStoreLTM())
        register_actions(self.registry, engine)

        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["_cache_hit"])


class TestCacheGetAction(unittest.TestCase):
    """Tests for cache.get action."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.registry = {}
        register_actions(self.registry, self.engine)

    def test_get_existing_key(self):
        """Should retrieve existing cached value."""
        future = (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat() + "Z"
        self.ltm.store(
            key="cache:test:key",
            value={"result": "cached_value"},
            metadata={"_cache_expires_at": future},
        )

        result = self.registry["cache.get"](
            state={},
            key="cache:test:key",
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], "cached_value")
        self.assertFalse(result["expired"])

    def test_get_nonexistent_key(self):
        """Should return found=False for missing key."""
        result = self.registry["cache.get"](
            state={},
            key="nonexistent",
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["found"])
        self.assertIsNone(result["value"])

    def test_get_expired_key(self):
        """Should indicate expired status."""
        past = (datetime.now(timezone.utc) - timedelta(hours=1)).isoformat() + "Z"
        self.ltm.store(
            key="cache:expired:key",
            value={"result": "old_value"},
            metadata={"_cache_expires_at": past},
        )

        result = self.registry["cache.get"](
            state={},
            key="cache:expired:key",
        )

        self.assertTrue(result["found"])
        self.assertTrue(result["expired"])

    def test_get_with_metadata(self):
        """Should include metadata when requested."""
        future = (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat() + "Z"
        self.ltm.store(
            key="cache:meta:key",
            value={"result": "value"},
            metadata={
                "_cache_expires_at": future,
                "_cache_action": "test.action",
            },
        )

        result = self.registry["cache.get"](
            state={},
            key="cache:meta:key",
            include_metadata=True,
        )

        self.assertIn("metadata", result)
        self.assertEqual(result["metadata"]["_cache_action"], "test.action")

    def test_get_missing_key_parameter(self):
        """Should error when key is missing."""
        result = self.registry["cache.get"](
            state={},
            key=None,
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestCacheInvalidateAction(unittest.TestCase):
    """Tests for cache.invalidate action."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.registry = {}
        register_actions(self.registry, self.engine)

        # Pre-populate cache entries
        for i in range(5):
            self.ltm.store(
                key=f"cache:test:key{i}",
                value={"result": f"value{i}"},
                metadata={
                    "_cache_type": "action_result",
                    "_cache_action": "test.action",
                },
            )

    def test_invalidate_by_exact_key(self):
        """Should delete entry by exact key."""
        result = self.registry["cache.invalidate"](
            state={},
            key="cache:test:key0",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["deleted_count"], 1)
        self.assertIn("cache:test:key0", result["deleted_keys"])

    def test_invalidate_by_metadata_filter(self):
        """Should delete entries matching metadata filter."""
        result = self.registry["cache.invalidate"](
            state={},
            metadata_filter={"_cache_action": "test.action"},
        )

        self.assertTrue(result["success"])
        self.assertGreater(result["deleted_count"], 0)

    def test_invalidate_by_pattern(self):
        """Should delete entries matching pattern."""
        result = self.registry["cache.invalidate"](
            state={},
            pattern="cache:test:*",
        )

        self.assertTrue(result["success"])
        self.assertGreater(result["deleted_count"], 0)

    def test_invalidate_missing_parameters(self):
        """Should error when no filter specified."""
        result = self.registry["cache.invalidate"](
            state={},
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestStorageHashAction(unittest.TestCase):
    """Tests for storage.hash action."""

    def setUp(self):
        self.engine = MockEngine(ltm_backend=None)
        self.registry = {}
        register_actions(self.registry, self.engine)

    def test_hash_local_file(self):
        """Should compute hash of local file."""
        with tempfile.NamedTemporaryFile(mode="w", delete=False, suffix=".txt") as f:
            f.write("test content")
            temp_path = f.name

        try:
            result = self.registry["storage.hash"](
                state={},
                path=temp_path,
            )

            self.assertTrue(result["success"])
            self.assertEqual(result["algorithm"], "sha256")
            expected_hash = hashlib.sha256(b"test content").hexdigest()
            self.assertEqual(result["hash"], expected_hash)
            self.assertEqual(result["size_bytes"], 12)
        finally:
            os.unlink(temp_path)

    def test_hash_md5_algorithm(self):
        """Should support MD5 algorithm."""
        with tempfile.NamedTemporaryFile(mode="w", delete=False) as f:
            f.write("test")
            temp_path = f.name

        try:
            result = self.registry["storage.hash"](
                state={},
                path=temp_path,
                algorithm="md5",
            )

            self.assertTrue(result["success"])
            self.assertEqual(result["algorithm"], "md5")
            expected_hash = hashlib.md5(b"test").hexdigest()
            self.assertEqual(result["hash"], expected_hash)
        finally:
            os.unlink(temp_path)

    def test_hash_blake2b_algorithm(self):
        """Should support Blake2b algorithm."""
        with tempfile.NamedTemporaryFile(mode="w", delete=False) as f:
            f.write("test")
            temp_path = f.name

        try:
            result = self.registry["storage.hash"](
                state={},
                path=temp_path,
                algorithm="blake2b",
            )

            self.assertTrue(result["success"])
            self.assertEqual(result["algorithm"], "blake2b")
        finally:
            os.unlink(temp_path)

    def test_hash_nonexistent_file(self):
        """Should handle file not found gracefully."""
        result = self.registry["storage.hash"](
            state={},
            path="/nonexistent/path/file.txt",
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "file_error")

    def test_hash_missing_path(self):
        """Should error when path is missing."""
        result = self.registry["storage.hash"](
            state={},
            path=None,
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestSha256JinjaFilter(unittest.TestCase):
    """Tests for sha256 Jinja filter."""

    def test_filter_string(self):
        """Should hash string input."""
        from jinja2 import Environment

        env = Environment()
        register_jinja_filters(env)

        template = env.from_string("{{ value | sha256 }}")
        result = template.render(value="hello")

        expected = hashlib.sha256(b"hello").hexdigest()
        self.assertEqual(result, expected)

    def test_filter_bytes(self):
        """Should hash bytes input."""
        from jinja2 import Environment

        env = Environment()
        register_jinja_filters(env)

        template = env.from_string("{{ value | sha256 }}")
        result = template.render(value=b"hello")

        expected = hashlib.sha256(b"hello").hexdigest()
        self.assertEqual(result, expected)

    def test_filter_number(self):
        """Should hash numeric input as string."""
        from jinja2 import Environment

        env = Environment()
        register_jinja_filters(env)

        template = env.from_string("{{ value | sha256 }}")
        result = template.render(value=12345)

        expected = hashlib.sha256(b"12345").hexdigest()
        self.assertEqual(result, expected)


class TestAutomaticCleanup(unittest.TestCase):
    """Tests for automatic cleanup (AC-14, AC-15, AC-16)."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)

        # Pre-populate with expired entries
        past = (datetime.now(timezone.utc) - timedelta(hours=1)).isoformat() + "Z"
        for i in range(10):
            self.ltm.store(
                key=f"cache:expired:key{i}",
                value={"result": f"value{i}"},
                metadata={
                    "_cache_type": "action_result",
                    "_cache_expires_at": past,
                },
            )

    def test_cleanup_deletes_expired(self):
        """Cleanup should delete expired entries."""
        initial_count = len(self.ltm.storage)
        _run_cleanup(self.engine, limit=5)

        self.assertLess(len(self.ltm.storage), initial_count)

    def test_cleanup_respects_limit(self):
        """Cleanup should respect deletion limit."""
        initial_count = len(self.ltm.storage)
        _run_cleanup(self.engine, limit=3)

        # Should delete exactly 3 (the limit)
        self.assertEqual(len(self.ltm.storage), initial_count - 3)

    def test_cleanup_no_ltm(self):
        """Cleanup should handle missing LTM gracefully."""
        engine = MockEngine(ltm_backend=None)
        _run_cleanup(engine, limit=5)  # Should not raise


class TestCacheMetadata(unittest.TestCase):
    """Tests for cache metadata (AC-19)."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.registry = {}
        register_actions(self.registry, self.engine)

        def test_action(state, **kwargs):
            return {"success": True, "data": "result"}

        self.registry["test.action"] = test_action

    def test_metadata_fields_stored(self):
        """Should store standard metadata fields."""
        self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"foo": "bar"},
        )

        # Find the cached entry
        for key, metadata in self.ltm.metadata.items():
            if key.startswith("cache:"):
                self.assertEqual(metadata["_cache_type"], "action_result")
                self.assertEqual(metadata["_cache_action"], "test.action")
                self.assertIn("_cache_created_at", metadata)
                self.assertIn("_cache_expires_at", metadata)
                self.assertIn("_cache_key", metadata)
                break


class TestDualNamespace(unittest.TestCase):
    """Tests for dual namespace registration (AC-22)."""

    def test_cache_namespace(self):
        """Actions should be registered under cache.* namespace."""
        registry = {}
        engine = MockEngine(ltm_backend=None)
        register_actions(registry, engine)

        self.assertIn("cache.wrap", registry)
        self.assertIn("cache.get", registry)
        self.assertIn("cache.invalidate", registry)
        self.assertIn("storage.hash", registry)

    def test_actions_namespace(self):
        """Actions should be registered under actions.* namespace."""
        registry = {}
        engine = MockEngine(ltm_backend=None)
        register_actions(registry, engine)

        self.assertIn("actions.cache_wrap", registry)
        self.assertIn("actions.cache_get", registry)
        self.assertIn("actions.cache_invalidate", registry)
        self.assertIn("actions.storage_hash", registry)


class TestCleanupProbability(unittest.TestCase):
    """Tests for probabilistic cleanup (AC-14)."""

    def test_cleanup_probability_zero(self):
        """cleanup_probability=0 should never run cleanup."""
        ltm = MockLTMBackend()
        engine = MockEngine(ltm_backend=ltm)
        registry = {}
        register_actions(registry, engine)

        def test_action(state, **kwargs):
            return {"success": True}

        registry["test.action"] = test_action

        # Add expired entry
        past = (datetime.now(timezone.utc) - timedelta(hours=1)).isoformat() + "Z"
        ltm.store(
            key="cache:expired:test",
            value={"result": "old"},
            metadata={"_cache_type": "action_result", "_cache_expires_at": past},
        )

        # Run many times with probability 0
        for _ in range(100):
            registry["cache.wrap"](
                state={},
                action="test.action",
                args={"value": str(time.time())},  # Force cache miss
                cleanup_probability=0.0,
            )

        # Expired entry should still exist
        self.assertIn("cache:expired:test", ltm.storage)


class TestAgentNameResolution(unittest.TestCase):
    """Tests for agent name resolution (TEA-BUILTIN-010.1 AC-2)."""

    def test_settings_name_priority(self):
        """settings.name should have highest priority."""
        engine = MockEngine(ltm_backend=None)
        engine._config = {
            "name": "top_level_name",
            "settings": {"name": "settings_name"},
        }
        engine.workflow_name = "workflow_name"

        name = _get_agent_name(engine)
        self.assertEqual(name, "settings_name")

    def test_top_level_name_fallback(self):
        """Top-level name should be used if settings.name not present."""
        engine = MockEngine(ltm_backend=None)
        engine._config = {"name": "top_level_name", "settings": {}}
        engine.workflow_name = "workflow_name"

        name = _get_agent_name(engine)
        self.assertEqual(name, "top_level_name")

    def test_workflow_name_fallback(self):
        """workflow_name should be used if no name in config."""
        engine = MockEngine(ltm_backend=None)
        engine._config = {"settings": {}}
        engine.workflow_name = "my_workflow"

        name = _get_agent_name(engine)
        self.assertEqual(name, "my_workflow")

    def test_unknown_agent_fallback(self):
        """Should fall back to unknown_agent when no name sources available."""
        engine = MockEngine(ltm_backend=None)
        engine._config = None

        name = _get_agent_name(engine)
        self.assertEqual(name, "unknown_agent")

    def test_no_config_attribute(self):
        """Should handle engine without _config attribute."""
        engine = MockEngine(ltm_backend=None)
        # engine has no _config attribute (not set by default in MockEngine)

        name = _get_agent_name(engine)
        self.assertEqual(name, "unknown_agent")

    def test_empty_settings_name(self):
        """Empty settings.name should fallback to next option."""
        engine = MockEngine(ltm_backend=None)
        engine._config = {"name": "fallback", "settings": {"name": ""}}

        name = _get_agent_name(engine)
        self.assertEqual(name, "fallback")

    def test_none_settings_name(self):
        """None settings.name should fallback to next option."""
        engine = MockEngine(ltm_backend=None)
        engine._config = {"name": "fallback", "settings": {"name": None}}

        name = _get_agent_name(engine)
        self.assertEqual(name, "fallback")


class TestAgentPrefixedCacheKey(unittest.TestCase):
    """Tests for automatic agent prefix in cache keys (TEA-BUILTIN-010.1 AC-1, AC-3)."""

    def setUp(self):
        self.engine = MockEngine(ltm_backend=None)
        self.engine._config = {"name": "my_agent"}

    def test_args_strategy_with_prefix(self):
        """Args strategy should include agent prefix."""
        key = _compute_cache_key(
            action="llm.call",
            args={"text": "hello"},
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
            engine=self.engine,
            key_prefix=True,
        )
        self.assertTrue(key.startswith("cache:my_agent:llm.call:"))

    def test_custom_key_with_prefix(self):
        """Custom key should get agent prefix."""
        key = _compute_cache_key(
            action="test.action",
            args={},
            key="custom:key",
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
            engine=self.engine,
            key_prefix=True,
        )
        self.assertEqual(key, "cache:my_agent:custom:key")

    def test_key_prefix_false_no_prefix(self):
        """key_prefix=False should not add agent prefix."""
        key = _compute_cache_key(
            action="llm.call",
            args={"text": "hello"},
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
            engine=self.engine,
            key_prefix=False,
        )
        self.assertTrue(key.startswith("cache:llm.call:"))
        self.assertNotIn("my_agent", key)

    def test_no_engine_no_prefix(self):
        """No engine should result in no prefix."""
        key = _compute_cache_key(
            action="llm.call",
            args={"text": "hello"},
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
            engine=None,
            key_prefix=True,
        )
        self.assertTrue(key.startswith("cache:llm.call:"))
        self.assertNotIn("my_agent", key)

    def test_sha256_strategy_with_prefix(self):
        """SHA256 strategy should include agent prefix."""
        key = _compute_cache_key(
            action="test.action",
            args={"content": "hello world"},
            key=None,
            key_strategy="sha256",
            key_source="content",
            registry={},
            state={},
            engine=self.engine,
            key_prefix=True,
        )
        self.assertTrue(key.startswith("cache:my_agent:test.action:"))

    def test_key_format_matches_spec(self):
        """Key format should be cache:{agent_name}:{user_key}."""
        key = _compute_cache_key(
            action="llm.call",
            args={"text": "test"},
            key=None,
            key_strategy="args",
            key_source=None,
            registry={},
            state={},
            engine=self.engine,
            key_prefix=True,
        )
        # Format: cache:my_agent:llm.call:{hash}
        parts = key.split(":")
        self.assertEqual(parts[0], "cache")
        self.assertEqual(parts[1], "my_agent")
        self.assertEqual(parts[2], "llm.call")
        self.assertEqual(len(parts), 4)  # cache:agent:action:hash


class TestAgentPrefixInCacheWrap(unittest.TestCase):
    """Integration tests for agent prefix in cache.wrap action (TEA-BUILTIN-010.1)."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.engine._config = {"name": "test_agent"}
        self.registry = {}
        register_actions(self.registry, self.engine)

        self.call_count = 0

        def test_action(state, **kwargs):
            self.call_count += 1
            return {"success": True, "data": "result"}

        self.registry["test.action"] = test_action

    def test_cache_key_includes_agent_prefix(self):
        """cache.wrap should produce keys with agent prefix."""
        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["_cache_key"].startswith("cache:test_agent:"))

    def test_cache_hit_with_prefixed_key(self):
        """Cache hit should work with prefixed keys."""
        # First call
        result1 = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        # Second call
        result2 = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
        )

        self.assertTrue(result2["_cache_hit"])
        self.assertEqual(result2["_cache_key"], result1["_cache_key"])
        self.assertTrue(result2["_cache_key"].startswith("cache:test_agent:"))
        self.assertEqual(self.call_count, 1)  # Action called only once

    def test_key_prefix_false_no_agent_prefix(self):
        """key_prefix=False should not include agent prefix."""
        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "hello"},
            key_prefix=False,
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["_cache_key"].startswith("cache:test_agent:"))
        self.assertTrue(result["_cache_key"].startswith("cache:test.action:"))


class TestAgentSpecificInvalidation(unittest.TestCase):
    """Integration tests for agent-specific cache invalidation (TEA-BUILTIN-010.1 AC-8, AC-9)."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.registry = {}
        register_actions(self.registry, self.engine)

        # Pre-populate cache with entries from different "agents"
        # Simulating keys from agent1
        self.ltm.store(
            key="cache:agent1:llm.call:hash1",
            value={"result": "value1"},
            metadata={"_cache_type": "action_result"},
        )
        self.ltm.store(
            key="cache:agent1:llm.call:hash2",
            value={"result": "value2"},
            metadata={"_cache_type": "action_result"},
        )
        # Simulating keys from agent2
        self.ltm.store(
            key="cache:agent2:llm.call:hash3",
            value={"result": "value3"},
            metadata={"_cache_type": "action_result"},
        )
        # Old-style key (no agent prefix)
        self.ltm.store(
            key="cache:llm.call:hash4",
            value={"result": "value4"},
            metadata={"_cache_type": "action_result"},
        )

    def test_invalidate_agent_specific_pattern(self):
        """Pattern cache:agent1:* should only delete agent1 entries."""
        result = self.registry["cache.invalidate"](
            state={},
            pattern="cache:agent1:*",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["deleted_count"], 2)

        # agent1 entries should be gone
        self.assertNotIn("cache:agent1:llm.call:hash1", self.ltm.storage)
        self.assertNotIn("cache:agent1:llm.call:hash2", self.ltm.storage)

        # agent2 and old-style entries should remain
        self.assertIn("cache:agent2:llm.call:hash3", self.ltm.storage)
        self.assertIn("cache:llm.call:hash4", self.ltm.storage)

    def test_invalidate_all_pattern(self):
        """Pattern cache:* should delete all entries."""
        result = self.registry["cache.invalidate"](
            state={},
            pattern="cache:*",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["deleted_count"], 4)
        self.assertEqual(len(self.ltm.storage), 0)

    def test_invalidate_action_specific_across_agents(self):
        """Pattern cache:*:llm.call:* should work across agents."""
        result = self.registry["cache.invalidate"](
            state={},
            pattern="cache:*:llm.call:*",
        )

        self.assertTrue(result["success"])
        # Should match agent1:llm.call and agent2:llm.call
        self.assertGreaterEqual(result["deleted_count"], 3)


class TestBackwardCompatibility(unittest.TestCase):
    """Tests for backward compatibility (TEA-BUILTIN-010.1 AC-5, AC-6, AC-7)."""

    def setUp(self):
        self.ltm = MockLTMBackend()
        self.engine = MockEngine(ltm_backend=self.ltm)
        self.engine._config = {"name": "my_agent"}
        self.registry = {}
        register_actions(self.registry, self.engine)

        def test_action(state, **kwargs):
            return {"success": True, "data": "fresh_result"}

        self.registry["test.action"] = test_action

    def test_old_keys_still_readable(self):
        """Old-style keys (without agent prefix) should still be readable."""
        # Store an old-style key directly
        future = (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat() + "Z"
        old_key = "cache:test.action:oldhash123"
        self.ltm.store(
            key=old_key,
            value={"result": {"success": True, "data": "old_result"}},
            metadata={
                "_cache_type": "action_result",
                "_cache_expires_at": future,
            },
        )

        # Should be retrievable via cache.get
        result = self.registry["cache.get"](
            state={},
            key=old_key,
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"]["data"], "old_result")

    def test_new_keys_written_with_prefix(self):
        """New entries should be written with agent prefix."""
        result = self.registry["cache.wrap"](
            state={},
            action="test.action",
            args={"value": "new_value"},
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["_cache_key"].startswith("cache:my_agent:"))

    def test_agents_without_name_use_fallback(self):
        """Agents without explicit name should use fallback."""
        engine = MockEngine(ltm_backend=self.ltm)
        engine._config = {}  # No name
        engine.workflow_name = "workflow_from_file"
        registry = {}
        register_actions(registry, engine)

        def test_action(state, **kwargs):
            return {"success": True}

        registry["test.action"] = test_action

        result = registry["cache.wrap"](
            state={},
            action="test.action",
            args={"x": 1},
        )

        self.assertTrue(result["_cache_key"].startswith("cache:workflow_from_file:"))


# Check for HierarchicalLTMBackend dependencies
HIERARCHICAL_AVAILABLE = False
try:
    import sqlalchemy
    import fsspec

    HIERARCHICAL_AVAILABLE = True
except ImportError:
    pass


@unittest.skipUnless(
    HIERARCHICAL_AVAILABLE, "SQLAlchemy and fsspec required for hierarchical tests"
)
class TestCacheWrapHierarchicalIntegration(unittest.TestCase):
    """
    Integration tests for cache.wrap with HierarchicalLTMBackend (TEA-FIX-001).

    These tests verify that cache.wrap correctly stores large cached results
    in the HierarchicalLTMBackend when no entity is provided.
    """

    def setUp(self):
        """Set up test fixtures with real HierarchicalLTMBackend."""
        from the_edge_agent.memory import HierarchicalLTMBackend

        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = f"file://{self.temp_dir}/"

        # Create real HierarchicalLTMBackend with low inline_threshold
        self.ltm = HierarchicalLTMBackend(
            catalog_url="sqlite:///:memory:",
            storage_uri=self.storage_uri,
            hierarchy_levels=["org", "project", "user", "session"],
            inline_threshold=100,  # Force blob storage for larger results
            cache_path="_cache/",
            lazy=False,
        )

        self.engine = MockEngine(ltm_backend=self.ltm)
        self.registry = {}
        register_actions(self.registry, self.engine)

    def tearDown(self):
        """Clean up test fixtures."""
        self.ltm.close()

        import shutil

        try:
            shutil.rmtree(self.temp_dir)
        except Exception:
            pass

    def test_cache_wrap_large_result_without_entity(self):
        """
        TEA-FIX-001 AC-2, AC-4: cache.wrap stores large results without entity.

        This is the primary integration test verifying the bug fix.
        Before the fix, large results would fail to store because the backend
        required an entity to generate a blob path.
        """

        # Create a test action that returns a large result
        def large_result_action(state, **kwargs):
            return {"success": True, "data": "x" * 200}  # > inline_threshold

        self.registry["test.large"] = large_result_action

        # First call - cache miss, should store result
        result1 = self.registry["cache.wrap"](
            state={},
            action="test.large",
            args={},
            key="test_large_cache_key",
        )

        self.assertTrue(result1["success"])
        self.assertFalse(result1["_cache_hit"])
        self.assertIn("data", result1["result"])

        # Verify the entry was stored using the computed cache key
        actual_cache_key = result1["_cache_key"]
        retrieve_result = self.ltm.retrieve(actual_cache_key)
        self.assertTrue(retrieve_result["success"])
        self.assertTrue(retrieve_result["found"])

        # Second call - cache hit
        result2 = self.registry["cache.wrap"](
            state={},
            action="test.large",
            args={},
            key="test_large_cache_key",
        )

        self.assertTrue(result2["success"])
        self.assertTrue(result2["_cache_hit"])  # Should hit cache
        self.assertEqual(result2["result"]["data"], "x" * 200)

    def test_cache_wrap_small_result_inlines(self):
        """
        Verify that small results are still inlined (not using blob storage).
        """

        def small_result_action(state, **kwargs):
            return {"success": True, "msg": "hi"}  # < inline_threshold

        self.registry["test.small"] = small_result_action

        result = self.registry["cache.wrap"](
            state={},
            action="test.small",
            args={},
            key="test_small_cache_key",
        )

        self.assertTrue(result["success"])

        # Verify cache hit on second call
        result2 = self.registry["cache.wrap"](
            state={},
            action="test.small",
            args={},
            key="test_small_cache_key",
        )

        self.assertTrue(result2["_cache_hit"])

    def test_cache_wrap_uses_cache_path_for_blobs(self):
        """
        TEA-FIX-001: Verify blob path uses _cache/ prefix.
        """

        def blob_action(state, **kwargs):
            return {"success": True, "content": "y" * 200}

        self.registry["test.blob"] = blob_action

        result = self.registry["cache.wrap"](
            state={},
            action="test.blob",
            args={},
            key="test_blob_key",
        )

        self.assertTrue(result["success"])

        # Verify blob was created in cache path
        # The blob path should contain _cache/
        cache_files = list(self.ltm._fs.glob(f"{self.temp_dir}/_cache/*.json"))
        self.assertGreater(len(cache_files), 0, "Expected blob in _cache/ directory")


if __name__ == "__main__":
    unittest.main()
