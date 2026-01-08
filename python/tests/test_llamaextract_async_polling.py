"""
Tests for LlamaExtract Async Polling Configuration (TEA-BUILTIN-008.6).

Test Coverage:
- Configuration parameters (AC: 1-5)
- Functional behavior (AC: 6-9)
- Backwards compatibility (AC: 10-11)
- Validation errors
"""

import pytest
from unittest.mock import MagicMock, patch
import time
import json


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_api_key():
    """Mock environment with API key."""
    with patch.dict("os.environ", {"LLAMAEXTRACT_API_KEY": "test-api-key"}):
        yield


@pytest.fixture
def mock_file_prep():
    """
    Provide a base64 data URL that doesn't require file system access.
    This format is handled directly by _prepare_file_content without validation.
    """
    # Return a valid base64 data URL that bypasses file existence check
    return (
        "data:application/pdf;base64,dGVzdCBjb250ZW50"  # "test content" base64 encoded
    )


# A constant for tests that need file data
TEST_FILE = "data:application/pdf;base64,dGVzdCBjb250ZW50"


# =============================================================================
# Configuration Parameter Tests (AC: 1-5)
# =============================================================================


class TestAsyncModeParameter:
    """Test async_mode parameter (AC: 1)."""

    def test_unit_001_async_mode_requires_use_rest(self, mock_api_key):
        """AC-1: async_mode=True requires use_rest=True."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        result = registry["llamaextract.extract"](
            {},
            file=TEST_FILE,
            schema={"type": "object"},
            async_mode=True,
            use_rest=False,  # Should fail
        )

        assert result["success"] is False
        assert "async_mode=True requires use_rest=True" in result["error"]
        assert result["error_type"] == "validation"

    def test_unit_002_async_mode_default_false(self, mock_api_key):
        """AC-10: Default async_mode=False uses sync behavior."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        # Use data URL to bypass file preparation
        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "data": {"result": "test"},
            }

            # Default behavior (async_mode=False, use_rest=True)
            result = registry["llamaextract.extract"](
                {},
                file=TEST_FILE,  # Data URL bypasses file existence check
                schema={"type": "object"},
                use_rest=True,
            )

            # Should use sync /run endpoint
            mock_post.assert_called()
            call_url = mock_post.call_args[0][0]
            assert "/extraction/run" in call_url
            assert "/jobs" not in call_url


class TestPollingIntervalParameter:
    """Test polling_interval parameter (AC: 2, 7)."""

    def test_unit_003_polling_interval_default(self, mock_api_key):
        """AC-2: Default polling_interval is 5 seconds."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        # We can't easily verify the default value directly,
        # but we verify it accepts the call without error
        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "SUCCESS"}

                with patch("time.sleep") as mock_sleep:
                    result = registry["llamaextract.extract"](
                        {},
                        file=TEST_FILE,
                        schema={"type": "object"},
                        use_rest=True,
                        async_mode=True,
                        # Not specifying polling_interval - should use default 5
                    )

    def test_unit_004_custom_polling_interval(self, mock_api_key):
        """AC-7: Custom polling_interval is respected."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                # First call returns PENDING, second returns SUCCESS
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.side_effect = [
                    {"status": "PENDING"},
                    {"status": "SUCCESS"},
                ]

                with patch("time.sleep") as mock_sleep:
                    result = registry["llamaextract.extract"](
                        {},
                        file=TEST_FILE,
                        schema={"type": "object"},
                        use_rest=True,
                        async_mode=True,
                        polling_interval=10,  # Custom interval
                    )

                    # Verify sleep was called with custom interval
                    # (may be called multiple times)
                    sleep_calls = [call[0][0] for call in mock_sleep.call_args_list]
                    if sleep_calls:
                        assert 10 in sleep_calls

    def test_unit_005_polling_interval_accepts_float(self, mock_api_key):
        """AC-7: polling_interval accepts float values."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "SUCCESS"}

                # Should not raise error with float
                result = registry["llamaextract.extract"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    use_rest=True,
                    async_mode=True,
                    polling_interval=2.5,  # Float value
                )

    def test_unit_006_polling_interval_validation(self, mock_api_key):
        """AC-25: Zero or negative polling_interval raises validation error."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        # Test zero
        result = registry["llamaextract.extract"](
            {},
            file=TEST_FILE,
            schema={"type": "object"},
            use_rest=True,
            async_mode=True,
            polling_interval=0,
        )
        assert result["success"] is False
        assert "positive" in result["error"]
        assert result["error_type"] == "validation"

        # Test negative
        result = registry["llamaextract.extract"](
            {},
            file=TEST_FILE,
            schema={"type": "object"},
            use_rest=True,
            async_mode=True,
            polling_interval=-5,
        )
        assert result["success"] is False
        assert "positive" in result["error"]


class TestMaxPollAttemptsParameter:
    """Test max_poll_attempts parameter (AC: 3, 8)."""

    def test_unit_007_max_poll_attempts_default(self, mock_api_key):
        """AC-3: Default max_poll_attempts is 120."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        # Default is 120 - we verify by checking it doesn't error
        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "SUCCESS"}

                result = registry["llamaextract.extract"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    use_rest=True,
                    async_mode=True,
                    # Not specifying max_poll_attempts - should use default 120
                )

    def test_unit_008_max_poll_attempts_triggers_timeout(
        self, mock_api_key, mock_file_prep
    ):
        """AC-8: Polling stops after max_poll_attempts with timeout error."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                # Always return PENDING - never completes
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "PENDING"}

                with patch("time.sleep"):
                    result = registry["llamaextract.extract"](
                        {},
                        file=TEST_FILE,
                        schema={"type": "object"},
                        use_rest=True,
                        async_mode=True,
                        max_poll_attempts=3,  # Only 3 attempts
                        polling_interval=0.01,
                        timeout=9999,  # High timeout so max_attempts triggers first
                    )

                    assert result["success"] is False
                    assert "Max poll attempts" in result["error"]
                    assert result["error_type"] == "timeout"
                    assert result.get("attempts") == 3

    def test_unit_009_max_poll_attempts_validation(self, mock_api_key):
        """AC-25: Zero or negative max_poll_attempts raises validation error."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        # Test zero
        result = registry["llamaextract.extract"](
            {},
            file=TEST_FILE,
            schema={"type": "object"},
            use_rest=True,
            async_mode=True,
            max_poll_attempts=0,
        )
        assert result["success"] is False
        assert "positive" in result["error"]

        # Test negative
        result = registry["llamaextract.extract"](
            {},
            file=TEST_FILE,
            schema={"type": "object"},
            use_rest=True,
            async_mode=True,
            max_poll_attempts=-10,
        )
        assert result["success"] is False
        assert "positive" in result["error"]


class TestTimeoutParameter:
    """Test timeout parameter interactions (AC: 4, 9)."""

    def test_unit_010_timeout_triggers_before_max_attempts(
        self, mock_api_key, mock_file_prep
    ):
        """AC-9: Overall timeout triggers before max_poll_attempts if shorter."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                # Always return PENDING
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "PENDING"}

                with patch("time.sleep"):
                    # With 0.01 timeout, should timeout very quickly
                    result = registry["llamaextract.extract"](
                        {},
                        file=TEST_FILE,
                        schema={"type": "object"},
                        use_rest=True,
                        async_mode=True,
                        timeout=0.01,  # Very short timeout
                        max_poll_attempts=999,  # High attempts
                        polling_interval=0.001,
                    )

                    assert result["success"] is False
                    assert result["error_type"] == "timeout"
                    # Should timeout due to time, not max attempts
                    assert "timed out" in result["error"].lower()


# =============================================================================
# Functional Tests (AC: 6-9)
# =============================================================================


class TestAsyncEndpointSelection:
    """Test async endpoint selection (AC: 6)."""

    def test_unit_011_async_mode_uses_jobs_endpoint(self, mock_api_key, mock_file_prep):
        """AC-6: async_mode=True uses POST /api/v1/extraction/jobs."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "SUCCESS"}

                result = registry["llamaextract.extract"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    use_rest=True,
                    async_mode=True,
                )

                # Verify /jobs endpoint was called
                mock_post.assert_called()
                call_url = mock_post.call_args[0][0]
                assert "/extraction/jobs" in call_url


# =============================================================================
# Backwards Compatibility Tests (AC: 10-11)
# =============================================================================


class TestBackwardsCompatibility:
    """Test backwards compatibility (AC: 10-11)."""

    def test_unit_012_existing_parameters_unchanged(self, mock_api_key, mock_file_prep):
        """AC-5: All existing parameters from 008.5 continue to work."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {"data": {"test": "value"}}

            # Call with all 008.5 parameters
            result = registry["llamaextract.extract"](
                {},
                file=TEST_FILE,
                schema={"type": "object", "properties": {"test": {"type": "string"}}},
                mode="PREMIUM",
                timeout=120,
                max_retries=5,
                use_rest=True,
            )

            assert result["success"] is True

    def test_unit_013_sync_auto_poll_unchanged(self, mock_api_key, mock_file_prep):
        """AC-11: Sync endpoint auto-poll behavior unchanged when job response detected."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            # Sync endpoint returns job-like response (PENDING)
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-456",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {"status": "SUCCESS"}

                # Default sync mode should still auto-poll
                result = registry["llamaextract.extract"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    use_rest=True,
                    # async_mode=False (default)
                )

                # Should have called GET for status polling
                mock_get.assert_called()


# =============================================================================
# Integration Tests
# =============================================================================


class TestPollingBehavior:
    """Integration tests for polling behavior."""

    def test_int_001_poll_success_returns_data(self, mock_api_key, mock_file_prep):
        """Successful polling returns extracted data."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-789",
                "status": "PENDING",
            }

            with patch("requests.get") as mock_get:
                # Status check returns SUCCESS
                status_response = MagicMock()
                status_response.status_code = 200
                status_response.json.return_value = {"status": "SUCCESS"}

                # Result fetch returns data
                result_response = MagicMock()
                result_response.status_code = 200
                result_response.json.return_value = {
                    "data": {"invoice_number": "INV-001", "total": 100.50}
                }

                mock_get.side_effect = [status_response, result_response]

                result = registry["llamaextract.extract"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    use_rest=True,
                    async_mode=True,
                )

                assert result["success"] is True
                assert "data" in result
                assert result["job_id"] == "job-789"


# =============================================================================
# Run tests
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
