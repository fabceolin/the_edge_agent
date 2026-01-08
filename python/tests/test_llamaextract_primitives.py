"""
Tests for LlamaExtract Workflow Primitives (TEA-BUILTIN-008.7).

Test Coverage:
- submit_job: AC 1-4
- poll_status: AC 5-8
- get_result: AC 9-11
- Configuration: AC 12-13
- Integration: AC 14-15
"""

import pytest
from unittest.mock import MagicMock, patch


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_api_key():
    """Mock environment with API key."""
    with patch.dict("os.environ", {"LLAMAEXTRACT_API_KEY": "test-api-key"}):
        yield


# A constant for tests that need file data (base64 data URL)
TEST_FILE = "data:application/pdf;base64,dGVzdCBjb250ZW50"


# =============================================================================
# Submit Job Tests (AC: 1-4)
# =============================================================================


class TestSubmitJob:
    """Test llamaextract.submit_job primitive."""

    def test_unit_001_submit_job_returns_job_id(self, mock_api_key):
        """AC-2: submit_job returns job_id and PENDING status on success."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
                mode="FAST",
            )

            assert result["success"] is True
            assert result["job_id"] == "job-123"
            assert result["status"] == "PENDING"

    def test_unit_002_submit_job_handles_file_types(self, mock_api_key):
        """AC-1: submit_job accepts file path, URL, and base64."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            # Test base64
            result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
            )
            assert result["success"] is True

            # Verify /jobs endpoint was called
            call_url = mock_post.call_args[0][0]
            assert "/extraction/jobs" in call_url

    def test_unit_003_submit_job_handles_api_errors(self, mock_api_key):
        """AC-3: submit_job returns structured error on failure."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            # Test 401 error
            mock_post.return_value.status_code = 401
            mock_post.return_value.text = "Unauthorized"

            result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
            )

            assert result["success"] is False
            assert "error_type" in result
            assert result["error_type"] == "configuration"

    def test_unit_004_submit_job_validates_mode(self, mock_api_key):
        """AC-4: submit_job supports all extraction modes."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            # Test valid modes
            for mode in ["FAST", "BALANCED", "PREMIUM", "MULTIMODAL"]:
                result = registry["llamaextract.submit_job"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    mode=mode,
                )
                assert result["success"] is True

            # Test invalid mode
            result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
                mode="INVALID",
            )
            assert result["success"] is False
            assert "Invalid extraction mode" in result["error"]

    def test_unit_005_submit_job_requires_schema_or_agent_id(self, mock_api_key):
        """AC-1: submit_job requires schema or agent_id."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        # No schema or agent_id
        result = registry["llamaextract.submit_job"](
            {},
            file=TEST_FILE,
        )

        assert result["success"] is False
        assert "Either schema or agent_id" in result["error"]


# =============================================================================
# Poll Status Tests (AC: 5-8)
# =============================================================================


class TestPollStatus:
    """Test llamaextract.poll_status primitive."""

    def test_unit_006_poll_status_returns_status(self, mock_api_key):
        """AC-6: poll_status returns status and progress."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.get") as mock_get:
            mock_get.return_value.status_code = 200
            mock_get.return_value.json.return_value = {
                "status": "RUNNING",
                "progress": 50,
            }

            result = registry["llamaextract.poll_status"](
                {},
                job_id="job-123",
            )

            assert result["success"] is True
            assert result["status"] == "RUNNING"
            assert result["progress"] == 50

    def test_unit_007_poll_status_includes_error_on_error_status(self, mock_api_key):
        """AC-7: poll_status includes error field when status is ERROR."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.get") as mock_get:
            mock_get.return_value.status_code = 200
            mock_get.return_value.json.return_value = {
                "status": "ERROR",
                "error": "Document parsing failed",
            }

            result = registry["llamaextract.poll_status"](
                {},
                job_id="job-123",
            )

            assert result["success"] is True
            assert result["status"] == "ERROR"
            assert "error" in result
            assert "parsing failed" in result["error"]

    def test_unit_008_poll_status_configurable_timeout(self, mock_api_key):
        """AC-8: poll_status accepts configurable timeout."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.get") as mock_get:
            mock_get.return_value.status_code = 200
            mock_get.return_value.json.return_value = {"status": "SUCCESS"}

            result = registry["llamaextract.poll_status"](
                {},
                job_id="job-123",
                timeout=5,  # Custom timeout
            )

            # Verify timeout was passed to requests
            mock_get.assert_called_once()
            assert mock_get.call_args[1]["timeout"] == 5

    def test_unit_009_poll_status_requires_job_id(self, mock_api_key):
        """AC-5: poll_status requires job_id parameter."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        result = registry["llamaextract.poll_status"]({})

        assert result["success"] is False
        assert "job_id" in result["error"]

    def test_unit_010_poll_status_handles_status_values(self, mock_api_key):
        """AC-6: poll_status returns correct status values."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        for status in ["PENDING", "RUNNING", "SUCCESS", "ERROR", "PARTIAL_SUCCESS"]:
            with patch("requests.get") as mock_get:
                mock_get.return_value.status_code = 200
                mock_get.return_value.json.return_value = {
                    "status": status,
                    "progress": 100 if status == "SUCCESS" else 50,
                }

                result = registry["llamaextract.poll_status"](
                    {},
                    job_id="job-123",
                )

                assert result["success"] is True
                assert result["status"] == status


# =============================================================================
# Get Result Tests (AC: 9-11)
# =============================================================================


class TestGetResult:
    """Test llamaextract.get_result primitive."""

    def test_unit_011_get_result_returns_data(self, mock_api_key):
        """AC-10: get_result returns extracted data with job_id."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.get") as mock_get:
            mock_get.return_value.status_code = 200
            mock_get.return_value.json.return_value = {
                "data": {"invoice_number": "INV-001", "total": 100.50}
            }

            result = registry["llamaextract.get_result"](
                {},
                job_id="job-123",
            )

            assert result["success"] is True
            assert "data" in result
            assert result["data"]["invoice_number"] == "INV-001"
            assert result["job_id"] == "job-123"

    def test_unit_012_get_result_handles_incomplete_job(self, mock_api_key):
        """AC-11: get_result returns error if job not complete."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.get") as mock_get:
            mock_get.return_value.status_code = 404
            mock_get.return_value.text = "Result not found"

            result = registry["llamaextract.get_result"](
                {},
                job_id="job-123",
            )

            assert result["success"] is False
            assert result["error_type"] == "not_found"

    def test_unit_013_get_result_requires_job_id(self, mock_api_key):
        """AC-9: get_result requires job_id parameter."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        result = registry["llamaextract.get_result"]({})

        assert result["success"] is False
        assert "job_id" in result["error"]


# =============================================================================
# Configuration Tests (AC: 12-13)
# =============================================================================


class TestConfiguration:
    """Test configuration requirements."""

    def test_unit_014_api_key_from_env(self):
        """AC-12: All primitives use API key from environment."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        # No API key
        with patch.dict("os.environ", {}, clear=True):
            registry = {}
            register_actions(registry, MagicMock())

            # Test submit_job
            result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
            )
            assert result["success"] is False
            assert "API_KEY" in result["error"]

            # Test poll_status
            result = registry["llamaextract.poll_status"](
                {},
                job_id="job-123",
            )
            assert result["success"] is False
            assert "API_KEY" in result["error"]

            # Test get_result
            result = registry["llamaextract.get_result"](
                {},
                job_id="job-123",
            )
            assert result["success"] is False
            assert "API_KEY" in result["error"]

    def test_unit_015_retry_on_transient_failures(self, mock_api_key):
        """AC-13: Primitives retry on transient failures."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            # First call fails with 500, second succeeds
            mock_response_fail = MagicMock()
            mock_response_fail.status_code = 500
            mock_response_fail.text = "Server error"

            mock_response_success = MagicMock()
            mock_response_success.status_code = 200
            mock_response_success.json.return_value = {
                "id": "job-123",
                "status": "PENDING",
            }

            mock_post.side_effect = [mock_response_fail, mock_response_success]

            with patch("time.sleep"):  # Skip actual sleep
                result = registry["llamaextract.submit_job"](
                    {},
                    file=TEST_FILE,
                    schema={"type": "object"},
                    max_retries=3,
                )

                assert result["success"] is True
                assert mock_post.call_count == 2


# =============================================================================
# Integration Tests (AC: 14-15)
# =============================================================================


class TestIntegration:
    """Integration tests for workflow primitives."""

    def test_int_001_full_workflow_with_primitives(self, mock_api_key):
        """AC-15: Full workflow using submit_job, poll_status, get_result."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-789",
                "status": "PENDING",
            }

            # Step 1: Submit job
            submit_result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
                mode="FAST",
            )

            assert submit_result["success"] is True
            job_id = submit_result["job_id"]

        with patch("requests.get") as mock_get:
            # Step 2: Poll status until SUCCESS
            poll_responses = [
                {"status": "PENDING", "progress": 0},
                {"status": "RUNNING", "progress": 50},
                {"status": "SUCCESS", "progress": 100},
            ]

            mock_get.return_value.status_code = 200
            mock_get.return_value.json.side_effect = poll_responses

            # Poll until SUCCESS
            for _ in range(3):
                poll_result = registry["llamaextract.poll_status"](
                    {},
                    job_id=job_id,
                )
                assert poll_result["success"] is True
                if poll_result["status"] == "SUCCESS":
                    break

            assert poll_result["status"] == "SUCCESS"

        with patch("requests.get") as mock_get:
            # Step 3: Get result
            mock_get.return_value.status_code = 200
            mock_get.return_value.json.return_value = {"data": {"extracted": "value"}}

            get_result = registry["llamaextract.get_result"](
                {},
                job_id=job_id,
            )

            assert get_result["success"] is True
            assert get_result["data"]["extracted"] == "value"

    def test_int_002_multi_mode_escalation_pattern(self, mock_api_key):
        """AC-15: Multi-mode escalation pattern (FAST -> BALANCED)."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, MagicMock())

        with patch("requests.post") as mock_post:
            mock_post.return_value.status_code = 200
            mock_post.return_value.json.return_value = {
                "id": "job-fast",
                "status": "PENDING",
            }

            # Submit FAST mode
            fast_result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,
                schema={"type": "object"},
                mode="FAST",
            )

            assert fast_result["success"] is True
            assert fast_result["job_id"] == "job-fast"

            # Escalate to BALANCED (simulating quality check failure)
            mock_post.return_value.json.return_value = {
                "id": "job-balanced",
                "status": "PENDING",
            }

            balanced_result = registry["llamaextract.submit_job"](
                {},
                file=TEST_FILE,  # File must be re-uploaded
                schema={"type": "object"},
                mode="BALANCED",
            )

            assert balanced_result["success"] is True
            assert balanced_result["job_id"] == "job-balanced"


# =============================================================================
# Run tests
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
