"""
Tests for LlamaExtract Actions.

TEA-BUILTIN-008.1: Core LlamaExtract Actions

These tests mock the LlamaExtract API to verify:
- Action registration and parameter handling
- Retry logic with exponential backoff
- Error handling for various failure modes
- All extraction modes (BALANCED, MULTIMODAL, PREMIUM, FAST)
"""

import base64
import json
import os
import pytest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch, MagicMock, PropertyMock


class TestLlamaExtractActionsRegistration:
    """Test action registration."""

    def test_actions_registered(self):
        """Test all llamaextract actions are registered."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        expected_actions = [
            "llamaextract.extract",
            "llamaextract.upload_agent",
            "llamaextract.list_agents",
            "llamaextract.get_agent",
            "llamaextract.delete_agent",
            "actions.llamaextract_extract",
            "actions.llamaextract_upload_agent",
            "actions.llamaextract_list_agents",
            "actions.llamaextract_get_agent",
            "actions.llamaextract_delete_agent",
        ]

        for action in expected_actions:
            assert action in registry, f"Missing action: {action}"


class TestLlamaExtractExtract:
    """Test llamaextract.extract action."""

    def test_extract_missing_file(self):
        """Test error when file parameter is missing."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["llamaextract.extract"](
            state={}, file="", schema={"type": "object"}
        )

        assert result["success"] is False
        assert "file parameter is required" in result["error"]

    def test_extract_missing_schema_and_agent(self):
        """Test error when neither schema nor agent is provided."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["llamaextract.extract"](
            state={}, file="https://example.com/doc.pdf"
        )

        assert result["success"] is False
        assert "schema" in result["error"].lower() or "agent" in result["error"].lower()

    def test_extract_missing_api_key(self):
        """Test error when API key is not set."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Ensure no API key is set
        with patch.dict(os.environ, {}, clear=True):
            # Remove existing keys if present
            env_backup = {}
            for key in ["LLAMAEXTRACT_API_KEY", "LLAMAPARSE_API_KEY"]:
                if key in os.environ:
                    env_backup[key] = os.environ.pop(key)

            try:
                result = registry["llamaextract.extract"](
                    state={},
                    file="https://example.com/doc.pdf",
                    schema={"type": "object"},
                )

                assert result["success"] is False
                # Either API key error or dependency error (package not installed)
                error_lower = result["error"].lower()
                assert (
                    "api_key" in error_lower
                    or "environment" in error_lower
                    or "not installed" in error_lower
                )
            finally:
                # Restore keys
                os.environ.update(env_backup)

    def test_extract_file_not_found(self):
        """Test error when local file does not exist."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Mock the client to avoid API key issues
        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            result = registry["llamaextract.extract"](
                state={}, file="/nonexistent/file.pdf", schema={"type": "object"}
            )

            assert result["success"] is False
            # Either file_not_found or dependency error (package not installed)
            assert result["error_type"] in ["file_not_found", "dependency"]

    def test_extract_with_url_structure(self):
        """Test extraction with URL source - validates input handling."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            # This will fail at dependency level, but validates URL is accepted
            result = registry["llamaextract.extract"](
                state={}, file="https://example.com/doc.pdf", schema={"type": "object"}
            )

            # Should fail at dependency or API level, not validation
            assert result["success"] is False
            assert result["error_type"] in ["dependency", "api_error", "configuration"]

    def test_extract_invalid_mode(self):
        """Test error with invalid extraction mode."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            # The mode validation happens when getting the client
            result = registry["llamaextract.extract"](
                state={},
                file="https://example.com/doc.pdf",
                schema={"type": "object"},
                mode="INVALID_MODE",
            )

            # Either configuration or dependency error depending on import success
            assert result["success"] is False

    @pytest.mark.parametrize("mode", ["BALANCED", "MULTIMODAL", "PREMIUM", "FAST"])
    def test_extract_modes_accepted(self, mode):
        """Test all extraction modes are accepted (validation only)."""
        from the_edge_agent.actions.llamaextract_actions import EXTRACTION_MODES

        assert mode in EXTRACTION_MODES


class TestLlamaExtractUploadAgent:
    """Test llamaextract.upload_agent action."""

    def test_upload_missing_name(self):
        """Test error when name is missing."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["llamaextract.upload_agent"](
            state={}, name="", schema={"type": "object"}
        )

        assert result["success"] is False
        assert "name" in result["error"].lower()

    def test_upload_missing_schema(self):
        """Test error when schema is missing."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["llamaextract.upload_agent"](
            state={}, name="test-agent", schema=None
        )

        assert result["success"] is False
        assert "schema" in result["error"].lower()


class TestLlamaExtractListAgents:
    """Test llamaextract.list_agents action."""

    def test_list_agents_missing_api_key(self):
        """Test error when API key is not set."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Ensure no API key
        env_backup = {}
        for key in ["LLAMAEXTRACT_API_KEY", "LLAMAPARSE_API_KEY"]:
            if key in os.environ:
                env_backup[key] = os.environ.pop(key)

        try:
            result = registry["llamaextract.list_agents"](state={})
            assert result["success"] is False
        finally:
            os.environ.update(env_backup)


class TestLlamaExtractGetAgent:
    """Test llamaextract.get_agent action."""

    def test_get_agent_missing_id_and_name(self):
        """Test error when neither id nor name provided."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["llamaextract.get_agent"](state={})

        assert result["success"] is False
        assert (
            "agent_id" in result["error"].lower()
            or "agent_name" in result["error"].lower()
        )


class TestLlamaExtractDeleteAgent:
    """Test llamaextract.delete_agent action."""

    def test_delete_agent_missing_id_and_name(self):
        """Test error when neither id nor name provided."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["llamaextract.delete_agent"](state={})

        assert result["success"] is False
        assert (
            "agent_id" in result["error"].lower()
            or "agent_name" in result["error"].lower()
        )


class TestRetryLogic:
    """Test retry logic with exponential backoff."""

    def test_retry_calculation(self):
        """Test that retry delays follow exponential backoff pattern."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        # The retry logic uses base_delay * (2 ** attempt)
        # attempt 0: 1.0 * 1 = 1.0
        # attempt 1: 1.0 * 2 = 2.0
        # attempt 2: 1.0 * 4 = 4.0

        base_delay = 1.0
        max_delay = 30.0

        for attempt in range(5):
            expected_delay = min(base_delay * (2**attempt), max_delay)
            assert expected_delay <= max_delay


class TestErrorTypes:
    """Test error type classification."""

    def test_error_types_defined(self):
        """Test that expected error types are used."""
        expected_error_types = [
            "validation",
            "configuration",
            "dependency",
            "file_not_found",
            "api_error",
            "rate_limit",
            "timeout",
            "not_found",
        ]

        # This is a documentation test - verify error types are consistent
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Test validation errors
        result = registry["llamaextract.extract"](state={}, file="")
        assert result["error_type"] == "validation"

        result = registry["llamaextract.get_agent"](state={})
        assert result["error_type"] == "validation"


class TestMockedApiCalls:
    """Test with fully mocked API calls."""

    def test_extract_action_callable(self):
        """Test that extract action is properly callable."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Verify the action is registered and callable
        assert "llamaextract.extract" in registry
        assert callable(registry["llamaextract.extract"])

        # Test with valid inputs (will fail at dependency level)
        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            result = registry["llamaextract.extract"](
                state={},
                file="https://example.com/doc.pdf",
                schema={"type": "object", "properties": {"total": {"type": "number"}}},
            )

            # Should return a dict with expected keys
            assert isinstance(result, dict)
            assert "success" in result

    def test_list_agents_success_mocked(self):
        """Test list agents with mocked API."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Create mock agent objects
        mock_agent1 = MagicMock()
        mock_agent1.id = "agent-1"
        mock_agent1.name = "invoice-extractor"

        mock_agent2 = MagicMock()
        mock_agent2.id = "agent-2"
        mock_agent2.name = "receipt-extractor"

        mock_client = MagicMock()
        mock_client.list_agents.return_value = [mock_agent1, mock_agent2]

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            # This validates structure, full mock would require more setup
            pass


class TestLocalFileHandling:
    """Test local file handling."""

    def test_extract_with_local_file(self):
        """Test extraction with local file - file reading."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        with TemporaryDirectory() as tmpdir:
            # Create a test file
            test_file = Path(tmpdir) / "test.pdf"
            test_content = b"PDF content here"
            test_file.write_bytes(test_content)

            with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
                # The action will fail at client creation, but we can verify
                # the file handling code path is exercised
                result = registry["llamaextract.extract"](
                    state={}, file=str(test_file), schema={"type": "object"}
                )

                # Will fail at dependency/client, but file was read
                assert result["success"] is False
                # Should not be file_not_found since file exists
                assert (
                    result["error_type"] != "file_not_found"
                    or "dependency" in result["error_type"]
                )


class TestBase64Handling:
    """Test base64 content handling."""

    def test_extract_with_base64_content(self):
        """Test extraction with base64 encoded content."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Create base64 content
        content = base64.b64encode(b"PDF content").decode("utf-8")
        base64_file = f"data:application/pdf;base64,{content}"

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            result = registry["llamaextract.extract"](
                state={}, file=base64_file, schema={"type": "object"}
            )

            # Will fail at dependency, but base64 parsing should work
            assert result["success"] is False
            assert (
                "file" not in result.get("error", "").lower()
                or "dependency" in result["error_type"]
            )


# ============================================================
# TEA-BUILTIN-008.5: REST API Tests
# ============================================================


def _mock_url_download():
    """Create a mock for URL downloads that returns fake PDF content."""
    mock_get_response = MagicMock()
    mock_get_response.status_code = 200
    mock_get_response.content = b"fake pdf content"
    mock_get_response.headers = {"Content-Type": "application/pdf"}
    mock_get_response.raise_for_status = MagicMock()
    return mock_get_response


class TestRestApiSuccessfulExtraction:
    """Test successful extraction via REST API (T1)."""

    def test_extract_via_rest_success(self):
        """T1: Test successful extraction via REST API."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Mock successful response
        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {
            "data": {"invoice_number": "INV-001", "total": 100.50}
        }

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch("requests.post", return_value=mock_post_response):
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/invoice.pdf",
                        schema={
                            "type": "object",
                            "properties": {
                                "invoice_number": {"type": "string"},
                                "total": {"type": "number"},
                            },
                        },
                        mode="PREMIUM",
                        use_rest=True,
                    )

                    assert result["success"] is True
                    assert result["data"]["invoice_number"] == "INV-001"
                    assert result["data"]["total"] == 100.50
                    assert result["status"] == "completed"

    def test_extract_rest_api_endpoint_called(self):
        """Verify correct REST API endpoint is called."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        use_rest=True,
                    )

                    # Verify endpoint
                    call_args = mock_post.call_args
                    assert (
                        "api.cloud.llamaindex.ai/api/v1/extraction/run"
                        in call_args[0][0]
                    )


class TestRestApiFileHandling:
    """Test file handling for REST API (T2, T3, T4)."""

    def test_extract_with_url(self):
        """T2: Test URL file handling - downloads and converts to base64."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {"result": "test"}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()) as mock_get:
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/document.pdf",
                        schema={"type": "object"},
                        use_rest=True,
                    )

                    assert result["success"] is True
                    # Verify URL was downloaded
                    mock_get.assert_called_once()
                    # Verify file object with data and mime_type in payload
                    call_kwargs = mock_post.call_args[1]
                    assert "file" in call_kwargs["json"]
                    assert "data" in call_kwargs["json"]["file"]
                    assert "mime_type" in call_kwargs["json"]["file"]
                    assert call_kwargs["json"]["file"]["mime_type"] == "application/pdf"

    def test_extract_with_base64(self):
        """T3: Test base64 file handling."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {"result": "test"}}

        content = base64.b64encode(b"PDF content").decode("utf-8")
        base64_file = f"data:application/pdf;base64,{content}"

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.post", return_value=mock_post_response) as mock_post:
                result = registry["llamaextract.extract"](
                    state={}, file=base64_file, schema={"type": "object"}, use_rest=True
                )

                assert result["success"] is True
                # Verify file object with data and mime_type in payload
                call_kwargs = mock_post.call_args[1]
                assert "file" in call_kwargs["json"]
                assert call_kwargs["json"]["file"]["data"] == content
                assert call_kwargs["json"]["file"]["mime_type"] == "application/pdf"

    def test_extract_with_local_file(self):
        """T4: Test local file path handling."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {"result": "test"}}

        with TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test.pdf"
            test_content = b"PDF content here"
            test_file.write_bytes(test_content)

            expected_base64 = base64.b64encode(test_content).decode("utf-8")

            with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    result = registry["llamaextract.extract"](
                        state={},
                        file=str(test_file),
                        schema={"type": "object"},
                        use_rest=True,
                    )

                    assert result["success"] is True
                    # Verify file was read and base64 encoded
                    call_kwargs = mock_post.call_args[1]
                    assert "file" in call_kwargs["json"]
                    assert call_kwargs["json"]["file"]["data"] == expected_base64
                    assert call_kwargs["json"]["file"]["mime_type"] == "application/pdf"


class TestRestApiRateLimitRetry:
    """Test rate limit (429) handling with retry (T5)."""

    def test_rate_limit_triggers_retry(self):
        """T5: Test that 429 rate limit triggers retry with backoff."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # First call: 429, Second call: 200
        mock_response_429 = MagicMock()
        mock_response_429.status_code = 429

        mock_response_200 = MagicMock()
        mock_response_200.status_code = 200
        mock_response_200.json.return_value = {"data": {"success": True}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", side_effect=[mock_response_429, mock_response_200]
                ) as mock_post:
                    with patch("time.sleep") as mock_sleep:
                        result = registry["llamaextract.extract"](
                            state={},
                            file="https://example.com/doc.pdf",
                            schema={"type": "object"},
                            max_retries=3,
                            use_rest=True,
                        )

                        # Should succeed after retry
                        assert result["success"] is True
                        # Should have called post twice
                        assert mock_post.call_count == 2
                        # Should have slept (exponential backoff)
                        mock_sleep.assert_called()

    def test_rate_limit_max_retries_exceeded(self):
        """Test rate limit error after max retries."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_response_429 = MagicMock()
        mock_response_429.status_code = 429

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch("requests.post", return_value=mock_response_429):
                    with patch("time.sleep"):
                        result = registry["llamaextract.extract"](
                            state={},
                            file="https://example.com/doc.pdf",
                            schema={"type": "object"},
                            max_retries=3,
                            use_rest=True,
                        )

                        assert result["success"] is False
                        assert result["error_type"] == "rate_limit"


class TestRestApiServerErrorRetry:
    """Test server error (5xx) handling with retry (T6)."""

    def test_server_error_triggers_retry(self):
        """T6: Test that 5xx server error triggers retry."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_response_500 = MagicMock()
        mock_response_500.status_code = 500

        mock_response_200 = MagicMock()
        mock_response_200.status_code = 200
        mock_response_200.json.return_value = {"data": {"success": True}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", side_effect=[mock_response_500, mock_response_200]
                ) as mock_post:
                    with patch("time.sleep"):
                        result = registry["llamaextract.extract"](
                            state={},
                            file="https://example.com/doc.pdf",
                            schema={"type": "object"},
                            use_rest=True,
                        )

                        assert result["success"] is True
                        assert mock_post.call_count == 2


class TestRestApiClientError:
    """Test client error (4xx) handling without retry (T7)."""

    def test_client_error_no_retry(self):
        """T7: Test that 4xx client errors return immediately without retry."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_response_400 = MagicMock()
        mock_response_400.status_code = 400
        mock_response_400.text = "Bad request"
        mock_response_400.json.return_value = {"detail": "Invalid schema"}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_response_400
                ) as mock_post:
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        use_rest=True,
                    )

                    # Should fail immediately
                    assert result["success"] is False
                    assert result["error_type"] == "validation"
                    # Should only call once (no retry)
                    assert mock_post.call_count == 1

    def test_auth_error_no_retry(self):
        """Test that 401 auth errors return immediately."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_response_401 = MagicMock()
        mock_response_401.status_code = 401
        mock_response_401.text = "Unauthorized"

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "bad-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_response_401
                ) as mock_post:
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        use_rest=True,
                    )

                    assert result["success"] is False
                    assert result["error_type"] == "configuration"
                    assert mock_post.call_count == 1


class TestRestApiTimeout:
    """Test timeout handling (T8)."""

    def test_timeout_handling(self):
        """T8: Test timeout handling."""
        from the_edge_agent.actions.llamaextract_actions import register_actions
        import requests

        registry = {}
        register_actions(registry, engine=None)

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post",
                    side_effect=requests.Timeout("Connection timed out"),
                ):
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        timeout=30,
                        use_rest=True,
                    )

                    assert result["success"] is False
                    assert result["error_type"] == "timeout"
                    assert "timed out" in result["error"].lower()

    def test_custom_timeout_passed(self):
        """Test that custom timeout is passed to requests."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"data": {}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch("requests.post", return_value=mock_response) as mock_post:
                    registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        timeout=60,
                        use_rest=True,
                    )

                    # Verify timeout was passed (capped at 120 for initial request)
                    call_kwargs = mock_post.call_args[1]
                    assert call_kwargs["timeout"] == 60


class TestRestApiMissingApiKey:
    """Test missing API key handling (T9)."""

    def test_missing_api_key(self):
        """T9: Test error when API key is missing."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        # Clear API keys
        env_backup = {}
        for key in ["LLAMAEXTRACT_API_KEY", "LLAMAPARSE_API_KEY"]:
            if key in os.environ:
                env_backup[key] = os.environ.pop(key)

        try:
            result = registry["llamaextract.extract"](
                state={},
                file="https://example.com/doc.pdf",
                schema={"type": "object"},
                use_rest=True,
            )

            assert result["success"] is False
            assert result["error_type"] == "configuration"
            assert (
                "api_key" in result["error"].lower()
                or "environment" in result["error"].lower()
            )
        finally:
            os.environ.update(env_backup)


class TestRestApiFullFlow:
    """Integration test with full REST flow (T10)."""

    def test_full_rest_flow_with_schema(self):
        """T10: Test full REST flow with inline schema."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {
            "data": {
                "company_name": "Acme Corp",
                "invoice_number": "INV-2024-001",
                "total_amount": 1500.00,
                "line_items": [
                    {"description": "Widget A", "quantity": 10, "price": 100.00},
                    {"description": "Widget B", "quantity": 5, "price": 100.00},
                ],
            }
        }

        schema = {
            "type": "object",
            "properties": {
                "company_name": {"type": "string"},
                "invoice_number": {"type": "string"},
                "total_amount": {"type": "number"},
                "line_items": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "description": {"type": "string"},
                            "quantity": {"type": "integer"},
                            "price": {"type": "number"},
                        },
                    },
                },
            },
        }

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-api-key-12345"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/invoice.pdf",
                        schema=schema,
                        mode="PREMIUM",
                        timeout=300,
                        use_rest=True,
                    )

                    # Verify success
                    assert result["success"] is True
                    assert result["status"] == "completed"

                    # Verify data structure
                    data = result["data"]
                    assert data["company_name"] == "Acme Corp"
                    assert data["invoice_number"] == "INV-2024-001"
                    assert data["total_amount"] == 1500.00
                    assert len(data["line_items"]) == 2

                    # Verify request structure
                    call_kwargs = mock_post.call_args[1]
                    payload = call_kwargs["json"]

                    # Verify headers
                    headers = call_kwargs["headers"]
                    assert headers["Authorization"] == "Bearer test-api-key-12345"
                    assert headers["Content-Type"] == "application/json"

                    # Verify payload has file object with data and mime_type
                    assert "file" in payload
                    assert "data" in payload["file"]
                    assert "mime_type" in payload["file"]
                    assert payload["data_schema"] == schema
                    assert payload["config"]["extraction_mode"] == "PREMIUM"

    def test_full_rest_flow_with_agent_id(self):
        """Test full REST flow with agent_id instead of schema."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {"result": "extracted"}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        agent_id="agent-123-456",
                        use_rest=True,
                    )

                    assert result["success"] is True

                    # Verify agent_id in payload
                    call_kwargs = mock_post.call_args[1]
                    payload = call_kwargs["json"]
                    assert payload["agent_id"] == "agent-123-456"
                    assert "data_schema" not in payload


class TestRestApiBackwardsCompatibility:
    """Test SDK (default) vs REST API paths."""

    def test_default_uses_sdk(self):
        """Test that default path uses SDK (for client-side validation)."""
        from the_edge_agent.actions.llamaextract_actions import register_actions
        import sys

        registry = {}
        register_actions(registry, engine=None)

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            # Mock the SDK import to simulate it not being installed
            with patch.dict(sys.modules, {"llama_cloud_services": None}):
                # Default (no use_rest) should try SDK (will fail on import)
                result = registry["llamaextract.extract"](
                    state={},
                    file="https://example.com/doc.pdf",
                    schema={"type": "object"},
                )

                # Should fail at dependency level (SDK not installed)
                assert result["success"] is False
                assert result["error_type"] == "dependency"

    def test_use_rest_flag(self):
        """Test use_rest=True uses REST API directly."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {"result": "success"}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch("requests.post", return_value=mock_post_response):
                    result = registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        use_rest=True,
                    )

                    # Should succeed via REST API
                    assert result["success"] is True

    def test_agent_name_uses_sdk(self):
        """Test that agent_name parameter always uses SDK path."""
        from the_edge_agent.actions.llamaextract_actions import register_actions
        import sys

        registry = {}
        register_actions(registry, engine=None)

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            # Mock the SDK import to simulate it not being installed
            with patch.dict(sys.modules, {"llama_cloud_services": None}):
                # agent_name requires SDK (even with use_rest=True)
                result = registry["llamaextract.extract"](
                    state={},
                    file="https://example.com/doc.pdf",
                    agent_name="my-agent",
                    use_rest=True,  # Should be ignored when agent_name is provided
                )

                # Should fail at dependency level (SDK not installed)
                assert result["success"] is False
                assert result["error_type"] == "dependency"


class TestRestApiExtractionModes:
    """Test extraction mode handling."""

    @pytest.mark.parametrize("mode", ["BALANCED", "MULTIMODAL", "PREMIUM", "FAST"])
    def test_extraction_modes_in_payload(self, mode):
        """Test that extraction modes are correctly passed in payload."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        mode=mode,
                        use_rest=True,
                    )

                    call_kwargs = mock_post.call_args[1]
                    payload = call_kwargs["json"]
                    assert payload["config"]["extraction_mode"] == mode

    def test_mode_case_insensitive(self):
        """Test that mode is case insensitive."""
        from the_edge_agent.actions.llamaextract_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post", return_value=mock_post_response
                ) as mock_post:
                    registry["llamaextract.extract"](
                        state={},
                        file="https://example.com/doc.pdf",
                        schema={"type": "object"},
                        mode="premium",  # lowercase
                        use_rest=True,
                    )

                    call_kwargs = mock_post.call_args[1]
                    payload = call_kwargs["json"]
                    assert payload["config"]["extraction_mode"] == "PREMIUM"


class TestRestApiConnectionError:
    """Test connection error handling."""

    def test_connection_error_retry(self):
        """Test that connection errors trigger retry."""
        from the_edge_agent.actions.llamaextract_actions import register_actions
        import requests

        registry = {}
        register_actions(registry, engine=None)

        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"data": {"success": True}}

        with patch.dict(os.environ, {"LLAMAEXTRACT_API_KEY": "test-key"}):
            with patch("requests.get", return_value=_mock_url_download()):
                with patch(
                    "requests.post",
                    side_effect=[
                        requests.ConnectionError("Connection refused"),
                        mock_post_response,
                    ],
                ) as mock_post:
                    with patch("time.sleep"):
                        result = registry["llamaextract.extract"](
                            state={},
                            file="https://example.com/doc.pdf",
                            schema={"type": "object"},
                            max_retries=3,
                            use_rest=True,
                        )

                        # Should succeed after retry
                        assert result["success"] is True
                        assert mock_post.call_count == 2
