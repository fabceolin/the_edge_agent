"""
Tests for web.ai_scrape action (TEA-BUILTIN-008.4: ScrapeGraphAI Integration).

This test suite covers:
- API key configuration
- Inline schema extraction
- Git schema loading (via Story 008.2)
- fsspec URI schema loading (s3://, gs://, az://)
- Schema merging from multiple sources (via Story 008.3)
- Retry logic with exponential backoff
- Error handling (missing API key, invalid schema, network errors)
- Pydantic model generation from JSON Schema
"""

import pytest
import os
import sys
from unittest.mock import Mock, patch, MagicMock

# Create mock for scrapegraph_py before any imports
mock_scrapegraph_module = MagicMock()
mock_client_class = MagicMock()
mock_scrapegraph_module.Client = mock_client_class
sys.modules["scrapegraph_py"] = mock_scrapegraph_module


# Helper to get the web_ai_scrape function from the registry
def get_web_ai_scrape():
    """Get web_ai_scrape action from the actions registry."""
    from the_edge_agent.actions import web_actions

    registry = {}
    web_actions.register_actions(registry, None)
    return registry["web.ai_scrape"]


class TestWebAiScrapeConfiguration:
    """Tests for API key configuration."""

    def test_missing_api_key_returns_error(self):
        """Should return configuration error when API key missing."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {}, clear=True):
            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract data",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
            )

            assert result["success"] is False
            assert result["error_type"] == "configuration"
            assert "SCRAPEGRAPH_API_KEY" in result["error"]

    def test_missing_scrapegraph_package_returns_error(self):
        """Should return dependency error when scrapegraph-py not installed."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            # Mock import failure
            with patch.dict("sys.modules", {"scrapegraph_py": None}):
                # Force ImportError by making the import fail
                import builtins

                original_import = builtins.__import__

                def mock_import(name, *args, **kwargs):
                    if name == "scrapegraph_py":
                        raise ImportError("No module named 'scrapegraph_py'")
                    return original_import(name, *args, **kwargs)

                with patch.object(builtins, "__import__", mock_import):
                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract data",
                        output_schema={
                            "type": "object",
                            "properties": {"name": {"type": "string"}},
                        },
                    )

                    assert result["success"] is False
                    assert result["error_type"] == "dependency"
                    assert "scrapegraph-py" in result["error"]


class TestWebAiScrapeInlineSchema:
    """Tests for inline schema extraction."""

    def test_inline_schema_extraction_success(self):
        """Should extract data using inline output_schema."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {
                "name": "Test Product",
                "price": "$99.99",
            }
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com/product",
                prompt="Extract product info",
                output_schema={
                    "type": "object",
                    "properties": {
                        "name": {"type": "string"},
                        "price": {"type": "string"},
                    },
                },
            )

            assert result["success"] is True
            assert result["data"]["name"] == "Test Product"
            assert result["data"]["price"] == "$99.99"
            assert result["url"] == "https://example.com/product"
            assert "schema_used" in result

    def test_inline_schema_with_nested_objects(self):
        """Should handle nested object schemas."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {
                "company": {
                    "name": "Acme Corp",
                    "address": {"city": "New York", "country": "USA"},
                }
            }
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com/company",
                prompt="Extract company info",
                output_schema={
                    "type": "object",
                    "properties": {
                        "company": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "address": {
                                    "type": "object",
                                    "properties": {
                                        "city": {"type": "string"},
                                        "country": {"type": "string"},
                                    },
                                },
                            },
                        }
                    },
                },
            )

            assert result["success"] is True
            assert result["data"]["company"]["name"] == "Acme Corp"
            assert result["data"]["company"]["address"]["city"] == "New York"

    def test_inline_schema_with_arrays(self):
        """Should handle array type schemas."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {
                "products": [
                    {"name": "Product 1", "price": "$10"},
                    {"name": "Product 2", "price": "$20"},
                ]
            }
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com/products",
                prompt="Extract all products",
                output_schema={
                    "type": "object",
                    "properties": {
                        "products": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "name": {"type": "string"},
                                    "price": {"type": "string"},
                                },
                            },
                        }
                    },
                },
            )

            assert result["success"] is True
            assert len(result["data"]["products"]) == 2

    def test_no_schema_returns_error(self):
        """Should return schema_error when no schema provided."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract data",
                # No output_schema or schema provided
            )

            assert result["success"] is False
            assert result["error_type"] == "schema_error"
            assert "No schema provided" in result["error"]


class TestWebAiScrapeGitSchemaLoading:
    """Tests for Git schema loading (Story 008.2)."""

    def test_git_schema_loading_short_ref(self):
        """Should load schema from Git short reference."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                }

                mock_client = Mock()
                mock_client.smartscraper.return_value = {"name": "Test"}
                mock_scrapegraph_module.Client.return_value = mock_client

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract name",
                    schema={"uses": "company/schemas@v1.0.0#test.json"},
                )

                assert result["success"] is True
                mock_fetch.assert_called_once_with("company/schemas@v1.0.0#test.json")

    def test_git_schema_loading_full_url(self):
        """Should load schema from Git full URL reference."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"amount": {"type": "number"}},
                }

                mock_client = Mock()
                mock_client.smartscraper.return_value = {"amount": 99.99}
                mock_scrapegraph_module.Client.return_value = mock_client

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract amount",
                    schema={
                        "uses": "git+https://github.com/org/schemas.git@main#invoice.json"
                    },
                )

                assert result["success"] is True
                mock_fetch.assert_called_once_with(
                    "git+https://github.com/org/schemas.git@main#invoice.json"
                )


class TestWebAiScrapeFsspecSchemaLoading:
    """Tests for fsspec URI schema loading (s3://, gs://, az://)."""

    def test_s3_schema_loading(self):
        """Should load schema from S3 URI via fetch_schema."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"amount": {"type": "number"}},
                }

                mock_client = Mock()
                mock_client.smartscraper.return_value = {"amount": 99.99}
                mock_scrapegraph_module.Client.return_value = mock_client

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract amount",
                    schema={"uses": "s3://bucket/schemas/invoice.json"},
                )

                assert result["success"] is True
                mock_fetch.assert_called_once_with("s3://bucket/schemas/invoice.json")

    def test_gcs_schema_loading(self):
        """Should load schema from Google Cloud Storage URI."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"data": {"type": "string"}},
                }

                mock_client = Mock()
                mock_client.smartscraper.return_value = {"data": "test"}
                mock_scrapegraph_module.Client.return_value = mock_client

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract data",
                    schema={"uses": "gs://bucket-name/schemas/schema.json"},
                )

                assert result["success"] is True
                mock_fetch.assert_called_once_with(
                    "gs://bucket-name/schemas/schema.json"
                )

    def test_azure_schema_loading(self):
        """Should load schema from Azure Blob Storage URI."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"value": {"type": "integer"}},
                }

                mock_client = Mock()
                mock_client.smartscraper.return_value = {"value": 42}
                mock_scrapegraph_module.Client.return_value = mock_client

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract value",
                    schema={"uses": "az://container/schemas/custom.json"},
                )

                assert result["success"] is True
                mock_fetch.assert_called_once_with("az://container/schemas/custom.json")


class TestWebAiScrapeSchemaMerging:
    """Tests for schema merging from multiple sources (Story 008.3)."""

    def test_schema_merging_git_refs(self):
        """Should merge multiple schemas from Git refs."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                # Return different schemas for each call
                mock_fetch.side_effect = [
                    {"type": "object", "properties": {"base": {"type": "string"}}},
                    {"type": "object", "properties": {"overlay": {"type": "number"}}},
                ]

                # Use sys.modules to get the actual module (avoids namespace collision with function)
                import importlib

                dm_module = importlib.import_module("the_edge_agent.schema.deep_merge")
                with patch.object(dm_module, "merge_all") as mock_merge:
                    mock_merge.return_value = {
                        "type": "object",
                        "properties": {
                            "base": {"type": "string"},
                            "overlay": {"type": "number"},
                        },
                    }

                    mock_client = Mock()
                    mock_client.smartscraper.return_value = {
                        "base": "test",
                        "overlay": 123,
                    }
                    mock_scrapegraph_module.Client.return_value = mock_client

                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract",
                        schema={
                            "uses": [
                                "company/schemas@v1#base.json",
                                "company/schemas@v1#overlay.json",
                            ]
                        },
                    )

                    assert result["success"] is True
                    assert mock_fetch.call_count == 2
                    mock_merge.assert_called_once()

    def test_schema_merging_mixed_sources(self):
        """Should merge schemas from Git refs and fsspec URIs."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.side_effect = [
                    {"type": "object", "properties": {"git_field": {"type": "string"}}},
                    {"type": "object", "properties": {"s3_field": {"type": "boolean"}}},
                    {
                        "type": "object",
                        "properties": {"gcs_field": {"type": "integer"}},
                    },
                ]

                # Use sys.modules to get the actual module (avoids namespace collision with function)
                import importlib

                dm_module = importlib.import_module("the_edge_agent.schema.deep_merge")
                with patch.object(dm_module, "merge_all") as mock_merge:
                    mock_merge.return_value = {
                        "type": "object",
                        "properties": {
                            "git_field": {"type": "string"},
                            "s3_field": {"type": "boolean"},
                            "gcs_field": {"type": "integer"},
                        },
                    }

                    mock_client = Mock()
                    mock_client.smartscraper.return_value = {
                        "git_field": "test",
                        "s3_field": True,
                        "gcs_field": 42,
                    }
                    mock_scrapegraph_module.Client.return_value = mock_client

                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract",
                        schema={
                            "uses": [
                                "company/schemas@v1#base.json",
                                "s3://bucket/overlay.json",
                                "gs://shared/final.json",
                            ]
                        },
                    )

                    assert result["success"] is True
                    assert mock_fetch.call_count == 3

    def test_single_schema_uses_no_merge(self):
        """Should not call merge_all when only one schema is used."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                }

                # Use sys.modules to get the actual module (avoids namespace collision with function)
                import importlib

                dm_module = importlib.import_module("the_edge_agent.schema.deep_merge")
                with patch.object(dm_module, "merge_all") as mock_merge:
                    mock_client = Mock()
                    mock_client.smartscraper.return_value = {"name": "Test"}
                    mock_scrapegraph_module.Client.return_value = mock_client

                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract",
                        schema={"uses": "company/schemas@v1#single.json"},
                    )

                    assert result["success"] is True
                    mock_fetch.assert_called_once()
                    mock_merge.assert_not_called()


class TestWebAiScrapeRetryLogic:
    """Tests for retry logic with exponential backoff."""

    def test_retry_on_rate_limit_success(self):
        """Should retry with exponential backoff on rate limit and succeed."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            # First call raises rate limit, second succeeds
            mock_client.smartscraper.side_effect = [
                Exception("429 rate limit exceeded"),
                {"name": "Success"},
            ]
            mock_scrapegraph_module.Client.return_value = mock_client

            with patch("time.sleep") as mock_sleep:  # Don't actually sleep
                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    output_schema={
                        "type": "object",
                        "properties": {"name": {"type": "string"}},
                    },
                )

                assert result["success"] is True
                mock_sleep.assert_called_once_with(1)  # 2^0 = 1 second

    def test_retry_exhausted_returns_rate_limit_error(self):
        """Should return rate_limit error after all retries exhausted."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.side_effect = Exception("429 rate limit")
            mock_scrapegraph_module.Client.return_value = mock_client

            with patch("time.sleep"):
                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    output_schema={"type": "object", "properties": {}},
                    max_retries=3,
                )

                assert result["success"] is False
                assert result["error_type"] == "rate_limit"

    def test_retry_on_server_error(self):
        """Should retry on 5xx server errors."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.side_effect = [
                Exception("503 Service Unavailable"),
                {"data": "Success"},
            ]
            mock_scrapegraph_module.Client.return_value = mock_client

            with patch("time.sleep"):
                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    output_schema={
                        "type": "object",
                        "properties": {"data": {"type": "string"}},
                    },
                )

                assert result["success"] is True

    def test_exponential_backoff_timing(self):
        """Should use exponential backoff: 1s, 2s, 4s..."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.side_effect = [
                Exception("500 server error"),
                Exception("500 server error"),
                {"name": "Success"},
            ]
            mock_scrapegraph_module.Client.return_value = mock_client

            with patch("time.sleep") as mock_sleep:
                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    output_schema={
                        "type": "object",
                        "properties": {"name": {"type": "string"}},
                    },
                    max_retries=3,
                )

                assert result["success"] is True
                # First retry: 2^0 = 1, Second retry: 2^1 = 2
                assert mock_sleep.call_args_list == [
                    ((1,),),  # 2^0
                    ((2,),),  # 2^1
                ]


class TestWebAiScrapeErrorHandling:
    """Tests for error handling."""

    def test_api_timeout_handling(self):
        """Should handle API timeout gracefully."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.side_effect = Exception("Request timeout")
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={"type": "object", "properties": {}},
            )

            assert result["success"] is False
            assert result["error_type"] == "timeout"

    def test_authentication_error(self):
        """Should handle authentication/API key errors."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "invalid-key"}):
            mock_client = Mock()
            mock_client.smartscraper.side_effect = Exception(
                "401 Unauthorized - invalid api key"
            )
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={"type": "object", "properties": {}},
            )

            assert result["success"] is False
            assert result["error_type"] == "authentication"

    def test_schema_resolution_failure(self):
        """Should handle schema resolution failures."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            with patch("the_edge_agent.schema.fetch_schema") as mock_fetch:
                mock_fetch.side_effect = Exception("Failed to fetch schema from S3")

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    schema={"uses": "s3://nonexistent/schema.json"},
                )

                assert result["success"] is False
                assert result["error_type"] == "schema_error"
                assert "Schema resolution failed" in result["error"]

    def test_generic_api_error_no_retry(self):
        """Should not retry on generic non-retryable API errors."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.side_effect = Exception(
                "Invalid request parameters"
            )
            mock_scrapegraph_module.Client.return_value = mock_client

            with patch("time.sleep") as mock_sleep:
                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    output_schema={"type": "object", "properties": {}},
                    max_retries=3,
                )

                assert result["success"] is False
                assert result["error_type"] == "api_error"
                mock_sleep.assert_not_called()  # No retries


class TestWebAiScrapeSchemaInline:
    """Tests for inline schema within config."""

    def test_schema_config_inline(self):
        """Should use schema.inline when provided."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"title": "Test Title"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract title",
                schema={
                    "inline": {
                        "type": "object",
                        "properties": {
                            "title": {"type": "string", "description": "Page title"}
                        },
                    }
                },
            )

            assert result["success"] is True
            assert result["data"]["title"] == "Test Title"


class TestWebAiScrapePydanticConversion:
    """Tests for JSON Schema to Pydantic model conversion."""

    def test_all_json_schema_types(self):
        """Should handle all common JSON Schema types."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {
                "string_field": "text",
                "integer_field": 42,
                "number_field": 3.14,
                "boolean_field": True,
                "array_field": ["a", "b", "c"],
            }
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract all fields",
                output_schema={
                    "type": "object",
                    "properties": {
                        "string_field": {"type": "string"},
                        "integer_field": {"type": "integer"},
                        "number_field": {"type": "number"},
                        "boolean_field": {"type": "boolean"},
                        "array_field": {"type": "array", "items": {"type": "string"}},
                    },
                },
            )

            assert result["success"] is True
            assert result["data"]["string_field"] == "text"
            assert result["data"]["integer_field"] == 42
            assert result["data"]["number_field"] == 3.14
            assert result["data"]["boolean_field"] is True
            assert result["data"]["array_field"] == ["a", "b", "c"]

    def test_schema_without_type_treated_as_object(self):
        """Should treat schema without 'type' as object."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    # No "type": "object" wrapper
                    "name": {"type": "string"}
                },
            )

            assert result["success"] is True


class TestWebAiScrapeResponseHandling:
    """Tests for response handling (dict vs Pydantic model)."""

    def test_pydantic_model_response(self):
        """Should handle Pydantic model response with model_dump."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            # Create a mock Pydantic model response
            mock_response = Mock()
            mock_response.model_dump.return_value = {"name": "Pydantic Test"}

            mock_client = Mock()
            mock_client.smartscraper.return_value = mock_response
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
            )

            assert result["success"] is True
            assert result["data"]["name"] == "Pydantic Test"
            mock_response.model_dump.assert_called_once()

    def test_pydantic_v1_response(self):
        """Should handle Pydantic v1 response with .dict() method."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            # Create a mock Pydantic v1 model response (has .dict() but not .model_dump())
            mock_response = Mock(spec=["dict"])  # Only has .dict()
            mock_response.dict.return_value = {"name": "Pydantic v1 Test"}

            mock_client = Mock()
            mock_client.smartscraper.return_value = mock_response
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
            )

            assert result["success"] is True
            assert result["data"]["name"] == "Pydantic v1 Test"


class TestWebAiScrapeCaching:
    """Tests for cache behavior (TEA-BUILTIN-008.7)."""

    def test_cache_disabled_by_default(self):
        """Should not include cache metadata when cache not enabled."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                # No cache parameter
            )

            assert result["success"] is True
            assert "_cache_hit" not in result
            assert "_cache_key" not in result

    def test_cache_enabled_returns_metadata(self):
        """Should include cache metadata when cache enabled."""
        web_ai_scrape = get_web_ai_scrape()

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True},
            )

            assert result["success"] is True
            assert result["_cache_hit"] is False
            assert "_cache_key" in result
            assert result["_cache_key"].startswith("cache:web.ai_scrape:")

    def test_cache_hit_returns_cached_data(self):
        """Should return cached data on cache hit."""
        # We need to mock the engine with LTM backend
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()

        # Configure cache hit response
        mock_ltm.retrieve.return_value = {
            "found": True,
            "value": {
                "result": {
                    "success": True,
                    "data": {"name": "Cached Product", "price": "$50"},
                    "url": "https://example.com",
                    "schema_used": {},
                }
            },
            "metadata": {
                "_cache_expires_at": "2099-12-31T23:59:59+00:00",  # Far future
                "_cache_created_at": "2025-01-01T00:00:00+00:00",
            },
        }
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True},
            )

            assert result["success"] is True
            assert result["_cache_hit"] is True
            assert result["data"]["name"] == "Cached Product"
            assert result["_cache_created_at"] == "2025-01-01T00:00:00+00:00"

    def test_cache_skip_ignores_cached_data(self):
        """Should ignore cache when skip_cache=True."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()

        # Configure cache with data (should be skipped)
        mock_ltm.retrieve.return_value = {
            "found": True,
            "value": {"result": {"data": {"name": "Cached"}}},
            "metadata": {"_cache_expires_at": "2099-12-31T23:59:59+00:00"},
        }
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Fresh Data"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True, "skip_cache": True},
            )

            assert result["success"] is True
            assert result["_cache_hit"] is False
            assert result["data"]["name"] == "Fresh Data"
            # Should have called the API
            mock_client.smartscraper.assert_called_once()

    def test_cache_expired_fetches_fresh(self):
        """Should fetch fresh data when cache is expired."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()

        # Configure expired cache
        mock_ltm.retrieve.return_value = {
            "found": True,
            "value": {"result": {"data": {"name": "Expired Data"}}},
            "metadata": {"_cache_expires_at": "2020-01-01T00:00:00+00:00"},  # Past date
        }
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Fresh Data"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True},
            )

            assert result["success"] is True
            assert result["_cache_hit"] is False
            assert result["data"]["name"] == "Fresh Data"

    def test_cache_miss_stores_result(self):
        """Should store result in cache on cache miss."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()

        # Configure cache miss
        mock_ltm.retrieve.return_value = {"found": False}
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "New Data"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True, "ttl_days": 30},
            )

            assert result["success"] is True
            assert result["_cache_hit"] is False

            # Verify store was called
            mock_ltm.store.assert_called_once()
            store_call = mock_ltm.store.call_args
            assert "cache:web.ai_scrape:" in store_call.kwargs["key"]
            assert "_cache_type" in store_call.kwargs["metadata"]
            assert store_call.kwargs["metadata"]["_cache_type"] == "web.ai_scrape"

    def test_cache_key_strategy_url(self):
        """Should generate cache key from URL only with 'url' strategy."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()
        mock_ltm.retrieve.return_value = {"found": False}
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            # Same URL, different prompts should get same cache key
            result1 = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract products",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True, "key_strategy": "url"},
            )

            key1 = result1["_cache_key"]

            result2 = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Different prompt",  # Different prompt
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True, "key_strategy": "url"},
            )

            key2 = result2["_cache_key"]

            assert key1 == key2  # Same key because strategy is 'url' only

    def test_cache_key_strategy_url_prompt_schema(self):
        """Should generate different cache keys with 'url+prompt+schema' strategy."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()
        mock_ltm.retrieve.return_value = {"found": False}
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result1 = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract products",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True, "key_strategy": "url+prompt+schema"},
            )

            key1 = result1["_cache_key"]

            result2 = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Different prompt",  # Different prompt
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True, "key_strategy": "url+prompt+schema"},
            )

            key2 = result2["_cache_key"]

            assert key1 != key2  # Different keys because prompts differ

    def test_cache_ttl_seconds_priority(self):
        """Should use ttl_seconds over ttl_hours and ttl_days."""
        from the_edge_agent.actions import web_actions
        from datetime import datetime, timedelta, timezone

        mock_engine = Mock()
        mock_ltm = Mock()
        mock_ltm.retrieve.return_value = {"found": False}
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={
                    "enabled": True,
                    "ttl_days": 30,  # Would be 30 days
                    "ttl_hours": 24,  # Would be 24 hours
                    "ttl_seconds": 3600,  # 1 hour - should take priority
                },
            )

            # Check the metadata stored
            store_call = mock_ltm.store.call_args
            expires_at = store_call.kwargs["metadata"]["_cache_expires_at"]
            created_at = store_call.kwargs["metadata"]["_cache_created_at"]

            # Parse timestamps
            created = datetime.fromisoformat(created_at.replace("Z", "+00:00"))
            expires = datetime.fromisoformat(expires_at.replace("Z", "+00:00"))

            # Should be approximately 1 hour difference (ttl_seconds=3600)
            diff = (expires - created).total_seconds()
            assert 3550 <= diff <= 3650  # Allow small margin

    def test_cache_ltm_failure_graceful(self):
        """Should gracefully handle LTM failures and proceed with API call."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()

        # LTM raises exception
        mock_ltm.retrieve.side_effect = Exception("LTM connection failed")
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Fallback Data"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True},
            )

            # Should succeed despite LTM failure
            assert result["success"] is True
            assert result["data"]["name"] == "Fallback Data"
            assert result["_cache_hit"] is False

    def test_cache_store_failure_graceful(self):
        """Should gracefully handle LTM store failures."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_ltm = Mock()

        mock_ltm.retrieve.return_value = {"found": False}
        mock_ltm.store.side_effect = Exception("LTM store failed")
        mock_engine._ltm_backend = mock_ltm

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test Data"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True},
            )

            # Should succeed despite store failure
            assert result["success"] is True
            assert result["data"]["name"] == "Test Data"

    def test_cache_no_ltm_backend(self):
        """Should work when no LTM backend is configured."""
        from the_edge_agent.actions import web_actions

        mock_engine = Mock()
        mock_engine._ltm_backend = None  # No LTM configured

        registry = {}
        web_actions.register_actions(registry, mock_engine)
        web_ai_scrape = registry["web.ai_scrape"]

        with patch.dict(os.environ, {"SCRAPEGRAPH_API_KEY": "test-key"}):
            mock_client = Mock()
            mock_client.smartscraper.return_value = {"name": "Test"}
            mock_scrapegraph_module.Client.return_value = mock_client

            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
                cache={"enabled": True},
            )

            assert result["success"] is True
            assert result["_cache_hit"] is False


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
