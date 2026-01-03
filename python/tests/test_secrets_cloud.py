"""
Cloud Secrets Backend Tests (TEA-BUILTIN-012.2).

This module contains unit tests for AWS, Azure, and GCP secrets backends.
All tests use mocked cloud APIs - no real cloud credentials are required.

Test Coverage:
    - AWS Secrets Manager: JSON secret, prefix-based, error handling
    - Azure Key Vault: get/has/get_all, error handling
    - GCP Secret Manager: get/has/get_all, prefix filtering, error handling
    - Import error handling for missing dependencies

Note: These tests require the cloud SDK dependencies to be installed.
      Tests are skipped if the dependencies are not available.
"""

import json
import sys
import pytest
from unittest.mock import Mock, MagicMock, patch, PropertyMock


# =============================================================================
# DEPENDENCY CHECK HELPERS
# =============================================================================


def _check_boto3():
    """Check if boto3 and botocore are available."""
    try:
        import boto3
        import botocore

        return True
    except ImportError:
        return False


def _check_azure():
    """Check if Azure SDK packages are available."""
    try:
        import azure.identity
        import azure.keyvault.secrets
        import azure.core.exceptions

        return True
    except ImportError:
        return False


def _check_gcp():
    """Check if Google Cloud Secret Manager is available."""
    try:
        from google.cloud import secretmanager
        from google.api_core import exceptions

        return True
    except ImportError:
        return False


# Skip markers
requires_boto3 = pytest.mark.skipif(
    not _check_boto3(), reason="boto3/botocore not installed"
)
requires_azure = pytest.mark.skipif(
    not _check_azure(), reason="azure-identity/azure-keyvault-secrets not installed"
)
requires_gcp = pytest.mark.skipif(
    not _check_gcp(), reason="google-cloud-secret-manager not installed"
)


# =============================================================================
# AWS SECRETS MANAGER TESTS
# =============================================================================


@requires_boto3
class TestAWSSecretsBackend:
    """Tests for AWS Secrets Manager backend."""

    def test_aws_import_error_without_boto3(self):
        """GIVEN boto3 not installed, WHEN AWSSecretsBackend imported, THEN ImportError with hint."""
        with patch.dict(sys.modules, {"boto3": None}):
            # Need to reload the module to trigger the ImportError
            import importlib
            from the_edge_agent.secrets import aws

            importlib.reload(aws)

            with pytest.raises(ImportError) as exc_info:
                aws.AWSSecretsBackend(secret_name="test")

            assert "pip install the-edge-agent[aws]" in str(exc_info.value)

            # Restore the module
            importlib.reload(aws)

    def test_aws_backend_requires_secret_name_or_prefix(self):
        """GIVEN no secret_name or secret_prefix, WHEN creating backend, THEN ValueError."""
        import boto3

        with patch.object(boto3, "client"):
            from the_edge_agent.secrets.aws import AWSSecretsBackend

            with pytest.raises(ValueError) as exc_info:
                AWSSecretsBackend()
            assert "Must specify either" in str(exc_info.value)

    def test_aws_backend_rejects_both_secret_name_and_prefix(self):
        """GIVEN both secret_name and secret_prefix, WHEN creating backend, THEN ValueError."""
        import boto3

        with patch.object(boto3, "client"):
            from the_edge_agent.secrets.aws import AWSSecretsBackend

            with pytest.raises(ValueError) as exc_info:
                AWSSecretsBackend(secret_name="test", secret_prefix="prefix/")
            assert "Cannot specify both" in str(exc_info.value)

    def test_aws_backend_get_from_json_secret(self):
        """GIVEN JSON secret in AWS, WHEN get() called, THEN value returned."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretString": '{"api_key": "secret123", "db_password": "dbsecret"}'
            }

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="myapp/secrets")

            assert backend.get("api_key") == "secret123"
            assert backend.get("db_password") == "dbsecret"
            assert backend.get("missing", "default") == "default"

    def test_aws_backend_get_from_prefix_secrets(self):
        """GIVEN prefix-based secrets in AWS, WHEN get() called, THEN values returned."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client

            # Mock paginator for listing secrets
            mock_paginator = Mock()
            mock_client.get_paginator.return_value = mock_paginator
            mock_paginator.paginate.return_value = [
                {
                    "SecretList": [
                        {"Name": "myapp/api_key"},
                        {"Name": "myapp/db_password"},
                        {"Name": "other/secret"},  # Should be ignored
                    ]
                }
            ]

            # Mock get_secret_value for individual secrets
            def get_secret_side_effect(SecretId):
                secrets = {
                    "myapp/api_key": {"SecretString": "secret123"},
                    "myapp/db_password": {"SecretString": "dbsecret"},
                }
                return secrets.get(SecretId, {})

            mock_client.get_secret_value.side_effect = get_secret_side_effect

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_prefix="myapp/")

            assert backend.get("api_key") == "secret123"
            assert backend.get("db_password") == "dbsecret"

    def test_aws_backend_get_all_returns_copy(self):
        """GIVEN AWS backend, WHEN get_all() called, THEN returns dict copy."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretString": '{"key1": "val1", "key2": "val2"}'
            }

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="test")

            all_secrets = backend.get_all()
            assert all_secrets == {"key1": "val1", "key2": "val2"}

            # Verify it's a copy, not the internal cache
            all_secrets["key3"] = "val3"
            assert "key3" not in backend.get_all()

    def test_aws_backend_has(self):
        """GIVEN AWS backend, WHEN has() called, THEN returns correct boolean."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretString": '{"api_key": "secret"}'
            }

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="test")

            assert backend.has("api_key") is True
            assert backend.has("missing") is False

    def test_aws_backend_resource_not_found_error(self):
        """GIVEN non-existent secret, WHEN loading, THEN RuntimeError raised."""
        import boto3
        from botocore.exceptions import ClientError

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.side_effect = ClientError(
                {
                    "Error": {
                        "Code": "ResourceNotFoundException",
                        "Message": "Secret not found",
                    }
                },
                "GetSecretValue",
            )

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="nonexistent")

            with pytest.raises(RuntimeError) as exc_info:
                backend.get("any")
            assert "Secret not found" in str(exc_info.value)

    def test_aws_backend_access_denied_error(self):
        """GIVEN insufficient permissions, WHEN loading, THEN RuntimeError raised."""
        import boto3
        from botocore.exceptions import ClientError

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.side_effect = ClientError(
                {
                    "Error": {
                        "Code": "AccessDeniedException",
                        "Message": "Access denied",
                    }
                },
                "GetSecretValue",
            )

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="restricted")

            with pytest.raises(RuntimeError) as exc_info:
                backend.get("any")
            assert "Access denied" in str(exc_info.value)

    def test_aws_backend_binary_secret(self):
        """GIVEN binary secret, WHEN loading, THEN stored as _binary key."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretBinary": b"\x00\x01\x02\x03"
            }

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="binary-secret")

            assert backend.get("_binary") == b"\x00\x01\x02\x03"

    def test_aws_backend_non_json_string_secret(self):
        """GIVEN non-JSON string secret, WHEN loading, THEN stored as _value key."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretString": "plain-text-secret"
            }

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="plain-secret")

            assert backend.get("_value") == "plain-text-secret"


# =============================================================================
# AZURE KEY VAULT TESTS
# =============================================================================


@requires_azure
class TestAzureSecretsBackend:
    """Tests for Azure Key Vault backend."""

    def test_azure_import_error_without_packages(self):
        """GIVEN azure packages not installed, WHEN AzureSecretsBackend imported, THEN ImportError with hint."""
        with patch.dict(
            sys.modules, {"azure.identity": None, "azure.keyvault.secrets": None}
        ):
            import importlib
            from the_edge_agent.secrets import azure

            importlib.reload(azure)

            with pytest.raises(ImportError) as exc_info:
                azure.AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            assert "pip install the-edge-agent[azure]" in str(exc_info.value)

            # Restore the module
            importlib.reload(azure)

    def test_azure_backend_requires_vault_url(self):
        """GIVEN no vault_url, WHEN creating backend, THEN ValueError."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            with pytest.raises(ValueError) as exc_info:
                AzureSecretsBackend(vault_url="")
            assert "vault_url is required" in str(exc_info.value)

    def test_azure_backend_validates_vault_url_format(self):
        """GIVEN invalid vault_url format, WHEN creating backend, THEN ValueError."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            with pytest.raises(ValueError) as exc_info:
                AzureSecretsBackend(vault_url="http://invalid-url.com")
            assert "Invalid vault_url" in str(exc_info.value)

    def test_azure_backend_get_secret(self):
        """GIVEN secret in Azure Key Vault, WHEN get() called, THEN value returned."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        mock_secret = Mock()
        mock_secret.value = "secret123"

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", return_value=mock_secret
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            assert backend.get("api-key") == "secret123"

    def test_azure_backend_get_missing_secret(self):
        """GIVEN missing secret, WHEN get() called, THEN default returned."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient
        from azure.core.exceptions import ResourceNotFoundError

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", side_effect=ResourceNotFoundError("Not found")
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            assert backend.get("missing", "default") == "default"

    def test_azure_backend_get_all(self):
        """GIVEN secrets in Key Vault, WHEN get_all() called, THEN all secrets returned."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        # Mock list_properties_of_secrets
        mock_prop1 = Mock()
        mock_prop1.name = "api-key"
        mock_prop2 = Mock()
        mock_prop2.name = "db-password"

        # Mock get_secret
        def get_secret_side_effect(name):
            secrets = {"api-key": "secret1", "db-password": "secret2"}
            mock_secret = Mock()
            mock_secret.value = secrets[name]
            return mock_secret

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient,
            "list_properties_of_secrets",
            return_value=[mock_prop1, mock_prop2],
        ), patch.object(
            SecretClient, "get_secret", side_effect=get_secret_side_effect
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            all_secrets = backend.get_all()
            assert all_secrets == {"api-key": "secret1", "db-password": "secret2"}

    def test_azure_backend_get_all_returns_copy(self):
        """GIVEN Azure backend, WHEN get_all() called, THEN returns dict copy."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "list_properties_of_secrets", return_value=[]
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            all_secrets = backend.get_all()
            all_secrets["new_key"] = "new_value"
            assert "new_key" not in backend.get_all()

    def test_azure_backend_has(self):
        """GIVEN Azure backend, WHEN has() called, THEN returns correct boolean."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient
        from azure.core.exceptions import ResourceNotFoundError

        def get_secret_side_effect(name):
            if name == "exists":
                mock_secret = Mock()
                mock_secret.value = "value"
                return mock_secret
            raise ResourceNotFoundError("Not found")

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", side_effect=get_secret_side_effect
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            assert backend.has("exists") is True
            assert backend.has("missing") is False

    def test_azure_backend_caches_secrets(self):
        """GIVEN Azure backend, WHEN get() called twice, THEN only one API call."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        mock_secret = Mock()
        mock_secret.value = "cached_value"
        mock_get_secret = Mock(return_value=mock_secret)

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", mock_get_secret
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            # First call
            assert backend.get("api-key") == "cached_value"
            # Second call - should use cache
            assert backend.get("api-key") == "cached_value"

            # Only one API call should have been made
            assert mock_get_secret.call_count == 1

    def test_azure_backend_access_denied_on_get(self):
        """GIVEN insufficient permissions, WHEN get() called, THEN RuntimeError raised."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient
        from azure.core.exceptions import HttpResponseError

        error = HttpResponseError("Forbidden")
        error.status_code = 403

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", side_effect=error
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")

            with pytest.raises(RuntimeError) as exc_info:
                backend.get("restricted")
            assert "Access denied" in str(exc_info.value)


# =============================================================================
# GCP SECRET MANAGER TESTS
# =============================================================================


@requires_gcp
class TestGCPSecretsBackend:
    """Tests for Google Cloud Secret Manager backend."""

    def test_gcp_import_error_without_package(self):
        """GIVEN google-cloud-secret-manager not installed, WHEN imported, THEN ImportError with hint."""
        with patch.dict(
            sys.modules, {"google.cloud": None, "google.cloud.secretmanager": None}
        ):
            import importlib
            from the_edge_agent.secrets import gcp

            importlib.reload(gcp)

            with pytest.raises(ImportError) as exc_info:
                gcp.GCPSecretsBackend(project_id="test-project")

            assert "pip install the-edge-agent[gcp]" in str(exc_info.value)

            # Restore the module
            importlib.reload(gcp)

    def test_gcp_backend_requires_project_id(self):
        """GIVEN no project_id, WHEN creating backend, THEN ValueError."""
        from google.cloud import secretmanager

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            with pytest.raises(ValueError) as exc_info:
                GCPSecretsBackend(project_id="")
            assert "project_id is required" in str(exc_info.value)

    def test_gcp_backend_get_secret(self):
        """GIVEN secret in GCP, WHEN get() called, THEN value returned."""
        from google.cloud import secretmanager

        mock_response = Mock()
        mock_response.payload.data = b"secret123"

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            return_value=mock_response,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            assert backend.get("api_key") == "secret123"

    def test_gcp_backend_get_secret_with_prefix(self):
        """GIVEN secret with prefix in GCP, WHEN get() called, THEN value returned."""
        from google.cloud import secretmanager

        mock_response = Mock()
        mock_response.payload.data = b"prefixed_secret"
        mock_access = Mock(return_value=mock_response)

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            mock_access,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project", secret_prefix="myapp-")

            result = backend.get("api_key")
            assert result == "prefixed_secret"

            # Verify the correct secret name was requested
            call_args = mock_access.call_args
            assert "myapp-api_key" in call_args[1]["request"]["name"]

    def test_gcp_backend_get_missing_secret(self):
        """GIVEN missing secret, WHEN get() called, THEN default returned."""
        from google.cloud import secretmanager
        from google.api_core.exceptions import NotFound

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            side_effect=NotFound("Not found"),
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            assert backend.get("missing", "default") == "default"

    def test_gcp_backend_get_all(self):
        """GIVEN secrets in GCP, WHEN get_all() called, THEN all secrets returned."""
        from google.cloud import secretmanager

        # Mock list_secrets
        mock_secret1 = Mock()
        mock_secret1.name = "projects/my-project/secrets/api_key"
        mock_secret2 = Mock()
        mock_secret2.name = "projects/my-project/secrets/db_password"

        # Mock access_secret_version
        def access_side_effect(request):
            name = request["name"]
            mock_response = Mock()
            if "api_key" in name:
                mock_response.payload.data = b"secret1"
            else:
                mock_response.payload.data = b"secret2"
            return mock_response

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "list_secrets",
            return_value=[mock_secret1, mock_secret2],
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            side_effect=access_side_effect,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            all_secrets = backend.get_all()
            assert all_secrets == {"api_key": "secret1", "db_password": "secret2"}

    def test_gcp_backend_get_all_with_prefix_filter(self):
        """GIVEN secrets with mixed prefixes, WHEN get_all() called, THEN only matching secrets returned."""
        from google.cloud import secretmanager

        # Mock list_secrets - includes secrets with and without prefix
        mock_secret1 = Mock()
        mock_secret1.name = "projects/my-project/secrets/myapp-api_key"
        mock_secret2 = Mock()
        mock_secret2.name = "projects/my-project/secrets/myapp-db_password"
        mock_secret3 = Mock()
        mock_secret3.name = (
            "projects/my-project/secrets/other-secret"  # Should be filtered out
        )

        # Mock access_secret_version
        def access_side_effect(request):
            name = request["name"]
            mock_response = Mock()
            if "api_key" in name:
                mock_response.payload.data = b"secret1"
            elif "db_password" in name:
                mock_response.payload.data = b"secret2"
            return mock_response

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "list_secrets",
            return_value=[mock_secret1, mock_secret2, mock_secret3],
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            side_effect=access_side_effect,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project", secret_prefix="myapp-")

            all_secrets = backend.get_all()
            # Keys should have prefix stripped
            assert "api_key" in all_secrets
            assert "db_password" in all_secrets
            assert "other-secret" not in all_secrets

    def test_gcp_backend_get_all_returns_copy(self):
        """GIVEN GCP backend, WHEN get_all() called, THEN returns dict copy."""
        from google.cloud import secretmanager

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient, "list_secrets", return_value=[]
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            all_secrets = backend.get_all()
            all_secrets["new_key"] = "new_value"
            assert "new_key" not in backend.get_all()

    def test_gcp_backend_has(self):
        """GIVEN GCP backend, WHEN has() called, THEN returns correct boolean."""
        from google.cloud import secretmanager
        from google.api_core.exceptions import NotFound

        def access_side_effect(request):
            if "exists" in request["name"]:
                mock_response = Mock()
                mock_response.payload.data = b"value"
                return mock_response
            raise NotFound("Not found")

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            side_effect=access_side_effect,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            assert backend.has("exists") is True
            assert backend.has("missing") is False

    def test_gcp_backend_permission_denied_error(self):
        """GIVEN insufficient permissions, WHEN get() called, THEN RuntimeError raised."""
        from google.cloud import secretmanager
        from google.api_core.exceptions import PermissionDenied

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            side_effect=PermissionDenied("Forbidden"),
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            with pytest.raises(RuntimeError) as exc_info:
                backend.get("restricted")
            assert "Access denied" in str(exc_info.value)

    def test_gcp_backend_caches_secrets(self):
        """GIVEN GCP backend, WHEN get() called twice, THEN only one API call."""
        from google.cloud import secretmanager

        mock_response = Mock()
        mock_response.payload.data = b"cached_value"
        mock_access = Mock(return_value=mock_response)

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            mock_access,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-project")

            # First call
            assert backend.get("api_key") == "cached_value"
            # Second call - should use cache
            assert backend.get("api_key") == "cached_value"

            # Only one API call should have been made
            assert mock_access.call_count == 1


# =============================================================================
# FACTORY FUNCTION TESTS
# =============================================================================


class TestSecretsFactory:
    """Tests for create_secrets_backend factory function."""

    @requires_boto3
    def test_factory_creates_aws_backend(self):
        """GIVEN aws backend type, WHEN factory called, THEN AWSSecretsBackend returned."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretString": '{"key": "value"}'
            }

            from the_edge_agent.secrets import create_secrets_backend

            backend = create_secrets_backend("aws", secret_name="test")

            assert backend.__class__.__name__ == "AWSSecretsBackend"

    @requires_azure
    def test_factory_creates_azure_backend(self):
        """GIVEN azure backend type, WHEN factory called, THEN AzureSecretsBackend returned."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None):
            from the_edge_agent.secrets import create_secrets_backend

            backend = create_secrets_backend(
                "azure", vault_url="https://test.vault.azure.net"
            )

            assert backend.__class__.__name__ == "AzureSecretsBackend"

    @requires_gcp
    def test_factory_creates_gcp_backend(self):
        """GIVEN gcp backend type, WHEN factory called, THEN GCPSecretsBackend returned."""
        from google.cloud import secretmanager

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ):
            from the_edge_agent.secrets import create_secrets_backend

            backend = create_secrets_backend("gcp", project_id="my-project")

            assert backend.__class__.__name__ == "GCPSecretsBackend"

    def test_factory_raises_for_unknown_backend(self):
        """GIVEN unknown backend type, WHEN factory called, THEN ValueError."""
        from the_edge_agent.secrets import create_secrets_backend

        with pytest.raises(ValueError) as exc_info:
            create_secrets_backend("unknown")
        assert "Unknown secrets backend type" in str(exc_info.value)

    @requires_boto3
    def test_factory_import_error_message_for_aws(self):
        """GIVEN boto3 not installed, WHEN factory creates aws, THEN ImportError with hint."""
        with patch.dict(sys.modules, {"boto3": None}):
            import importlib
            from the_edge_agent.secrets import aws

            importlib.reload(aws)

            from the_edge_agent.secrets import create_secrets_backend

            with pytest.raises(ImportError) as exc_info:
                create_secrets_backend("aws", secret_name="test")
            assert "pip install the-edge-agent[aws]" in str(exc_info.value)

            # Restore module
            importlib.reload(aws)

    def test_get_registered_backends_includes_cloud(self):
        """GIVEN secrets module, WHEN get_registered_backends(), THEN includes cloud backends."""
        from the_edge_agent.secrets import get_registered_backends

        backends = get_registered_backends()

        assert "env" in backends
        assert "aws" in backends
        assert "azure" in backends
        assert "gcp" in backends


# =============================================================================
# SECURITY TESTS
# =============================================================================


@requires_boto3
class TestSecurityRequirementsAWS:
    """Tests for AWS security requirements per QA Risk Assessment."""

    def test_aws_error_does_not_leak_credentials(self):
        """GIVEN AWS credential error, WHEN raised, THEN no credentials in message."""
        import boto3
        from botocore.exceptions import ClientError

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.side_effect = ClientError(
                {
                    "Error": {
                        "Code": "AccessDeniedException",
                        "Message": "User: arn:aws:iam::123:user/test",
                    }
                },
                "GetSecretValue",
            )

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            backend = AWSSecretsBackend(secret_name="test")

            try:
                backend.get("any")
            except RuntimeError as e:
                error_msg = str(e)
                # Should not contain ARNs or specific user info
                assert "arn:aws:iam" not in error_msg
                assert "Access denied" in error_msg


@requires_azure
class TestSecurityRequirementsAzure:
    """Tests for Azure security requirements per QA Risk Assessment."""

    def test_azure_error_does_not_leak_vault_url(self):
        """GIVEN Azure error, WHEN raised, THEN vault URL not fully exposed."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient
        from azure.core.exceptions import HttpResponseError

        error = HttpResponseError("Forbidden")
        error.status_code = 403

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", side_effect=error
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            backend = AzureSecretsBackend(vault_url="https://myvault.vault.azure.net")

            try:
                backend.get("secret")
            except RuntimeError as e:
                error_msg = str(e)
                # Should not expose the full vault URL in error
                assert "myvault" not in error_msg
                assert "Access denied" in error_msg


@requires_gcp
class TestSecurityRequirementsGCP:
    """Tests for GCP security requirements per QA Risk Assessment."""

    def test_gcp_error_does_not_leak_project_id(self):
        """GIVEN GCP error, WHEN raised, THEN project ID not in generic error."""
        from google.cloud import secretmanager
        from google.api_core.exceptions import PermissionDenied

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            side_effect=PermissionDenied("Forbidden"),
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            backend = GCPSecretsBackend(project_id="my-secret-project-123")

            try:
                backend.get("secret")
            except RuntimeError as e:
                error_msg = str(e)
                # Should not expose the project ID in error
                assert "my-secret-project-123" not in error_msg


# =============================================================================
# CONTEXT MANAGER TESTS (inherited from base)
# =============================================================================


@requires_boto3
class TestContextManagerAWS:
    """Tests for AWS context manager support."""

    def test_aws_backend_context_manager(self):
        """GIVEN AWS backend, WHEN used as context manager, THEN works correctly."""
        import boto3

        with patch.object(boto3, "client") as mock_boto:
            mock_client = Mock()
            mock_boto.return_value = mock_client
            mock_client.get_secret_value.return_value = {
                "SecretString": '{"key": "value"}'
            }

            from the_edge_agent.secrets.aws import AWSSecretsBackend

            with AWSSecretsBackend(secret_name="test") as backend:
                assert backend.get("key") == "value"


@requires_azure
class TestContextManagerAzure:
    """Tests for Azure context manager support."""

    def test_azure_backend_context_manager(self):
        """GIVEN Azure backend, WHEN used as context manager, THEN works correctly."""
        from azure.identity import DefaultAzureCredential
        from azure.keyvault.secrets import SecretClient

        mock_secret = Mock()
        mock_secret.value = "value"

        with patch.object(
            DefaultAzureCredential, "__init__", return_value=None
        ), patch.object(SecretClient, "__init__", return_value=None), patch.object(
            SecretClient, "get_secret", return_value=mock_secret
        ):
            from the_edge_agent.secrets.azure import AzureSecretsBackend

            with AzureSecretsBackend(
                vault_url="https://test.vault.azure.net"
            ) as backend:
                assert backend.get("key") == "value"


@requires_gcp
class TestContextManagerGCP:
    """Tests for GCP context manager support."""

    def test_gcp_backend_context_manager(self):
        """GIVEN GCP backend, WHEN used as context manager, THEN works correctly."""
        from google.cloud import secretmanager

        mock_response = Mock()
        mock_response.payload.data = b"value"

        with patch.object(
            secretmanager.SecretManagerServiceClient, "__init__", return_value=None
        ), patch.object(
            secretmanager.SecretManagerServiceClient,
            "access_secret_version",
            return_value=mock_response,
        ):
            from the_edge_agent.secrets.gcp import GCPSecretsBackend

            with GCPSecretsBackend(project_id="my-project") as backend:
                assert backend.get("key") == "value"
