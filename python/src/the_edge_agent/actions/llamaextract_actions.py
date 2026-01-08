"""
LlamaExtract Actions for YAMLEngine.

This module provides document extraction capabilities using LlamaExtract's
AI-powered extraction API.

TEA-BUILTIN-008.1: Core LlamaExtract Actions
TEA-BUILTIN-008.5: Direct REST API Integration
TEA-BUILTIN-008.6: Async Polling Configuration
TEA-BUILTIN-008.7: Workflow Primitives

Actions:
    - llamaextract.extract: Extract structured data from documents (REST API)
    - llamaextract.submit_job: Submit async extraction job (TEA-BUILTIN-008.7)
    - llamaextract.poll_status: Check job status (TEA-BUILTIN-008.7)
    - llamaextract.get_result: Retrieve extraction result (TEA-BUILTIN-008.7)
    - llamaextract.upload_agent: Create or update extraction agent
    - llamaextract.list_agents: List available extraction agents
    - llamaextract.get_agent: Get agent details
    - llamaextract.delete_agent: Delete an extraction agent

Required Environment Variables:
    - LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY: API key for LlamaExtract

Example:
    >>> result = registry['llamaextract.extract'](
    ...     state={},
    ...     file="https://example.com/invoice.pdf",
    ...     schema={"type": "object", "properties": {"total": {"type": "number"}}}
    ... )
    >>> print(result['data'])
"""

import base64
import json
import os
import time
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Union


# Extraction mode enum values
EXTRACTION_MODES = ["BALANCED", "MULTIMODAL", "PREMIUM", "FAST"]

# LlamaExtract REST API base URL
LLAMAEXTRACT_API_BASE = "https://api.cloud.llamaindex.ai"


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register llamaextract actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def _get_api_key() -> Optional[str]:
        """Get API key from environment variables."""
        return os.environ.get("LLAMAEXTRACT_API_KEY") or os.environ.get(
            "LLAMAPARSE_API_KEY"
        )

    def _get_mime_type(file_path: str) -> str:
        """Get MIME type from file extension."""
        ext = Path(file_path).suffix.lower()
        mime_types = {
            ".pdf": "application/pdf",
            ".docx": "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
            ".doc": "application/msword",
            ".txt": "text/plain",
            ".csv": "text/csv",
            ".json": "application/json",
            ".html": "text/html",
            ".htm": "text/html",
            ".md": "text/markdown",
            ".png": "image/png",
            ".jpg": "image/jpeg",
            ".jpeg": "image/jpeg",
        }
        return mime_types.get(ext, "application/octet-stream")

    def _prepare_file_content(file: str) -> Dict[str, Any]:
        """
        Prepare file content for REST API request.

        Args:
            file: Document source - URL, base64 content, or local file path

        Returns:
            Dict with 'file' object containing 'data' and 'mime_type'
            for base64/local files, or URL info for HTTP sources

        Raises:
            FileNotFoundError: If local file does not exist
        """
        try:
            import requests as http_requests
        except ImportError:
            http_requests = None

        if file.startswith(("http://", "https://")):
            # For URLs, we need to download and convert to base64
            # since the API expects file.data and file.mime_type
            if http_requests is None:
                raise ImportError("requests package required for URL downloads")

            try:
                response = http_requests.get(file, timeout=60)
                response.raise_for_status()
            except http_requests.exceptions.HTTPError as e:
                raise ValueError(f"Failed to download file from URL: {e}")
            except http_requests.exceptions.ConnectionError as e:
                raise ValueError(f"Connection error downloading file: {e}")
            except http_requests.exceptions.Timeout as e:
                raise ValueError(f"Timeout downloading file: {e}")

            content = base64.b64encode(response.content).decode("utf-8")
            mime_type = response.headers.get("Content-Type", _get_mime_type(file))
            # Strip charset if present
            if ";" in mime_type:
                mime_type = mime_type.split(";")[0].strip()
            return {"file": {"data": content, "mime_type": mime_type}}
        elif file.startswith("data:") or ";base64," in file:
            # Base64 encoded content with optional MIME type
            if file.startswith("data:"):
                # data:application/pdf;base64,<content>
                parts = file.split(",", 1)
                if len(parts) == 2:
                    mime_part = parts[0]
                    content = parts[1]
                    # Extract mime type: data:application/pdf;base64
                    if ";base64" in mime_part:
                        mime_type = mime_part.replace("data:", "").replace(
                            ";base64", ""
                        )
                    else:
                        mime_type = "application/octet-stream"
                else:
                    content = file
                    mime_type = "application/octet-stream"
            elif ";base64," in file:
                content = file.split(";base64,")[1]
                mime_type = "application/octet-stream"
            else:
                content = file
                mime_type = "application/octet-stream"
            return {"file": {"data": content, "mime_type": mime_type}}
        else:
            # Local file path
            path = Path(file)
            if not path.exists():
                raise FileNotFoundError(f"File not found: {file}")
            with open(path, "rb") as f:
                content = base64.b64encode(f.read()).decode("utf-8")
            return {"file": {"data": content, "mime_type": _get_mime_type(file)}}

    def _poll_job_status(
        job_id: str,
        headers: Dict[str, str],
        timeout: int,
        poll_interval: Union[int, float] = 5,
        max_attempts: Optional[int] = None,
    ) -> Dict[str, Any]:
        """
        Poll job status until completion, timeout, or max attempts exceeded.

        TEA-BUILTIN-008.6: Configurable polling parameters.

        Args:
            job_id: The extraction job ID
            headers: HTTP headers with authorization
            timeout: Maximum time to wait for completion (seconds)
            poll_interval: Time between polls (seconds), accepts floats (AC-2, AC-7)
            max_attempts: Maximum number of poll attempts (AC-3, AC-8)
                          None means no limit (only timeout applies)

        Returns:
            Dict with success status and extracted data or error info
        """
        try:
            import requests
        except ImportError:
            return {
                "success": False,
                "error": "requests package not installed",
                "error_type": "dependency",
            }

        # Validate poll_interval (AC-25: clear error messages)
        if poll_interval <= 0:
            return {
                "success": False,
                "error": f"polling_interval must be positive, got {poll_interval}",
                "error_type": "validation",
            }

        # Validate max_attempts if specified (AC-25)
        if max_attempts is not None and max_attempts <= 0:
            return {
                "success": False,
                "error": f"max_poll_attempts must be positive, got {max_attempts}",
                "error_type": "validation",
            }

        start_time = time.time()
        attempt_count = 0
        status_url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/jobs/{job_id}"
        result_url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/jobs/{job_id}/result"

        while time.time() - start_time < timeout:
            try:
                # Check job status
                response = requests.get(status_url, headers=headers, timeout=30)

                if response.status_code != 200:
                    return {
                        "success": False,
                        "error": f"Failed to check job status: {response.text}",
                        "error_type": "api_error",
                        "job_id": job_id,
                    }

                job_data = response.json()
                status = job_data.get("status", "UNKNOWN")

                if status == "SUCCESS":
                    # Get the result
                    result_response = requests.get(
                        result_url, headers=headers, timeout=30
                    )
                    if result_response.status_code == 200:
                        result_data = result_response.json()
                        return {
                            "success": True,
                            "data": result_data.get("data", result_data),
                            "status": "completed",
                            "job_id": job_id,
                        }
                    else:
                        return {
                            "success": False,
                            "error": f"Failed to get result: {result_response.text}",
                            "error_type": "api_error",
                            "job_id": job_id,
                        }

                elif status == "ERROR":
                    error_msg = job_data.get("error", "Extraction failed")
                    return {
                        "success": False,
                        "error": error_msg,
                        "error_type": "extraction_error",
                        "job_id": job_id,
                    }

                elif status == "PARTIAL_SUCCESS":
                    # Get partial result
                    result_response = requests.get(
                        result_url, headers=headers, timeout=30
                    )
                    if result_response.status_code == 200:
                        result_data = result_response.json()
                        return {
                            "success": True,
                            "data": result_data.get("data", result_data),
                            "status": "partial",
                            "job_id": job_id,
                            "warning": "Some pages may have failed extraction",
                        }
                    else:
                        return {
                            "success": False,
                            "error": "Failed to get partial result",
                            "error_type": "api_error",
                            "job_id": job_id,
                        }

                elif status in ("PENDING", "RUNNING"):
                    # Still processing, increment attempt counter
                    attempt_count += 1

                    # Check max_attempts limit (AC-8)
                    if max_attempts is not None and attempt_count >= max_attempts:
                        elapsed = time.time() - start_time
                        return {
                            "success": False,
                            "error": (
                                f"Max poll attempts ({max_attempts}) exceeded "
                                f"after {elapsed:.1f}s"
                            ),
                            "error_type": "timeout",
                            "job_id": job_id,
                            "attempts": attempt_count,
                        }

                    # Wait and retry
                    time.sleep(poll_interval)
                    continue

                else:
                    return {
                        "success": False,
                        "error": f"Unknown job status: {status}",
                        "error_type": "api_error",
                        "job_id": job_id,
                    }

            except requests.Timeout:
                attempt_count += 1
                # Check max_attempts on timeout too (AC-8)
                if max_attempts is not None and attempt_count >= max_attempts:
                    elapsed = time.time() - start_time
                    return {
                        "success": False,
                        "error": (
                            f"Max poll attempts ({max_attempts}) exceeded "
                            f"after {elapsed:.1f}s (request timeout)"
                        ),
                        "error_type": "timeout",
                        "job_id": job_id,
                        "attempts": attempt_count,
                    }
                time.sleep(poll_interval)
                continue
            except Exception as e:
                return {
                    "success": False,
                    "error": f"Polling error: {str(e)}",
                    "error_type": "api_error",
                    "job_id": job_id,
                }

        elapsed = time.time() - start_time
        return {
            "success": False,
            "error": f"Job timed out after {elapsed:.1f}s ({attempt_count} attempts)",
            "error_type": "timeout",
            "job_id": job_id,
            "attempts": attempt_count,
        }

    def _execute_rest_with_retry(
        url: str,
        payload: Dict[str, Any],
        headers: Dict[str, str],
        timeout: int,
        max_retries: int,
    ) -> Dict[str, Any]:
        """
        Execute HTTP request with exponential backoff retry.
        Handles job-based response by polling for completion.

        Args:
            url: Target URL
            payload: JSON payload
            headers: HTTP headers
            timeout: Request timeout in seconds (also used for polling)
            max_retries: Maximum number of retry attempts

        Returns:
            Dict with success status and data or error info
        """
        try:
            import requests
        except ImportError:
            return {
                "success": False,
                "error": "requests package not installed. Install with: pip install requests",
                "error_type": "dependency",
            }

        last_error = None

        for attempt in range(max_retries):
            try:
                response = requests.post(
                    url,
                    json=payload,
                    headers=headers,
                    timeout=min(timeout, 120),  # Initial request timeout
                )

                if response.status_code == 200:
                    data = response.json()

                    # Check if this is a job-based response (has id and status=PENDING)
                    if data.get("status") == "PENDING" and data.get("id"):
                        job_id = data["id"]
                        # Poll for job completion
                        return _poll_job_status(job_id, headers, timeout)

                    # Direct result
                    return {
                        "success": True,
                        "data": data.get("data", data),
                        "status": "completed",
                    }

                elif response.status_code == 429:
                    # Rate limit - retry with backoff
                    last_error = {
                        "success": False,
                        "error": "Rate limit exceeded",
                        "error_type": "rate_limit",
                        "status_code": 429,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                elif response.status_code >= 500:
                    # Server error - retry with backoff
                    last_error = {
                        "success": False,
                        "error": f"Server error: {response.status_code}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                elif response.status_code == 401:
                    # Authentication error - don't retry
                    return {
                        "success": False,
                        "error": "Authentication failed. Check API key.",
                        "error_type": "configuration",
                        "status_code": 401,
                    }

                elif response.status_code == 400:
                    # Bad request - don't retry
                    try:
                        error_detail = response.json()
                        error_msg = error_detail.get("detail", response.text)
                    except Exception:
                        error_msg = response.text
                    return {
                        "success": False,
                        "error": f"Bad request: {error_msg}",
                        "error_type": "validation",
                        "status_code": 400,
                    }

                elif response.status_code == 422:
                    # Validation error - don't retry
                    try:
                        error_detail = response.json()
                        error_msg = error_detail.get("detail", response.text)
                    except Exception:
                        error_msg = response.text
                    return {
                        "success": False,
                        "error": f"Validation error: {error_msg}",
                        "error_type": "validation",
                        "status_code": 422,
                    }

                else:
                    # Other client error - don't retry
                    return {
                        "success": False,
                        "error": f"HTTP {response.status_code}: {response.text}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }

            except requests.Timeout:
                return {
                    "success": False,
                    "error": f"Request timed out after {timeout}s",
                    "error_type": "timeout",
                }
            except requests.ConnectionError as e:
                last_error = {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "api_error",
                }
                if attempt < max_retries - 1:
                    time.sleep(2**attempt)
                    continue
                return last_error
            except Exception as e:
                return {"success": False, "error": str(e), "error_type": "api_error"}

        # Should not reach here, but safety fallback
        return last_error or {
            "success": False,
            "error": "Max retries exceeded",
            "error_type": "api_error",
        }

    def _extract_via_rest(
        file: str,
        schema: Optional[Dict[str, Any]] = None,
        agent_id: Optional[str] = None,
        mode: str = "BALANCED",
        timeout: int = 300,
        max_retries: int = 3,
    ) -> Dict[str, Any]:
        """
        Extract structured data using LlamaExtract REST API directly.

        Uses the sync endpoint: POST /api/v1/extraction/run

        Args:
            file: Document source - URL, base64 content, or local file path
            schema: JSON Schema defining the structure to extract
            agent_id: ID of existing extraction agent to use
            mode: Extraction mode - BALANCED, MULTIMODAL, PREMIUM, or FAST
            timeout: HTTP request timeout in seconds
            max_retries: Maximum retry attempts for transient failures

        Returns:
            Dict with 'success', 'data', 'status', or error info
        """
        # Validate mode
        mode_upper = mode.upper()
        if mode_upper not in EXTRACTION_MODES:
            return {
                "success": False,
                "error": f"Invalid extraction mode: {mode}. Valid modes: {EXTRACTION_MODES}",
                "error_type": "validation",
            }

        # Get API key
        api_key = _get_api_key()
        if not api_key:
            return {
                "success": False,
                "error": "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set",
                "error_type": "configuration",
            }

        # Prepare file content
        try:
            file_data = _prepare_file_content(file)
        except FileNotFoundError as e:
            return {"success": False, "error": str(e), "error_type": "file_not_found"}
        except ValueError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "file_download_error",
            }
        except ImportError as e:
            return {"success": False, "error": str(e), "error_type": "dependency"}

        # Build request payload
        payload = {**file_data, "config": {"extraction_mode": mode_upper}}

        # Add schema or agent_id
        if agent_id:
            payload["agent_id"] = agent_id
        elif schema:
            payload["data_schema"] = schema

        # Build headers
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        # Execute request
        url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/run"
        return _execute_rest_with_retry(url, payload, headers, timeout, max_retries)

    def _extract_via_async(
        file: str,
        schema: Optional[Dict[str, Any]] = None,
        agent_id: Optional[str] = None,
        mode: str = "BALANCED",
        timeout: int = 300,
        max_retries: int = 3,
        polling_interval: Union[int, float] = 5,
        max_poll_attempts: int = 120,
    ) -> Dict[str, Any]:
        """
        Extract structured data using LlamaExtract async /jobs endpoint.

        TEA-BUILTIN-008.6: Async extraction with configurable polling.

        Uses the async endpoint: POST /api/v1/extraction/jobs

        Args:
            file: Document source - URL, base64 content, or local file path
            schema: JSON Schema defining the structure to extract
            agent_id: ID of existing extraction agent to use
            mode: Extraction mode - BALANCED, MULTIMODAL, PREMIUM, or FAST
            timeout: Overall operation timeout in seconds (AC-4, AC-9)
            max_retries: Maximum retry attempts for submission
            polling_interval: Seconds between poll requests (AC-2, AC-7)
            max_poll_attempts: Maximum poll attempts before timeout (AC-3, AC-8)

        Returns:
            Dict with 'success', 'data', 'status', 'job_id', or error info
        """
        try:
            import requests
        except ImportError:
            return {
                "success": False,
                "error": "requests package not installed. Install with: pip install requests",
                "error_type": "dependency",
            }

        # Validate mode
        mode_upper = mode.upper()
        if mode_upper not in EXTRACTION_MODES:
            return {
                "success": False,
                "error": f"Invalid extraction mode: {mode}. Valid modes: {EXTRACTION_MODES}",
                "error_type": "validation",
            }

        # Get API key
        api_key = _get_api_key()
        if not api_key:
            return {
                "success": False,
                "error": "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set",
                "error_type": "configuration",
            }

        # Prepare file content
        try:
            file_data = _prepare_file_content(file)
        except FileNotFoundError as e:
            return {"success": False, "error": str(e), "error_type": "file_not_found"}
        except ValueError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "file_download_error",
            }
        except ImportError as e:
            return {"success": False, "error": str(e), "error_type": "dependency"}

        # Build request payload
        payload = {**file_data, "config": {"extraction_mode": mode_upper}}

        # Add schema or agent_id
        if agent_id:
            payload["agent_id"] = agent_id
        elif schema:
            payload["data_schema"] = schema

        # Build headers
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        # Submit to async /jobs endpoint (AC-6)
        url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/jobs"
        last_error = None

        for attempt in range(max_retries):
            try:
                response = requests.post(
                    url,
                    json=payload,
                    headers=headers,
                    timeout=min(timeout, 120),  # Initial request timeout
                )

                if response.status_code == 200:
                    data = response.json()
                    job_id = data.get("id")

                    if not job_id:
                        return {
                            "success": False,
                            "error": "No job_id returned from async endpoint",
                            "error_type": "api_error",
                        }

                    # Poll for job completion with configurable parameters (AC-7, AC-8)
                    return _poll_job_status(
                        job_id=job_id,
                        headers=headers,
                        timeout=timeout,
                        poll_interval=polling_interval,
                        max_attempts=max_poll_attempts,
                    )

                elif response.status_code == 429:
                    # Rate limit - retry with backoff
                    last_error = {
                        "success": False,
                        "error": "Rate limit exceeded",
                        "error_type": "rate_limit",
                        "status_code": 429,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                elif response.status_code >= 500:
                    # Server error - retry with backoff
                    last_error = {
                        "success": False,
                        "error": f"Server error: {response.status_code}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                elif response.status_code == 401:
                    return {
                        "success": False,
                        "error": "Authentication failed. Check API key.",
                        "error_type": "configuration",
                        "status_code": 401,
                    }

                elif response.status_code in (400, 422):
                    try:
                        error_detail = response.json()
                        error_msg = error_detail.get("detail", response.text)
                    except Exception:
                        error_msg = response.text
                    return {
                        "success": False,
                        "error": f"Validation error: {error_msg}",
                        "error_type": "validation",
                        "status_code": response.status_code,
                    }

                else:
                    return {
                        "success": False,
                        "error": f"HTTP {response.status_code}: {response.text}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }

            except requests.Timeout:
                return {
                    "success": False,
                    "error": f"Request timed out after {timeout}s",
                    "error_type": "timeout",
                }
            except requests.ConnectionError as e:
                last_error = {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "api_error",
                }
                if attempt < max_retries - 1:
                    time.sleep(2**attempt)
                    continue
                return last_error
            except Exception as e:
                return {"success": False, "error": str(e), "error_type": "api_error"}

        return last_error or {
            "success": False,
            "error": "Max retries exceeded",
            "error_type": "api_error",
        }

    def _get_client():
        """Get LlamaExtract client."""
        try:
            from llama_cloud_services import LlamaExtract
        except ImportError:
            raise ImportError(
                "llama-cloud-services package not installed. "
                "Install with: pip install llama-cloud-services"
            )

        api_key = _get_api_key()
        if not api_key:
            raise ValueError(
                "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set"
            )

        # Note: SDK uses LLAMA_CLOUD_API_KEY internally
        # Set it temporarily if using LLAMAPARSE_API_KEY
        import os

        if "LLAMA_CLOUD_API_KEY" not in os.environ and api_key:
            os.environ["LLAMA_CLOUD_API_KEY"] = api_key

        return LlamaExtract(api_key=api_key)

    def _get_extract_mode(mode: str):
        """Get ExtractMode enum value from llama_cloud_services."""
        try:
            from llama_cloud_services.extract import ExtractMode
        except ImportError:
            raise ImportError(
                "llama-cloud-services package not installed. "
                "Install with: pip install llama-cloud-services"
            )

        mode_upper = mode.upper()
        if mode_upper == "BALANCED":
            return ExtractMode.BALANCED
        elif mode_upper == "MULTIMODAL":
            return ExtractMode.MULTIMODAL
        elif mode_upper == "PREMIUM":
            return ExtractMode.PREMIUM
        elif mode_upper == "FAST":
            return ExtractMode.FAST
        else:
            raise ValueError(
                f"Invalid extraction mode: {mode}. " f"Valid modes: {EXTRACTION_MODES}"
            )

    def _retry_with_backoff(
        func: Callable,
        max_retries: int = 3,
        base_delay: float = 1.0,
        max_delay: float = 30.0,
    ) -> Any:
        """Execute function with exponential backoff retry."""
        last_exception = None

        for attempt in range(max_retries):
            try:
                return func()
            except Exception as e:
                last_exception = e
                error_str = str(e).lower()

                # Don't retry on configuration errors
                if "api_key" in error_str or "authentication" in error_str:
                    raise

                # Don't retry on validation errors
                if "invalid" in error_str and "schema" in error_str:
                    raise

                # Retry on rate limits and transient errors
                if attempt < max_retries - 1:
                    delay = min(base_delay * (2**attempt), max_delay)
                    time.sleep(delay)

        raise last_exception

    # ============================================================
    # llamaextract.extract - Extract structured data from documents
    # ============================================================

    def llamaextract_extract(
        state,
        file: str,
        schema: Optional[Dict[str, Any]] = None,
        agent_id: Optional[str] = None,
        agent_name: Optional[str] = None,
        mode: str = "BALANCED",
        timeout: int = 300,
        max_retries: int = 3,
        use_rest: bool = False,
        async_mode: bool = False,
        polling_interval: Union[int, float] = 5,
        max_poll_attempts: int = 120,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Extract structured data from a document using LlamaExtract.

        Uses SDK by default for client-side validation before sending to server.
        Set use_rest=True for direct REST API calls (fewer dependencies).

        TEA-BUILTIN-008.6: Async polling configuration parameters.

        Args:
            state: Current workflow state
            file: Document source - URL, base64 content, or local file path
            schema: JSON Schema defining the structure to extract.
                   Either schema or agent_id/agent_name must be provided.
            agent_id: ID of existing extraction agent to use
            agent_name: Name of existing extraction agent to use
            mode: Extraction mode - BALANCED, MULTIMODAL, PREMIUM, or FAST
            timeout: HTTP request timeout in seconds (default: 300)
            max_retries: Maximum retry attempts for transient failures
            use_rest: If True, use direct REST API instead of SDK (default: False)
            async_mode: If True, use async /jobs endpoint explicitly (AC-1, AC-6)
                       Requires use_rest=True. When False (default), uses sync behavior.
            polling_interval: Seconds between poll requests (default: 5) (AC-2, AC-7)
                             Accepts floats for sub-second precision.
            max_poll_attempts: Maximum number of poll attempts (default: 120) (AC-3, AC-8)
                              Combined with polling_interval, default allows ~10 min.

        Returns:
            Dict with 'success', 'data' (extracted data), 'status', etc.

        Example:
            >>> result = llamaextract_extract(
            ...     state={},
            ...     file="https://example.com/invoice.pdf",
            ...     schema={
            ...         "type": "object",
            ...         "properties": {
            ...             "invoice_number": {"type": "string"},
            ...             "total": {"type": "number"}
            ...         }
            ...     },
            ...     mode="PREMIUM",
            ...     timeout=300
            ... )

        Example with async mode (for large documents):
            >>> result = llamaextract_extract(
            ...     state={},
            ...     file="large-contract.pdf",
            ...     schema={...},
            ...     use_rest=True,
            ...     async_mode=True,
            ...     polling_interval=10,
            ...     max_poll_attempts=60,
            ...     timeout=900
            ... )
        """
        # Validate inputs
        if not file:
            return {
                "success": False,
                "error": "file parameter is required",
                "error_type": "validation",
            }

        if not schema and not agent_id and not agent_name:
            return {
                "success": False,
                "error": "Either schema, agent_id, or agent_name must be provided",
                "error_type": "validation",
            }

        # TEA-BUILTIN-008.6: Validate async_mode requires use_rest (AC-1, AC-6)
        if async_mode and not use_rest:
            return {
                "success": False,
                "error": "async_mode=True requires use_rest=True",
                "error_type": "validation",
            }

        # TEA-BUILTIN-008.6: Validate polling parameters (AC-2, AC-3)
        if polling_interval <= 0:
            return {
                "success": False,
                "error": f"polling_interval must be positive, got {polling_interval}",
                "error_type": "validation",
            }
        if max_poll_attempts <= 0:
            return {
                "success": False,
                "error": f"max_poll_attempts must be positive, got {max_poll_attempts}",
                "error_type": "validation",
            }

        # TEA-BUILTIN-008.6: Async mode - use /jobs endpoint directly (AC-1, AC-6)
        if async_mode and use_rest and not agent_name:
            return _extract_via_async(
                file=file,
                schema=schema,
                agent_id=agent_id,
                mode=mode,
                timeout=timeout,
                max_retries=max_retries,
                polling_interval=polling_interval,
                max_poll_attempts=max_poll_attempts,
            )

        # TEA-BUILTIN-008.5: Use REST API when explicitly requested (sync mode)
        # Note: REST API doesn't support agent_name lookup, only agent_id
        if use_rest and not agent_name:
            return _extract_via_rest(
                file=file,
                schema=schema,
                agent_id=agent_id,
                mode=mode,
                timeout=timeout,
                max_retries=max_retries,
            )

        # SDK path (default) - provides client-side validation
        try:
            client = _get_client()
            extract_mode = _get_extract_mode(mode)
        except ImportError as e:
            return {"success": False, "error": str(e), "error_type": "dependency"}
        except ValueError as e:
            return {"success": False, "error": str(e), "error_type": "configuration"}

        try:
            from llama_cloud_services.extract import ExtractConfig
        except ImportError:
            return {
                "success": False,
                "error": "llama-cloud-services package not installed",
                "error_type": "dependency",
            }

        # Prepare file input for SDK
        # SDK accepts: str (path or URL), Path, BufferedIOBase, SourceText, File
        file_input = None
        local_file_path = None

        if file.startswith(("http://", "https://")):
            # URL - pass directly to SDK
            file_input = file
        elif file.startswith("data:") or ";base64," in file:
            # Base64 encoded content - need to write to temp file
            import tempfile

            if ";base64," in file:
                content = file.split(";base64,")[1]
            else:
                content = file
            # Create temp file
            with tempfile.NamedTemporaryFile(delete=False, suffix=".pdf") as tmp:
                tmp.write(base64.b64decode(content))
                local_file_path = tmp.name
            file_input = local_file_path
        else:
            # Local file path - SDK accepts path directly
            path = Path(file)
            if not path.exists():
                return {
                    "success": False,
                    "error": f"File not found: {file}",
                    "error_type": "file_not_found",
                }
            file_input = str(path.absolute())

        def do_extract():
            # Build extraction config
            config = ExtractConfig(extraction_mode=extract_mode)

            if agent_id:
                # Use existing agent by ID - need to get agent object first
                agent = client.get_agent(id=agent_id)
                result = agent.extract(file_input)
            elif agent_name:
                # Use existing agent by name - need to get agent object first
                agent = client.get_agent(name=agent_name)
                result = agent.extract(file_input)
            else:
                # Use inline schema - client.extract(data_schema, config, files)
                result = client.extract(
                    data_schema=schema, config=config, files=file_input
                )
            return result

        try:
            result = _retry_with_backoff(do_extract, max_retries=max_retries)

            return {
                "success": True,
                "data": result.data if hasattr(result, "data") else result,
                "job_id": result.job_id if hasattr(result, "job_id") else None,
                "status": "completed",
            }

        except Exception as e:
            error_str = str(e)
            error_type = "api_error"

            if "rate" in error_str.lower() or "429" in error_str:
                error_type = "rate_limit"
            elif "timeout" in error_str.lower():
                error_type = "timeout"
            elif "not found" in error_str.lower():
                error_type = "not_found"

            return {"success": False, "error": error_str, "error_type": error_type}

    # ============================================================
    # TEA-BUILTIN-008.7: Workflow Primitives
    # ============================================================

    # ============================================================
    # llamaextract.submit_job - Submit async extraction job
    # ============================================================

    def llamaextract_submit_job(
        state,
        file: str,
        schema: Optional[Dict[str, Any]] = None,
        agent_id: Optional[str] = None,
        mode: str = "BALANCED",
        max_retries: int = 3,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Submit async extraction job to LlamaExtract.

        TEA-BUILTIN-008.7: Workflow primitive for job submission.

        This primitive submits a document extraction job and returns immediately
        with the job_id. Use llamaextract.poll_status and llamaextract.get_result
        to monitor and retrieve results.

        Args:
            state: Current workflow state
            file: Document source - URL, base64 content, or local file path
            schema: JSON Schema defining the structure to extract
            agent_id: ID of existing extraction agent to use
            mode: Extraction mode - BALANCED, MULTIMODAL, PREMIUM, or FAST
            max_retries: Maximum retry attempts for job submission

        Returns:
            Dict with:
            - success: True if job submitted successfully
            - job_id: The extraction job ID for polling
            - status: "PENDING" on successful submission
            - error/error_type: On failure

        Example:
            >>> result = llamaextract_submit_job(
            ...     state={},
            ...     file="invoice.pdf",
            ...     schema={"type": "object", "properties": {"total": {"type": "number"}}},
            ...     mode="FAST"
            ... )
            >>> if result['success']:
            ...     job_id = result['job_id']  # Use for polling
        """
        try:
            import requests
        except ImportError:
            return {
                "success": False,
                "error": "requests package not installed. Install with: pip install requests",
                "error_type": "dependency",
            }

        # Validate inputs (AC-1)
        if not file:
            return {
                "success": False,
                "error": "file parameter is required",
                "error_type": "validation",
            }

        if not schema and not agent_id:
            return {
                "success": False,
                "error": "Either schema or agent_id must be provided",
                "error_type": "validation",
            }

        # Validate mode (AC-4)
        mode_upper = mode.upper()
        if mode_upper not in EXTRACTION_MODES:
            return {
                "success": False,
                "error": f"Invalid extraction mode: {mode}. Valid modes: {EXTRACTION_MODES}",
                "error_type": "validation",
            }

        # Get API key (AC-12)
        api_key = _get_api_key()
        if not api_key:
            return {
                "success": False,
                "error": "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set",
                "error_type": "configuration",
            }

        # Prepare file content (AC-1: handles path, URL, base64)
        try:
            file_data = _prepare_file_content(file)
        except FileNotFoundError as e:
            return {"success": False, "error": str(e), "error_type": "file_not_found"}
        except ValueError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "file_download_error",
            }
        except ImportError as e:
            return {"success": False, "error": str(e), "error_type": "dependency"}

        # Build request payload
        payload = {**file_data, "config": {"extraction_mode": mode_upper}}

        # Add schema or agent_id
        if agent_id:
            payload["agent_id"] = agent_id
        elif schema:
            payload["data_schema"] = schema

        # Build headers
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        # Submit to async /jobs endpoint (AC-1, AC-2)
        url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/jobs"
        last_error = None

        for attempt in range(max_retries):
            try:
                response = requests.post(
                    url,
                    json=payload,
                    headers=headers,
                    timeout=120,  # Submission timeout
                )

                if response.status_code == 200:
                    data = response.json()
                    job_id = data.get("id")
                    status = data.get("status", "PENDING")

                    if not job_id:
                        return {
                            "success": False,
                            "error": "No job_id returned from async endpoint",
                            "error_type": "api_error",
                        }

                    # AC-2: Return job_id and PENDING status
                    return {
                        "success": True,
                        "job_id": job_id,
                        "status": status,
                    }

                elif response.status_code == 401:
                    # AC-3: Authentication error
                    return {
                        "success": False,
                        "error": "Authentication failed. Check API key.",
                        "error_type": "configuration",
                        "status_code": 401,
                    }

                elif response.status_code == 400:
                    # AC-3: Bad request
                    try:
                        error_detail = response.json()
                        error_msg = error_detail.get("detail", response.text)
                    except Exception:
                        error_msg = response.text
                    return {
                        "success": False,
                        "error": f"Bad request: {error_msg}",
                        "error_type": "validation",
                        "status_code": 400,
                    }

                elif response.status_code == 429:
                    # Rate limit - retry with backoff (AC-13)
                    last_error = {
                        "success": False,
                        "error": "Rate limit exceeded",
                        "error_type": "rate_limit",
                        "status_code": 429,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                elif response.status_code >= 500:
                    # Server error - retry with backoff (AC-13)
                    last_error = {
                        "success": False,
                        "error": f"Server error: {response.status_code}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                else:
                    # AC-3: Structured error
                    return {
                        "success": False,
                        "error": f"HTTP {response.status_code}: {response.text}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }

            except requests.Timeout:
                return {
                    "success": False,
                    "error": "Request timed out during job submission",
                    "error_type": "timeout",
                }
            except requests.ConnectionError as e:
                last_error = {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "api_error",
                }
                if attempt < max_retries - 1:
                    time.sleep(2**attempt)
                    continue
                return last_error
            except Exception as e:
                return {"success": False, "error": str(e), "error_type": "api_error"}

        return last_error or {
            "success": False,
            "error": "Max retries exceeded",
            "error_type": "api_error",
        }

    # ============================================================
    # llamaextract.poll_status - Check job status
    # ============================================================

    def llamaextract_poll_status(
        state,
        job_id: Optional[str] = None,
        timeout: int = 10,
        max_retries: int = 3,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Poll job status from LlamaExtract.

        TEA-BUILTIN-008.7: Workflow primitive for status polling.

        This primitive checks the current status of an extraction job.
        Use in custom polling loops for advanced workflow control.

        Args:
            state: Current workflow state
            job_id: The extraction job ID from submit_job
            timeout: HTTP request timeout in seconds (default: 10)
            max_retries: Maximum retry attempts for transient failures

        Returns:
            Dict with:
            - success: True if status retrieved successfully
            - status: "PENDING", "RUNNING", "SUCCESS", "ERROR", "PARTIAL_SUCCESS"
            - progress: Progress percentage (0-100) if available
            - error: Error message when status is ERROR
            - error_type: On API failure

        Example:
            >>> result = llamaextract_poll_status(
            ...     state={},
            ...     job_id="job_abc123"
            ... )
            >>> if result['success'] and result['status'] == 'SUCCESS':
            ...     # Ready to get result
            ...     pass
        """
        try:
            import requests
        except ImportError:
            return {
                "success": False,
                "error": "requests package not installed. Install with: pip install requests",
                "error_type": "dependency",
            }

        # Validate inputs (AC-5)
        if not job_id:
            return {
                "success": False,
                "error": "job_id parameter is required",
                "error_type": "validation",
            }

        # Get API key (AC-12)
        api_key = _get_api_key()
        if not api_key:
            return {
                "success": False,
                "error": "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set",
                "error_type": "configuration",
            }

        # Build headers
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        status_url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/jobs/{job_id}"
        last_error = None

        for attempt in range(max_retries):
            try:
                # AC-8: Configurable timeout for individual poll
                response = requests.get(status_url, headers=headers, timeout=timeout)

                if response.status_code == 200:
                    job_data = response.json()
                    status = job_data.get("status", "UNKNOWN")
                    progress = job_data.get("progress", 0)

                    # AC-6: Return status with progress
                    result = {
                        "success": True,
                        "status": status,
                        "progress": progress,
                        "job_id": job_id,
                    }

                    # AC-7: Include error field when status is ERROR
                    if status == "ERROR":
                        result["error"] = job_data.get("error", "Extraction failed")

                    return result

                elif response.status_code == 401:
                    return {
                        "success": False,
                        "error": "Authentication failed. Check API key.",
                        "error_type": "configuration",
                        "status_code": 401,
                    }

                elif response.status_code == 404:
                    return {
                        "success": False,
                        "error": f"Job not found: {job_id}",
                        "error_type": "not_found",
                        "status_code": 404,
                    }

                elif response.status_code >= 500:
                    # Server error - retry (AC-13)
                    last_error = {
                        "success": False,
                        "error": f"Server error: {response.status_code}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return last_error

                else:
                    return {
                        "success": False,
                        "error": f"HTTP {response.status_code}: {response.text}",
                        "error_type": "api_error",
                        "status_code": response.status_code,
                    }

            except requests.Timeout:
                last_error = {
                    "success": False,
                    "error": f"Request timed out after {timeout}s",
                    "error_type": "timeout",
                }
                if attempt < max_retries - 1:
                    time.sleep(2**attempt)
                    continue
                return last_error
            except requests.ConnectionError as e:
                last_error = {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "api_error",
                }
                if attempt < max_retries - 1:
                    time.sleep(2**attempt)
                    continue
                return last_error
            except Exception as e:
                return {"success": False, "error": str(e), "error_type": "api_error"}

        return last_error or {
            "success": False,
            "error": "Max retries exceeded",
            "error_type": "api_error",
        }

    # ============================================================
    # llamaextract.get_result - Retrieve extraction result
    # ============================================================

    def llamaextract_get_result(
        state,
        job_id: Optional[str] = None,
        timeout: int = 30,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get extraction result for a completed job.

        TEA-BUILTIN-008.7: Workflow primitive for result retrieval.

        This primitive retrieves the extracted data for a completed job.
        Only call after poll_status returns SUCCESS or PARTIAL_SUCCESS.

        Args:
            state: Current workflow state
            job_id: The extraction job ID
            timeout: HTTP request timeout in seconds (default: 30)

        Returns:
            Dict with:
            - success: True if result retrieved successfully
            - data: Extracted data when job is complete
            - job_id: The job ID for reference
            - error/error_type: If job not complete or failed

        Example:
            >>> result = llamaextract_get_result(
            ...     state={},
            ...     job_id="job_abc123"
            ... )
            >>> if result['success']:
            ...     extracted_data = result['data']
        """
        try:
            import requests
        except ImportError:
            return {
                "success": False,
                "error": "requests package not installed. Install with: pip install requests",
                "error_type": "dependency",
            }

        # Validate inputs (AC-9)
        if not job_id:
            return {
                "success": False,
                "error": "job_id parameter is required",
                "error_type": "validation",
            }

        # Get API key (AC-12)
        api_key = _get_api_key()
        if not api_key:
            return {
                "success": False,
                "error": "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set",
                "error_type": "configuration",
            }

        # Build headers
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        result_url = f"{LLAMAEXTRACT_API_BASE}/api/v1/extraction/jobs/{job_id}/result"

        try:
            response = requests.get(result_url, headers=headers, timeout=timeout)

            if response.status_code == 200:
                result_data = response.json()
                # AC-10: Return data with job_id
                return {
                    "success": True,
                    "data": result_data.get("data", result_data),
                    "job_id": job_id,
                }

            elif response.status_code == 401:
                return {
                    "success": False,
                    "error": "Authentication failed. Check API key.",
                    "error_type": "configuration",
                    "status_code": 401,
                }

            elif response.status_code == 404:
                # AC-11: Job not found or not complete
                return {
                    "success": False,
                    "error": f"Result not found for job: {job_id}. Job may not be complete.",
                    "error_type": "not_found",
                    "status_code": 404,
                    "job_id": job_id,
                }

            elif response.status_code == 400:
                # AC-11: Job not ready or failed
                try:
                    error_detail = response.json()
                    error_msg = error_detail.get("detail", response.text)
                except Exception:
                    error_msg = response.text
                return {
                    "success": False,
                    "error": f"Cannot get result: {error_msg}",
                    "error_type": "job_incomplete",
                    "status_code": 400,
                    "job_id": job_id,
                }

            else:
                return {
                    "success": False,
                    "error": f"HTTP {response.status_code}: {response.text}",
                    "error_type": "api_error",
                    "status_code": response.status_code,
                    "job_id": job_id,
                }

        except requests.Timeout:
            return {
                "success": False,
                "error": f"Request timed out after {timeout}s",
                "error_type": "timeout",
                "job_id": job_id,
            }
        except requests.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}",
                "error_type": "api_error",
                "job_id": job_id,
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "api_error",
                "job_id": job_id,
            }

    # ============================================================
    # llamaextract.upload_agent - Create or update extraction agent
    # ============================================================

    def llamaextract_upload_agent(
        state,
        name: str,
        schema: Dict[str, Any],
        description: Optional[str] = None,
        mode: str = "BALANCED",
        force: bool = False,
        max_retries: int = 3,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Create or update an extraction agent.

        Args:
            state: Current workflow state
            name: Agent name (must be unique)
            schema: JSON Schema for extraction
            description: Agent description
            mode: Default extraction mode
            force: If True, update existing agent with same name
            max_retries: Maximum retry attempts

        Returns:
            Dict with 'success', 'agent_id', 'name', 'status'

        Example:
            >>> result = llamaextract_upload_agent(
            ...     state={},
            ...     name="invoice-extractor",
            ...     schema={"type": "object", "properties": {...}},
            ...     mode="PREMIUM"
            ... )
        """
        if not name:
            return {
                "success": False,
                "error": "name parameter is required",
                "error_type": "validation",
            }

        if not schema:
            return {
                "success": False,
                "error": "schema parameter is required",
                "error_type": "validation",
            }

        try:
            client = _get_client()
            extract_mode = _get_extract_mode(mode)
        except (ImportError, ValueError) as e:
            return {"success": False, "error": str(e), "error_type": "configuration"}

        try:
            from llama_cloud_services.extract import ExtractConfig
        except ImportError:
            return {
                "success": False,
                "error": "llama-cloud-services package not installed",
                "error_type": "dependency",
            }

        def do_upload():
            config = ExtractConfig(extraction_mode=extract_mode)

            # Check if agent exists
            try:
                existing = client.get_agent(name=name)
                if not force:
                    raise ValueError(
                        f"Agent '{name}' already exists. Use force=True to update."
                    )
                # Update existing agent by modifying and saving
                existing.data_schema = schema
                existing.config = config
                existing.save()
                return {"agent": existing, "action": "updated"}
            except Exception as e:
                # Agent doesn't exist, create new one
                error_msg = str(e).lower()
                if (
                    "not found" in error_msg
                    or "does not exist" in error_msg
                    or "no extraction agent" in error_msg
                ):
                    result = client.create_agent(
                        name=name, data_schema=schema, config=config
                    )
                    return {"agent": result, "action": "created"}
                else:
                    raise

        try:
            result = _retry_with_backoff(do_upload, max_retries=max_retries)

            agent = result["agent"]
            return {
                "success": True,
                "agent_id": agent.id if hasattr(agent, "id") else None,
                "name": name,
                "action": result["action"],
                "status": "active",
            }

        except ValueError as e:
            return {"success": False, "error": str(e), "error_type": "validation"}
        except Exception as e:
            return {"success": False, "error": str(e), "error_type": "api_error"}

    # ============================================================
    # llamaextract.list_agents - List available extraction agents
    # ============================================================

    def llamaextract_list_agents(
        state, name_filter: Optional[str] = None, **kwargs
    ) -> Dict[str, Any]:
        """
        List available extraction agents.

        Args:
            state: Current workflow state
            name_filter: Optional name filter (substring match)

        Returns:
            Dict with 'success', 'agents' (list of agent info)

        Example:
            >>> result = llamaextract_list_agents(state={})
            >>> for agent in result['agents']:
            ...     print(agent['name'], agent['id'])
        """
        try:
            client = _get_client()
        except (ImportError, ValueError) as e:
            return {"success": False, "error": str(e), "error_type": "configuration"}

        try:
            agents = client.list_agents()

            agent_list = []
            for agent in agents:
                agent_info = {
                    "id": agent.id if hasattr(agent, "id") else None,
                    "name": agent.name if hasattr(agent, "name") else None,
                }

                # Apply name filter
                if name_filter:
                    if not agent_info["name"] or name_filter not in agent_info["name"]:
                        continue

                agent_list.append(agent_info)

            return {"success": True, "agents": agent_list, "count": len(agent_list)}

        except Exception as e:
            return {"success": False, "error": str(e), "error_type": "api_error"}

    # ============================================================
    # llamaextract.get_agent - Get agent details
    # ============================================================

    def llamaextract_get_agent(
        state,
        agent_id: Optional[str] = None,
        agent_name: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get extraction agent details.

        Args:
            state: Current workflow state
            agent_id: Agent ID (preferred)
            agent_name: Agent name (alternative lookup)

        Returns:
            Dict with 'success', 'agent' containing full configuration

        Example:
            >>> result = llamaextract_get_agent(
            ...     state={},
            ...     agent_name="invoice-extractor"
            ... )
            >>> print(result['agent']['schema'])
        """
        if not agent_id and not agent_name:
            return {
                "success": False,
                "error": "Either agent_id or agent_name must be provided",
                "error_type": "validation",
            }

        try:
            client = _get_client()
        except (ImportError, ValueError) as e:
            return {"success": False, "error": str(e), "error_type": "configuration"}

        try:
            if agent_id:
                agent = client.get_agent(id=agent_id)
            else:
                # Lookup by name (SDK supports this directly)
                agent = client.get_agent(name=agent_name)

            return {
                "success": True,
                "agent": {
                    "id": agent.id if hasattr(agent, "id") else None,
                    "name": agent.name if hasattr(agent, "name") else None,
                    "schema": (
                        agent.data_schema if hasattr(agent, "data_schema") else None
                    ),
                    "config": agent.config if hasattr(agent, "config") else None,
                },
            }

        except Exception as e:
            error_str = str(e)
            error_type = "api_error"
            if "not found" in error_str.lower():
                error_type = "not_found"

            return {"success": False, "error": error_str, "error_type": error_type}

    # ============================================================
    # llamaextract.delete_agent - Delete an extraction agent
    # ============================================================

    def llamaextract_delete_agent(
        state,
        agent_id: Optional[str] = None,
        agent_name: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Delete an extraction agent.

        Args:
            state: Current workflow state
            agent_id: Agent ID (preferred)
            agent_name: Agent name (alternative lookup)

        Returns:
            Dict with 'success' and status info

        Example:
            >>> result = llamaextract_delete_agent(
            ...     state={},
            ...     agent_name="old-extractor"
            ... )
        """
        if not agent_id and not agent_name:
            return {
                "success": False,
                "error": "Either agent_id or agent_name must be provided",
                "error_type": "validation",
            }

        try:
            client = _get_client()
        except (ImportError, ValueError) as e:
            return {"success": False, "error": str(e), "error_type": "configuration"}

        try:
            # Get agent ID if only name provided
            if not agent_id:
                agent = client.get_agent(name=agent_name)
                agent_id = agent.id

            client.delete_agent(agent_id)

            return {"success": True, "deleted_agent_id": agent_id, "status": "deleted"}

        except Exception as e:
            error_str = str(e)
            error_type = "api_error"
            if "not found" in error_str.lower():
                error_type = "not_found"

            return {"success": False, "error": error_str, "error_type": error_type}

    # Register all actions
    registry["llamaextract.extract"] = llamaextract_extract
    registry["llamaextract.upload_agent"] = llamaextract_upload_agent
    registry["llamaextract.list_agents"] = llamaextract_list_agents
    registry["llamaextract.get_agent"] = llamaextract_get_agent
    registry["llamaextract.delete_agent"] = llamaextract_delete_agent

    # TEA-BUILTIN-008.7: Workflow Primitives
    registry["llamaextract.submit_job"] = llamaextract_submit_job
    registry["llamaextract.poll_status"] = llamaextract_poll_status
    registry["llamaextract.get_result"] = llamaextract_get_result

    # Also register with actions. prefix for compatibility
    registry["actions.llamaextract_extract"] = llamaextract_extract
    registry["actions.llamaextract_upload_agent"] = llamaextract_upload_agent
    registry["actions.llamaextract_list_agents"] = llamaextract_list_agents
    registry["actions.llamaextract_get_agent"] = llamaextract_get_agent
    registry["actions.llamaextract_delete_agent"] = llamaextract_delete_agent
    registry["actions.llamaextract_submit_job"] = llamaextract_submit_job
    registry["actions.llamaextract_poll_status"] = llamaextract_poll_status
    registry["actions.llamaextract_get_result"] = llamaextract_get_result
