"""
LlamaExtract Actions for YAMLEngine.

This module provides document extraction capabilities using LlamaExtract's
AI-powered extraction API.

TEA-BUILTIN-008.1: Core LlamaExtract Actions

Actions:
    - llamaextract.extract: Extract structured data from documents
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
import os
import time
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Union


# Extraction mode enum values
EXTRACTION_MODES = ["BALANCED", "MULTIMODAL", "PREMIUM", "FAST"]


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register llamaextract actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def _get_api_key() -> Optional[str]:
        """Get API key from environment variables."""
        return os.environ.get('LLAMAEXTRACT_API_KEY') or os.environ.get('LLAMAPARSE_API_KEY')

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

        return LlamaExtract(api_key=api_key)

    def _get_extract_mode(mode: str):
        """Get ExtractMode enum value."""
        try:
            from llama_cloud.types import ExtractMode
        except ImportError:
            raise ImportError(
                "llama-cloud package not installed. "
                "Install with: pip install llama-cloud"
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
                f"Invalid extraction mode: {mode}. "
                f"Valid modes: {EXTRACTION_MODES}"
            )

    def _retry_with_backoff(
        func: Callable,
        max_retries: int = 3,
        base_delay: float = 1.0,
        max_delay: float = 30.0
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
                    delay = min(base_delay * (2 ** attempt), max_delay)
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
        max_retries: int = 3,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Extract structured data from a document using LlamaExtract.

        Args:
            state: Current workflow state
            file: Document source - URL, base64 content, or local file path
            schema: JSON Schema defining the structure to extract.
                   Either schema or agent_id/agent_name must be provided.
            agent_id: ID of existing extraction agent to use
            agent_name: Name of existing extraction agent to use
            mode: Extraction mode - BALANCED, MULTIMODAL, PREMIUM, or FAST
            max_retries: Maximum retry attempts for transient failures

        Returns:
            Dict with 'success', 'data' (extracted data), 'job_id', etc.

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
            ...     mode="PREMIUM"
            ... )
        """
        # Validate inputs
        if not file:
            return {
                "success": False,
                "error": "file parameter is required",
                "error_type": "validation"
            }

        if not schema and not agent_id and not agent_name:
            return {
                "success": False,
                "error": "Either schema, agent_id, or agent_name must be provided",
                "error_type": "validation"
            }

        try:
            client = _get_client()
            extract_mode = _get_extract_mode(mode)
        except ImportError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "dependency"
            }
        except ValueError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "configuration"
            }

        try:
            from llama_cloud.types import ExtractConfig
        except ImportError:
            return {
                "success": False,
                "error": "llama-cloud package not installed",
                "error_type": "dependency"
            }

        # Prepare file content
        file_content = None
        file_url = None

        if file.startswith(('http://', 'https://')):
            file_url = file
        elif file.startswith('data:') or ';base64,' in file:
            # Base64 encoded content
            if ';base64,' in file:
                file_content = file.split(';base64,')[1]
            else:
                file_content = file
        else:
            # Local file path
            path = Path(file)
            if not path.exists():
                return {
                    "success": False,
                    "error": f"File not found: {file}",
                    "error_type": "file_not_found"
                }
            with open(path, 'rb') as f:
                file_content = base64.b64encode(f.read()).decode('utf-8')

        def do_extract():
            # Build extraction config
            config = ExtractConfig(extraction_mode=extract_mode)

            if agent_id:
                # Use existing agent by ID
                result = client.extract(
                    agent_id=agent_id,
                    file_url=file_url,
                    file_base64=file_content,
                    config=config
                )
            elif agent_name:
                # Use existing agent by name
                result = client.extract(
                    agent_name=agent_name,
                    file_url=file_url,
                    file_base64=file_content,
                    config=config
                )
            else:
                # Use inline schema
                result = client.extract(
                    data_schema=schema,
                    file_url=file_url,
                    file_base64=file_content,
                    config=config
                )
            return result

        try:
            result = _retry_with_backoff(do_extract, max_retries=max_retries)

            return {
                "success": True,
                "data": result.data if hasattr(result, 'data') else result,
                "job_id": result.job_id if hasattr(result, 'job_id') else None,
                "status": "completed"
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

            return {
                "success": False,
                "error": error_str,
                "error_type": error_type
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
        **kwargs
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
                "error_type": "validation"
            }

        if not schema:
            return {
                "success": False,
                "error": "schema parameter is required",
                "error_type": "validation"
            }

        try:
            client = _get_client()
            extract_mode = _get_extract_mode(mode)
        except (ImportError, ValueError) as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "configuration"
            }

        try:
            from llama_cloud.types import ExtractConfig
        except ImportError:
            return {
                "success": False,
                "error": "llama-cloud package not installed",
                "error_type": "dependency"
            }

        def do_upload():
            config = ExtractConfig(extraction_mode=extract_mode)

            # Check if agent exists
            existing_agents = client.list_agents()
            existing = next((a for a in existing_agents if a.name == name), None)

            if existing:
                if not force:
                    raise ValueError(
                        f"Agent '{name}' already exists. Use force=True to update."
                    )
                # Update existing agent
                result = client.update_agent(
                    agent_id=existing.id,
                    name=name,
                    data_schema=schema,
                    config=config
                )
                return {"agent": result, "action": "updated"}
            else:
                # Create new agent
                result = client.create_agent(
                    name=name,
                    data_schema=schema,
                    config=config
                )
                return {"agent": result, "action": "created"}

        try:
            result = _retry_with_backoff(do_upload, max_retries=max_retries)

            agent = result["agent"]
            return {
                "success": True,
                "agent_id": agent.id if hasattr(agent, 'id') else None,
                "name": name,
                "action": result["action"],
                "status": "active"
            }

        except ValueError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "validation"
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "api_error"
            }

    # ============================================================
    # llamaextract.list_agents - List available extraction agents
    # ============================================================

    def llamaextract_list_agents(
        state,
        name_filter: Optional[str] = None,
        **kwargs
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
            return {
                "success": False,
                "error": str(e),
                "error_type": "configuration"
            }

        try:
            agents = client.list_agents()

            agent_list = []
            for agent in agents:
                agent_info = {
                    "id": agent.id if hasattr(agent, 'id') else None,
                    "name": agent.name if hasattr(agent, 'name') else None,
                }

                # Apply name filter
                if name_filter:
                    if not agent_info["name"] or name_filter not in agent_info["name"]:
                        continue

                agent_list.append(agent_info)

            return {
                "success": True,
                "agents": agent_list,
                "count": len(agent_list)
            }

        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "api_error"
            }

    # ============================================================
    # llamaextract.get_agent - Get agent details
    # ============================================================

    def llamaextract_get_agent(
        state,
        agent_id: Optional[str] = None,
        agent_name: Optional[str] = None,
        **kwargs
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
                "error_type": "validation"
            }

        try:
            client = _get_client()
        except (ImportError, ValueError) as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "configuration"
            }

        try:
            if agent_id:
                agent = client.get_agent(agent_id=agent_id)
            else:
                # Lookup by name
                agents = client.list_agents()
                agent = next((a for a in agents if a.name == agent_name), None)
                if not agent:
                    return {
                        "success": False,
                        "error": f"Agent not found: {agent_name}",
                        "error_type": "not_found"
                    }
                # Get full details
                agent = client.get_agent(agent_id=agent.id)

            return {
                "success": True,
                "agent": {
                    "id": agent.id if hasattr(agent, 'id') else None,
                    "name": agent.name if hasattr(agent, 'name') else None,
                    "schema": agent.data_schema if hasattr(agent, 'data_schema') else None,
                    "config": agent.config if hasattr(agent, 'config') else None,
                }
            }

        except Exception as e:
            error_str = str(e)
            error_type = "api_error"
            if "not found" in error_str.lower():
                error_type = "not_found"

            return {
                "success": False,
                "error": error_str,
                "error_type": error_type
            }

    # ============================================================
    # llamaextract.delete_agent - Delete an extraction agent
    # ============================================================

    def llamaextract_delete_agent(
        state,
        agent_id: Optional[str] = None,
        agent_name: Optional[str] = None,
        **kwargs
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
                "error_type": "validation"
            }

        try:
            client = _get_client()
        except (ImportError, ValueError) as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "configuration"
            }

        try:
            # Get agent ID if only name provided
            if not agent_id:
                agents = client.list_agents()
                agent = next((a for a in agents if a.name == agent_name), None)
                if not agent:
                    return {
                        "success": False,
                        "error": f"Agent not found: {agent_name}",
                        "error_type": "not_found"
                    }
                agent_id = agent.id

            client.delete_agent(agent_id=agent_id)

            return {
                "success": True,
                "deleted_agent_id": agent_id,
                "status": "deleted"
            }

        except Exception as e:
            error_str = str(e)
            error_type = "api_error"
            if "not found" in error_str.lower():
                error_type = "not_found"

            return {
                "success": False,
                "error": error_str,
                "error_type": error_type
            }

    # Register all actions
    registry['llamaextract.extract'] = llamaextract_extract
    registry['llamaextract.upload_agent'] = llamaextract_upload_agent
    registry['llamaextract.list_agents'] = llamaextract_list_agents
    registry['llamaextract.get_agent'] = llamaextract_get_agent
    registry['llamaextract.delete_agent'] = llamaextract_delete_agent

    # Also register with actions. prefix for compatibility
    registry['actions.llamaextract_extract'] = llamaextract_extract
    registry['actions.llamaextract_upload_agent'] = llamaextract_upload_agent
    registry['actions.llamaextract_list_agents'] = llamaextract_list_agents
    registry['actions.llamaextract_get_agent'] = llamaextract_get_agent
    registry['actions.llamaextract_delete_agent'] = llamaextract_delete_agent
