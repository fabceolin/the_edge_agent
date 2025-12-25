"""
Web Actions for YAMLEngine.

This module provides web scraping, crawling, and search capabilities using
external APIs (Firecrawl for scraping/crawling, Perplexity for search,
ScrapeGraphAI for AI-powered schema-based extraction).

This architecture enables TEA agents to run on Firebase Cloud Functions
and other serverless environments without browser dependencies.

Actions:
    - web.scrape: Extract LLM-ready content from URLs via Firecrawl API
    - web.crawl: Recursively crawl websites via Firecrawl API
    - web.search: Perform web searches via Perplexity API
    - web.ai_scrape: AI-powered structured extraction via ScrapeGraphAI (TEA-BUILTIN-008.4/008.7)

Required Environment Variables:
    - FIRECRAWL_API_KEY: API key for Firecrawl (web.scrape, web.crawl)
    - PERPLEXITY_API_KEY: API key for Perplexity (web.search)
    - SCRAPEGRAPH_API_KEY: API key for ScrapeGraphAI (web.ai_scrape)

Example:
    >>> # Scrape a single page
    >>> result = registry['web.scrape'](
    ...     state={},
    ...     url="https://example.com",
    ...     formats=["markdown", "links"]
    ... )
    >>> print(result['markdown'])

    >>> # Crawl a website
    >>> result = registry['web.crawl'](
    ...     state={},
    ...     url="https://example.com",
    ...     max_depth=2,
    ...     limit=10
    ... )
    >>> print(len(result['pages']))

    >>> # Search the web
    >>> result = registry['web.search'](
    ...     state={},
    ...     query="AI news 2025",
    ...     num_results=5
    ... )
    >>> for r in result['results']:
    ...     print(r['title'], r['url'])

    >>> # AI-powered structured extraction (TEA-BUILTIN-008.4)
    >>> result = registry['web.ai_scrape'](
    ...     state={},
    ...     url="https://example.com/products",
    ...     prompt="Extract product info",
    ...     output_schema={
    ...         "type": "object",
    ...         "properties": {"name": {"type": "string"}, "price": {"type": "string"}}
    ...     }
    ... )
    >>> if result['success']:
    ...     print(result['data'])

    >>> # AI-powered extraction with caching (TEA-BUILTIN-008.7)
    >>> result = registry['web.ai_scrape'](
    ...     state={},
    ...     url="https://example.com/products",
    ...     prompt="Extract product info",
    ...     output_schema={"type": "object", "properties": {"name": {"type": "string"}}},
    ...     cache={"enabled": True, "ttl_days": 30, "key_strategy": "url"}
    ... )
    >>> if result['success']:
    ...     print(f"Cache hit: {result.get('_cache_hit', False)}")
"""

import hashlib
import json
import os
import time
from datetime import datetime, timedelta, timezone
from typing import Any, Callable, Dict, List, Optional


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register web actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # ============================================================
    # web.scrape - Firecrawl Scrape API
    # ============================================================

    def web_scrape(
        state,
        url: str,
        formats: Optional[List[str]] = None,
        only_main_content: bool = True,
        wait_for: Optional[int] = None,
        timeout: int = 30000,
        actions: Optional[List[Dict]] = None,
        extract_schema: Optional[Dict] = None,
        extract_prompt: Optional[str] = None,
        mobile: bool = False,
        include_tags: Optional[List[str]] = None,
        exclude_tags: Optional[List[str]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Scrape a URL and extract LLM-ready content via Firecrawl API.

        Args:
            state: Current workflow state
            url: URL to scrape
            formats: Output formats to request. Options: "markdown", "html",
                    "links", "screenshot", "extract". Default: ["markdown"]
            only_main_content: Extract only main content, removing navigation,
                              headers, footers, etc. Default: True
            wait_for: Time in milliseconds to wait for page to load
            timeout: Request timeout in milliseconds. Default: 30000
            actions: Browser actions to perform before scraping. Each action is
                    a dict with keys like: type, selector, text, milliseconds.
                    Supported types: click, type, scroll, wait, screenshot
            extract_schema: JSON Schema for structured data extraction.
                           Uses LLM to extract data matching the schema.
            extract_prompt: Natural language prompt for data extraction.
                           Alternative to schema-based extraction.
            mobile: Use mobile viewport for scraping. Default: False
            include_tags: HTML tags to include (e.g., ["article", "main"])
            exclude_tags: HTML tags to exclude (e.g., ["nav", "footer"])

        Returns:
            On success:
            {
                "success": True,
                "url": str,
                "markdown": str,           # Clean LLM-ready markdown
                "html": str,               # If "html" in formats
                "links": List[str],        # If "links" in formats
                "screenshot": str,         # Base64 if "screenshot" in formats
                "extract": dict,           # If extract_schema/prompt provided
                "metadata": {
                    "title": str,
                    "description": str,
                    "language": str,
                    "statusCode": int
                }
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str  # configuration, rate_limit, timeout, api_error
            }

        Example:
            >>> result = web_scrape(
            ...     state={},
            ...     url="https://example.com/article",
            ...     formats=["markdown", "links"],
            ...     only_main_content=True
            ... )
            >>> if result['success']:
            ...     print(result['markdown'][:500])
        """
        import requests

        # Check for API key
        api_key = os.environ.get("FIRECRAWL_API_KEY")
        if not api_key:
            return {
                "success": False,
                "error": "FIRECRAWL_API_KEY environment variable not set. "
                "Get your API key from https://firecrawl.dev",
                "error_type": "configuration",
            }

        # Build request payload
        payload: Dict[str, Any] = {
            "url": url,
            "onlyMainContent": only_main_content,
            "timeout": timeout,
        }

        # Add formats
        if formats:
            payload["formats"] = formats

        # Add optional parameters
        if wait_for is not None:
            payload["waitFor"] = wait_for

        if actions:
            payload["actions"] = actions

        if mobile:
            payload["mobile"] = True

        if include_tags:
            payload["includeTags"] = include_tags

        if exclude_tags:
            payload["excludeTags"] = exclude_tags

        # Add extraction configuration
        if extract_schema:
            payload["extract"] = {"schema": extract_schema}
        elif extract_prompt:
            payload["extract"] = {"prompt": extract_prompt}

        # Make API request
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        try:
            response = requests.post(
                "https://api.firecrawl.dev/v1/scrape",
                json=payload,
                headers=headers,
                timeout=timeout / 1000 + 5,  # Convert ms to seconds + buffer
            )

            # Handle rate limiting
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "Rate limit exceeded. Please wait and try again.",
                    "error_type": "rate_limit",
                }

            # Handle payment required
            if response.status_code == 402:
                return {
                    "success": False,
                    "error": "Insufficient Firecrawl credits. "
                    "Please top up at https://firecrawl.dev",
                    "error_type": "payment_required",
                }

            # Handle other HTTP errors
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"Firecrawl API error: {response.text}",
                    "error_type": "api_error",
                }

            # Parse response
            data = response.json()

            if not data.get("success", False):
                return {
                    "success": False,
                    "error": data.get("error", "Unknown error from Firecrawl"),
                    "error_type": "api_error",
                }

            # Build result
            result: Dict[str, Any] = {"success": True, "url": url}

            # Extract data from response
            data_content = data.get("data", {})

            if "markdown" in data_content:
                result["markdown"] = data_content["markdown"]

            if "html" in data_content:
                result["html"] = data_content["html"]

            if "links" in data_content:
                result["links"] = data_content["links"]

            if "screenshot" in data_content:
                result["screenshot"] = data_content["screenshot"]

            if "extract" in data_content:
                result["extract"] = data_content["extract"]

            if "metadata" in data_content:
                result["metadata"] = data_content["metadata"]
            else:
                result["metadata"] = {}

            return result

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": f"Request timed out after {timeout}ms",
                "error_type": "timeout",
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}. Check your network connection.",
                "error_type": "connection",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "api_error",
            }

    registry["web.scrape"] = web_scrape
    registry["actions.web_scrape"] = web_scrape

    # ============================================================
    # web.crawl - Firecrawl Crawl API
    # ============================================================

    def web_crawl(
        state,
        url: str,
        max_depth: int = 2,
        limit: int = 10,
        formats: Optional[List[str]] = None,
        only_main_content: bool = True,
        include_paths: Optional[List[str]] = None,
        exclude_paths: Optional[List[str]] = None,
        allow_external_links: bool = False,
        poll_interval: float = 2.0,
        max_poll_time: float = 300.0,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Crawl a website recursively via Firecrawl API.

        Starts a crawl job and polls for completion. Returns content from
        all discovered pages up to the specified limit.

        Args:
            state: Current workflow state
            url: Starting URL to crawl
            max_depth: Maximum link depth to crawl. Default: 2
            limit: Maximum number of pages to crawl. Default: 10
            formats: Output formats for each page. Default: ["markdown"]
            only_main_content: Extract only main content. Default: True
            include_paths: URL patterns to include (e.g., ["/blog/*"])
            exclude_paths: URL patterns to exclude (e.g., ["/admin/*"])
            allow_external_links: Follow links to external domains. Default: False
            poll_interval: Seconds between status checks. Default: 2.0
            max_poll_time: Maximum seconds to wait for completion. Default: 300.0

        Returns:
            On success:
            {
                "success": True,
                "pages": [
                    {
                        "url": str,
                        "markdown": str,
                        "metadata": {...}
                    },
                    ...
                ],
                "total_pages": int,
                "job_id": str
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str
            }

        Example:
            >>> result = web_crawl(
            ...     state={},
            ...     url="https://docs.example.com",
            ...     max_depth=2,
            ...     limit=20,
            ...     include_paths=["/docs/*"]
            ... )
            >>> if result['success']:
            ...     for page in result['pages']:
            ...         print(f"Crawled: {page['url']}")
        """
        import requests

        # Check for API key
        api_key = os.environ.get("FIRECRAWL_API_KEY")
        if not api_key:
            return {
                "success": False,
                "error": "FIRECRAWL_API_KEY environment variable not set. "
                "Get your API key from https://firecrawl.dev",
                "error_type": "configuration",
            }

        # Build request payload
        payload: Dict[str, Any] = {
            "url": url,
            "maxDepth": max_depth,
            "limit": limit,
            "scrapeOptions": {"onlyMainContent": only_main_content},
        }

        if formats:
            payload["scrapeOptions"]["formats"] = formats

        if include_paths:
            payload["includePaths"] = include_paths

        if exclude_paths:
            payload["excludePaths"] = exclude_paths

        if allow_external_links:
            payload["allowExternalLinks"] = True

        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        try:
            # Start the crawl job
            response = requests.post(
                "https://api.firecrawl.dev/v1/crawl",
                json=payload,
                headers=headers,
                timeout=30,
            )

            # Handle rate limiting
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "Rate limit exceeded. Please wait and try again.",
                    "error_type": "rate_limit",
                }

            # Handle payment required
            if response.status_code == 402:
                return {
                    "success": False,
                    "error": "Insufficient Firecrawl credits. "
                    "Please top up at https://firecrawl.dev",
                    "error_type": "payment_required",
                }

            # Handle other HTTP errors
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"Firecrawl API error: {response.text}",
                    "error_type": "api_error",
                }

            # Get job ID
            data = response.json()

            if not data.get("success", False):
                return {
                    "success": False,
                    "error": data.get("error", "Failed to start crawl job"),
                    "error_type": "api_error",
                }

            job_id = data.get("id")
            if not job_id:
                return {
                    "success": False,
                    "error": "No job ID returned from Firecrawl",
                    "error_type": "api_error",
                }

            # Poll for completion
            start_time = time.time()
            while True:
                elapsed = time.time() - start_time
                if elapsed > max_poll_time:
                    return {
                        "success": False,
                        "error": f"Crawl job timed out after {max_poll_time}s",
                        "error_type": "timeout",
                        "job_id": job_id,
                    }

                # Check job status
                status_response = requests.get(
                    f"https://api.firecrawl.dev/v1/crawl/{job_id}",
                    headers=headers,
                    timeout=30,
                )

                if status_response.status_code >= 400:
                    return {
                        "success": False,
                        "error": f"Failed to check job status: {status_response.text}",
                        "error_type": "api_error",
                        "job_id": job_id,
                    }

                status_data = status_response.json()
                status = status_data.get("status")

                if status == "completed":
                    # Extract pages from response
                    pages = []
                    for page_data in status_data.get("data", []):
                        page = {
                            "url": page_data.get("metadata", {}).get("sourceURL", ""),
                            "markdown": page_data.get("markdown", ""),
                            "metadata": page_data.get("metadata", {}),
                        }
                        if page_data.get("html"):
                            page["html"] = page_data["html"]
                        if page_data.get("links"):
                            page["links"] = page_data["links"]
                        pages.append(page)

                    return {
                        "success": True,
                        "pages": pages,
                        "total_pages": len(pages),
                        "job_id": job_id,
                    }

                if status == "failed":
                    return {
                        "success": False,
                        "error": status_data.get("error", "Crawl job failed"),
                        "error_type": "api_error",
                        "job_id": job_id,
                    }

                # Wait before next poll
                time.sleep(poll_interval)

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": "Request timed out",
                "error_type": "timeout",
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}",
                "error_type": "connection",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "api_error",
            }

    registry["web.crawl"] = web_crawl
    registry["actions.web_crawl"] = web_crawl

    # ============================================================
    # web.search - Perplexity API
    # ============================================================

    def web_search(
        state,
        query: str,
        num_results: int = 10,
        model: str = "sonar",
        timeout: int = 60,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Perform web search via Perplexity API.

        Uses Perplexity's sonar model family to search the web and return
        structured results with citations.

        Args:
            state: Current workflow state
            query: Search query string
            num_results: Maximum number of results to return. Default: 10
            model: Perplexity model to use. Options:
                   - "sonar" (default): Fast, basic search
                   - "sonar-pro": Enhanced search
                   - "sonar-deep-research": Multi-step retrieval with reasoning
            timeout: Request timeout in seconds. Default: 60 (increase for deep-research)

        Returns:
            On success:
            {
                "success": True,
                "results": [
                    {
                        "title": str,
                        "url": str,
                        "snippet": str,
                        "position": int
                    },
                    ...
                ],
                "query": str,
                "total_results": int,
                "answer": str  # Perplexity's synthesized answer
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str
            }

        Example:
            >>> result = web_search(
            ...     state={},
            ...     query="latest AI developments 2025",
            ...     num_results=5
            ... )
            >>> if result['success']:
            ...     print(result['answer'])
            ...     for r in result['results']:
            ...         print(f"- {r['title']}: {r['url']}")
        """
        import requests

        # Check for API key
        api_key = os.environ.get("PERPLEXITY_API_KEY")
        if not api_key:
            return {
                "success": False,
                "error": "PERPLEXITY_API_KEY environment variable not set. "
                "Get your API key from https://perplexity.ai",
                "error_type": "configuration",
            }

        # Build request payload for Perplexity chat completion API
        # For deep-research, use larger max_tokens to accommodate comprehensive results
        max_tokens = 4096 if model == "sonar-deep-research" else 1024

        payload = {
            "model": model,
            "messages": [
                {
                    "role": "system",
                    "content": "You are a helpful search assistant. Provide concise, accurate search results.",
                },
                {"role": "user", "content": query},
            ],
            "max_tokens": max_tokens,
            "return_citations": True,
            "return_related_questions": False,
        }

        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        try:
            # Use provided timeout (longer for deep-research which does multi-step retrieval)
            request_timeout = (
                timeout if model == "sonar-deep-research" else min(timeout, 30)
            )
            response = requests.post(
                "https://api.perplexity.ai/chat/completions",
                json=payload,
                headers=headers,
                timeout=request_timeout,
            )

            # Handle rate limiting
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "Rate limit exceeded. Please wait and try again.",
                    "error_type": "rate_limit",
                }

            # Handle other HTTP errors
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"Perplexity API error: {response.text}",
                    "error_type": "api_error",
                }

            # Parse response
            data = response.json()

            # Extract answer from chat completion
            answer = ""
            if "choices" in data and len(data["choices"]) > 0:
                message = data["choices"][0].get("message", {})
                answer = message.get("content", "")

            # Extract citations/sources
            results = []
            citations = data.get("citations", [])
            for i, citation in enumerate(citations[:num_results]):
                if isinstance(citation, str):
                    # Citation is just a URL
                    results.append(
                        {
                            "title": citation,
                            "url": citation,
                            "snippet": "",
                            "position": i + 1,
                        }
                    )
                elif isinstance(citation, dict):
                    results.append(
                        {
                            "title": citation.get("title", citation.get("url", "")),
                            "url": citation.get("url", ""),
                            "snippet": citation.get(
                                "snippet", citation.get("text", "")
                            ),
                            "position": i + 1,
                        }
                    )

            return {
                "success": True,
                "results": results,
                "query": query,
                "total_results": len(results),
                "answer": answer,
                "model": model,
            }

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": "Request timed out",
                "error_type": "timeout",
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}",
                "error_type": "connection",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "api_error",
            }

    registry["web.search"] = web_search
    registry["actions.web_search"] = web_search

    # ============================================================
    # Cache helpers for web.ai_scrape (TEA-BUILTIN-008.7)
    # ============================================================

    def _compute_ai_scrape_cache_key(
        url: str, prompt: str, final_schema: Optional[Dict], key_strategy: str
    ) -> str:
        """
        Compute cache key for web.ai_scrape based on strategy.

        Args:
            url: URL being scraped
            prompt: Extraction prompt
            final_schema: Resolved schema dict
            key_strategy: One of 'url', 'url+schema', 'url+prompt', 'url+prompt+schema'

        Returns:
            Cache key string prefixed with 'cache:web.ai_scrape:'
        """
        prefix = "cache:web.ai_scrape:"

        if key_strategy == "url":
            # Hash URL only - same URL always returns cached result
            content_hash = hashlib.sha256(url.encode()).hexdigest()
            return f"{prefix}{content_hash}"

        elif key_strategy == "url+schema":
            # Hash URL + schema - different schemas get separate entries
            schema_json = json.dumps(final_schema or {}, sort_keys=True, default=str)
            combined = f"{url}|{schema_json}"
            content_hash = hashlib.sha256(combined.encode()).hexdigest()
            return f"{prefix}{content_hash}"

        elif key_strategy == "url+prompt":
            # Hash URL + prompt - different prompts get separate entries
            combined = f"{url}|{prompt}"
            content_hash = hashlib.sha256(combined.encode()).hexdigest()
            return f"{prefix}{content_hash}"

        else:  # url+prompt+schema (default, most granular)
            # Hash URL + prompt + schema - all three must match
            schema_json = json.dumps(final_schema or {}, sort_keys=True, default=str)
            combined = f"{url}|{prompt}|{schema_json}"
            content_hash = hashlib.sha256(combined.encode()).hexdigest()
            return f"{prefix}{content_hash}"

    def _is_cache_expired(expires_at: Optional[str]) -> bool:
        """
        Check if a cache entry has expired.

        Args:
            expires_at: ISO format timestamp string or None

        Returns:
            True if expired or no expiration time, False otherwise
        """
        if not expires_at:
            return True

        try:
            # Parse ISO format timestamp
            ts = expires_at
            if ts.endswith("Z"):
                ts = ts[:-1] + "+00:00"
            # Handle double timezone
            if "+00:00+00:00" in ts:
                ts = ts.replace("+00:00+00:00", "+00:00")

            expiry_time = datetime.fromisoformat(ts)
            now = datetime.now(timezone.utc)
            if expiry_time.tzinfo is None:
                expiry_time = expiry_time.replace(tzinfo=timezone.utc)
            return now >= expiry_time
        except (ValueError, TypeError):
            return True

    def _compute_cache_ttl_seconds(cache_config: Dict[str, Any]) -> int:
        """
        Compute cache TTL in seconds from cache config.

        Priority: ttl_seconds > ttl_hours > ttl_days > default (60 days)
        """
        if cache_config.get("ttl_seconds") is not None:
            return cache_config["ttl_seconds"]
        if cache_config.get("ttl_hours") is not None:
            return cache_config["ttl_hours"] * 3600
        if cache_config.get("ttl_days") is not None:
            return cache_config["ttl_days"] * 86400
        # Default: 60 days
        return 60 * 86400

    def _try_cache_lookup(cache_key: str) -> Optional[Dict[str, Any]]:
        """
        Attempt to retrieve cached result from LTM.

        Args:
            cache_key: Cache key to look up

        Returns:
            Cached result dict if found and not expired, None otherwise
        """
        try:
            if not hasattr(engine, "_ltm_backend") or engine._ltm_backend is None:
                return None

            cache_result = engine._ltm_backend.retrieve(key=cache_key)
            if not cache_result.get("found"):
                return None

            metadata = cache_result.get("metadata", {})
            expires_at = metadata.get("_cache_expires_at")

            if _is_cache_expired(expires_at):
                return None

            # Cache hit!
            cached_value = cache_result.get("value", {})
            return {
                "result": cached_value.get("result", cached_value),
                "created_at": metadata.get("_cache_created_at"),
            }
        except Exception:
            # LTM failure is graceful - return None to proceed with API call
            return None

    def _store_in_cache(
        cache_key: str, result: Dict[str, Any], cache_config: Dict[str, Any]
    ) -> None:
        """
        Store successful scrape result in LTM cache.

        Args:
            cache_key: Cache key to store under
            result: Successful scrape result
            cache_config: Cache configuration dict
        """
        try:
            if not hasattr(engine, "_ltm_backend") or engine._ltm_backend is None:
                return

            now = datetime.now(timezone.utc)
            ttl_sec = _compute_cache_ttl_seconds(cache_config)
            expires_at = now + timedelta(seconds=ttl_sec)

            engine._ltm_backend.store(
                key=cache_key,
                value={"result": result},
                metadata={
                    "_cache_type": "web.ai_scrape",
                    "_cache_key": cache_key,
                    "_cache_created_at": now.isoformat(),
                    "_cache_expires_at": expires_at.isoformat(),
                },
            )
        except Exception:
            # LTM store failure is graceful - don't fail the scrape
            pass

    # ============================================================
    # web.ai_scrape - ScrapeGraphAI API (TEA-BUILTIN-008.4/008.7)
    # ============================================================

    def web_ai_scrape(
        state,
        url: str,
        prompt: str,
        output_schema: Optional[Dict[str, Any]] = None,
        schema: Optional[Dict[str, Any]] = None,
        timeout: int = 60,
        max_retries: int = 3,
        cache: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Extract structured data from a URL using ScrapeGraphAI.

        Uses AI-powered extraction to parse web pages and return data
        matching a provided Pydantic/JSON schema. Supports optional LTM
        caching to avoid redundant API calls (TEA-BUILTIN-008.7).

        Args:
            state: Current workflow state
            url: URL to scrape
            prompt: Natural language prompt describing what to extract
            output_schema: JSON Schema dict for structured output (inline)
            schema: Schema configuration with optional `uses` for Git refs
                   or fsspec URIs (s3://, gs://, az://, https://, file://)
                   Supports merging via list of references.
            timeout: Request timeout in seconds. Default: 60
            max_retries: Maximum retry attempts for rate limits/server errors. Default: 3
            cache: Optional cache configuration dict (TEA-BUILTIN-008.7):
                   - enabled: bool (default: False) - Enable caching
                   - ttl_days: int (default: 60) - Cache TTL in days
                   - ttl_hours: int - Cache TTL in hours (overrides ttl_days)
                   - ttl_seconds: int - Cache TTL in seconds (overrides ttl_hours)
                   - key_strategy: str - Cache key generation strategy:
                       * "url" (default): Hash of URL only
                       * "url+schema": Hash of URL + schema
                       * "url+prompt": Hash of URL + prompt
                       * "url+prompt+schema": Hash of URL + prompt + schema (most unique)
                   - skip_cache: bool (default: False) - Force fresh scrape, ignore cache

        Returns:
            On success:
            {
                "success": True,
                "data": {...},           # Extracted data matching schema
                "url": str,
                "schema_used": {...},    # Final merged schema
                "_cache_hit": bool,      # True if result came from cache
                "_cache_key": str,       # Cache key used
                "_cache_created_at": str # ISO timestamp if cache hit
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str  # configuration, api_error, schema_error,
                                   # timeout, rate_limit, dependency, authentication
            }

        Schema Sources (via Story 008.2):
            - Inline dict via output_schema parameter
            - Git refs: "owner/repo@ref#path/to/schema.json"
            - Git full URLs: "git+https://..." or "git+ssh://..."
            - fsspec URIs: s3://, gs://, az://, https://, file://

        Schema Merging (via Story 008.3):
            When schema.uses is a list, schemas are merged with kubectl-style
            semantics (last wins). First schema = lowest priority.

        Caching (via Story 008.7):
            When cache.enabled=True, results are stored in Long-Term Memory
            with configurable TTL. Cache key strategies determine uniqueness:
            - "url": Same URL always returns cached result
            - "url+prompt": Different prompts for same URL get separate cache entries
            - "url+schema": Different schemas for same URL get separate cache entries
            - "url+prompt+schema": Most granular - all three must match

        Example:
            >>> result = web_ai_scrape(
            ...     state={},
            ...     url="https://example.com/products",
            ...     prompt="Extract all products with prices",
            ...     output_schema={
            ...         "type": "object",
            ...         "properties": {
            ...             "products": {
            ...                 "type": "array",
            ...                 "items": {
            ...                     "type": "object",
            ...                     "properties": {
            ...                         "name": {"type": "string"},
            ...                         "price": {"type": "string"}
            ...                     }
            ...                 }
            ...             }
            ...         }
            ...     },
            ...     cache={"enabled": True, "ttl_days": 30}
            ... )
            >>> if result['success']:
            ...     for product in result['data']['products']:
            ...         print(f"{product['name']}: {product['price']}")
        """
        # Parse cache configuration (TEA-BUILTIN-008.7)
        cache_config = cache or {}
        cache_enabled = cache_config.get("enabled", False)
        skip_cache = cache_config.get("skip_cache", False)
        key_strategy = cache_config.get("key_strategy", "url")

        # Check for API key
        api_key = os.environ.get("SCRAPEGRAPH_API_KEY")
        if not api_key:
            return {
                "success": False,
                "error": "SCRAPEGRAPH_API_KEY environment variable not set. "
                "Get your API key from https://scrapegraphai.com",
                "error_type": "configuration",
            }

        # Try to import required packages
        try:
            from scrapegraph_py import Client
        except ImportError:
            return {
                "success": False,
                "error": "scrapegraph-py package not installed. "
                "Install with: pip install scrapegraph-py",
                "error_type": "dependency",
            }

        try:
            from pydantic import BaseModel, Field, create_model
        except ImportError:
            return {
                "success": False,
                "error": "pydantic package not installed. "
                "Install with: pip install pydantic",
                "error_type": "dependency",
            }

        # Resolve schema (handles Git refs, fsspec URIs, caching, and merging)
        try:
            final_schema = _resolve_ai_scrape_schema(output_schema, schema)
        except Exception as e:
            return {
                "success": False,
                "error": f"Schema resolution failed: {str(e)}",
                "error_type": "schema_error",
            }

        if final_schema is None:
            return {
                "success": False,
                "error": "No schema provided. Use output_schema or schema.uses",
                "error_type": "schema_error",
            }

        # Compute cache key (needed for both lookup and store)
        cache_key = None
        if cache_enabled:
            cache_key = _compute_ai_scrape_cache_key(
                url=url,
                prompt=prompt,
                final_schema=final_schema,
                key_strategy=key_strategy,
            )

            # Try cache lookup (unless skip_cache is True)
            if not skip_cache:
                cached = _try_cache_lookup(cache_key)
                if cached is not None:
                    # Cache hit! Return cached result with metadata
                    cached_result = cached["result"]
                    return {
                        "success": True,
                        "data": cached_result.get("data", cached_result),
                        "url": url,
                        "schema_used": final_schema,
                        "_cache_hit": True,
                        "_cache_key": cache_key,
                        "_cache_created_at": cached["created_at"],
                    }

        # Convert JSON Schema to Pydantic model
        try:
            pydantic_model = _json_schema_to_pydantic(final_schema)
        except Exception as e:
            return {
                "success": False,
                "error": f"Invalid schema: {str(e)}",
                "error_type": "schema_error",
            }

        # Call ScrapeGraphAI API with retry logic (pattern from 008.5)
        client = Client(api_key=api_key)
        result = _call_scrapegraph_with_retry(
            client=client,
            url=url,
            prompt=prompt,
            pydantic_model=pydantic_model,
            final_schema=final_schema,
            max_retries=max_retries,
        )

        # Store in cache if successful and caching is enabled
        if result.get("success") and cache_enabled and cache_key:
            _store_in_cache(cache_key, result, cache_config)

        # Add cache metadata to response
        if cache_enabled:
            result["_cache_hit"] = False
            result["_cache_key"] = cache_key

        return result

    def _resolve_ai_scrape_schema(
        output_schema: Optional[Dict], schema_config: Optional[Dict]
    ) -> Optional[Dict]:
        """
        Resolve schema from inline dict, Git references, or fsspec URIs.

        Uses unified schema loader from Story 008.2 which handles:
        - Git short refs: owner/repo@ref#path
        - Git full URLs: git+https://... or git+ssh://...
        - fsspec URIs: s3://, gs://, az://, https://, file://
        - Caching with 5-minute TTL

        Priority:
        1. output_schema (inline dict) - used directly
        2. schema.uses (ref or list of refs) - loaded and merged
        3. schema.inline (inline within config)
        """
        # Direct inline schema (highest priority, no loading needed)
        if output_schema:
            return output_schema

        # Schema config with uses
        if schema_config:
            uses = schema_config.get("uses")
            if uses:
                # Import schema utilities from 008.2/008.3
                try:
                    from the_edge_agent.schema import fetch_schema
                    from the_edge_agent.schema.deep_merge import merge_all
                except ImportError as e:
                    raise ImportError(f"Schema loading requires the schema module: {e}")

                # Normalize to list
                refs = uses if isinstance(uses, list) else [uses]

                # Load all schemas using unified loader (handles Git + fsspec + caching)
                schemas = []
                for ref in refs:
                    loaded_schema = fetch_schema(ref)
                    schemas.append(loaded_schema)

                # Merge schemas (first = lowest priority, kubectl-style)
                if len(schemas) == 1:
                    return schemas[0]
                return merge_all(schemas)

            # Inline schema within config
            if "inline" in schema_config:
                return schema_config["inline"]

        return None

    def _json_schema_to_pydantic(schema: Dict[str, Any]):
        """
        Dynamically create Pydantic model from JSON Schema.

        Handles common JSON Schema types:
        - string -> str
        - integer -> int
        - number -> float
        - boolean -> bool
        - array -> List[...]
        - object -> nested model
        """
        from pydantic import BaseModel, Field, create_model
        from typing import (
            List as TypingList,
            Optional as TypingOptional,
            Any as TypingAny,
        )

        def get_python_type(prop: Dict) -> TypingAny:
            """Convert JSON Schema type to Python type."""
            schema_type = prop.get("type", "string")

            if schema_type == "string":
                return str
            elif schema_type == "integer":
                return int
            elif schema_type == "number":
                return float
            elif schema_type == "boolean":
                return bool
            elif schema_type == "array":
                items = prop.get("items", {})
                item_type = get_python_type(items)
                return TypingList[item_type]
            elif schema_type == "object":
                # Recursive: create nested model
                nested_props = prop.get("properties", {})
                if nested_props:
                    return _create_model_from_properties(nested_props)
                return dict
            else:
                return TypingAny

        def _create_model_from_properties(properties: Dict):
            """Create Pydantic model from properties dict."""
            fields = {}
            for name, prop in properties.items():
                python_type = get_python_type(prop)
                description = prop.get("description", "")
                default = prop.get("default", ...)
                fields[name] = (
                    python_type,
                    Field(default=default, description=description),
                )

            return create_model("DynamicSchema", **fields)

        # Root must be object type
        if schema.get("type") != "object":
            schema = {"type": "object", "properties": schema}

        properties = schema.get("properties", {})
        return _create_model_from_properties(properties)

    def _call_scrapegraph_with_retry(
        client,
        url: str,
        prompt: str,
        pydantic_model,
        final_schema: Dict,
        max_retries: int = 3,
    ) -> Dict[str, Any]:
        """
        Call ScrapeGraphAI API with exponential backoff retry.

        Follows retry pattern from TEA-BUILTIN-008.5 (LlamaExtract REST API).
        """
        for attempt in range(max_retries):
            try:
                response = client.smartscraper(
                    website_url=url, user_prompt=prompt, output_schema=pydantic_model
                )

                # Response is already structured according to schema
                # Handle both dict and Pydantic model responses
                if isinstance(response, dict):
                    data = response
                elif hasattr(response, "model_dump"):
                    data = response.model_dump()
                elif hasattr(response, "dict"):
                    data = response.dict()  # Pydantic v1 compatibility
                else:
                    data = dict(response) if response else {}

                return {
                    "success": True,
                    "data": data,
                    "url": url,
                    "schema_used": final_schema,
                }

            except Exception as e:
                error_msg = str(e).lower()

                # Retry on rate limit (429) or server errors (5xx)
                if "rate limit" in error_msg or "429" in error_msg:
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)  # Exponential backoff: 1s, 2s, 4s
                        continue
                    return {
                        "success": False,
                        "error": "Rate limit exceeded after retries",
                        "error_type": "rate_limit",
                    }

                if "500" in error_msg or "502" in error_msg or "503" in error_msg:
                    if attempt < max_retries - 1:
                        time.sleep(2**attempt)
                        continue
                    return {
                        "success": False,
                        "error": f"Server error after retries: {str(e)}",
                        "error_type": "api_error",
                    }

                # Non-retryable errors
                if "timeout" in error_msg:
                    return {
                        "success": False,
                        "error": f"Request timeout: {str(e)}",
                        "error_type": "timeout",
                    }
                if (
                    "api key" in error_msg
                    or "unauthorized" in error_msg
                    or "401" in error_msg
                ):
                    return {
                        "success": False,
                        "error": f"Authentication failed: {str(e)}",
                        "error_type": "authentication",
                    }

                # Generic API error - don't retry
                return {
                    "success": False,
                    "error": f"ScrapeGraphAI error: {str(e)}",
                    "error_type": "api_error",
                }

        return {
            "success": False,
            "error": "Max retries exceeded",
            "error_type": "api_error",
        }

    registry["web.ai_scrape"] = web_ai_scrape
    registry["actions.web_ai_scrape"] = web_ai_scrape
