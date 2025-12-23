"""
Web Actions for YAMLEngine.

This module provides web scraping, crawling, and search capabilities using
external APIs (Firecrawl for scraping/crawling, Perplexity for search).

This architecture enables TEA agents to run on Firebase Cloud Functions
and other serverless environments without browser dependencies.

Actions:
    - web.scrape: Extract LLM-ready content from URLs via Firecrawl API
    - web.crawl: Recursively crawl websites via Firecrawl API
    - web.search: Perform web searches via Perplexity API

Required Environment Variables:
    - FIRECRAWL_API_KEY: API key for Firecrawl (web.scrape, web.crawl)
    - PERPLEXITY_API_KEY: API key for Perplexity (web.search)

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
"""

import os
import time
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
        **kwargs
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
        api_key = os.environ.get('FIRECRAWL_API_KEY')
        if not api_key:
            return {
                "success": False,
                "error": "FIRECRAWL_API_KEY environment variable not set. "
                        "Get your API key from https://firecrawl.dev",
                "error_type": "configuration"
            }

        # Build request payload
        payload: Dict[str, Any] = {
            "url": url,
            "onlyMainContent": only_main_content,
            "timeout": timeout
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
            "Content-Type": "application/json"
        }

        try:
            response = requests.post(
                "https://api.firecrawl.dev/v1/scrape",
                json=payload,
                headers=headers,
                timeout=timeout / 1000 + 5  # Convert ms to seconds + buffer
            )

            # Handle rate limiting
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "Rate limit exceeded. Please wait and try again.",
                    "error_type": "rate_limit"
                }

            # Handle payment required
            if response.status_code == 402:
                return {
                    "success": False,
                    "error": "Insufficient Firecrawl credits. "
                            "Please top up at https://firecrawl.dev",
                    "error_type": "payment_required"
                }

            # Handle other HTTP errors
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"Firecrawl API error: {response.text}",
                    "error_type": "api_error"
                }

            # Parse response
            data = response.json()

            if not data.get("success", False):
                return {
                    "success": False,
                    "error": data.get("error", "Unknown error from Firecrawl"),
                    "error_type": "api_error"
                }

            # Build result
            result: Dict[str, Any] = {
                "success": True,
                "url": url
            }

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
                "error_type": "timeout"
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}. Check your network connection.",
                "error_type": "connection"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "api_error"
            }

    registry['web.scrape'] = web_scrape
    registry['actions.web_scrape'] = web_scrape

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
        **kwargs
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
        api_key = os.environ.get('FIRECRAWL_API_KEY')
        if not api_key:
            return {
                "success": False,
                "error": "FIRECRAWL_API_KEY environment variable not set. "
                        "Get your API key from https://firecrawl.dev",
                "error_type": "configuration"
            }

        # Build request payload
        payload: Dict[str, Any] = {
            "url": url,
            "maxDepth": max_depth,
            "limit": limit,
            "scrapeOptions": {
                "onlyMainContent": only_main_content
            }
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
            "Content-Type": "application/json"
        }

        try:
            # Start the crawl job
            response = requests.post(
                "https://api.firecrawl.dev/v1/crawl",
                json=payload,
                headers=headers,
                timeout=30
            )

            # Handle rate limiting
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "Rate limit exceeded. Please wait and try again.",
                    "error_type": "rate_limit"
                }

            # Handle payment required
            if response.status_code == 402:
                return {
                    "success": False,
                    "error": "Insufficient Firecrawl credits. "
                            "Please top up at https://firecrawl.dev",
                    "error_type": "payment_required"
                }

            # Handle other HTTP errors
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"Firecrawl API error: {response.text}",
                    "error_type": "api_error"
                }

            # Get job ID
            data = response.json()

            if not data.get("success", False):
                return {
                    "success": False,
                    "error": data.get("error", "Failed to start crawl job"),
                    "error_type": "api_error"
                }

            job_id = data.get("id")
            if not job_id:
                return {
                    "success": False,
                    "error": "No job ID returned from Firecrawl",
                    "error_type": "api_error"
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
                        "job_id": job_id
                    }

                # Check job status
                status_response = requests.get(
                    f"https://api.firecrawl.dev/v1/crawl/{job_id}",
                    headers=headers,
                    timeout=30
                )

                if status_response.status_code >= 400:
                    return {
                        "success": False,
                        "error": f"Failed to check job status: {status_response.text}",
                        "error_type": "api_error",
                        "job_id": job_id
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
                            "metadata": page_data.get("metadata", {})
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
                        "job_id": job_id
                    }

                if status == "failed":
                    return {
                        "success": False,
                        "error": status_data.get("error", "Crawl job failed"),
                        "error_type": "api_error",
                        "job_id": job_id
                    }

                # Wait before next poll
                time.sleep(poll_interval)

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": "Request timed out",
                "error_type": "timeout"
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}",
                "error_type": "connection"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "api_error"
            }

    registry['web.crawl'] = web_crawl
    registry['actions.web_crawl'] = web_crawl

    # ============================================================
    # web.search - Perplexity API
    # ============================================================

    def web_search(
        state,
        query: str,
        num_results: int = 10,
        model: str = "sonar",
        timeout: int = 60,
        **kwargs
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
        api_key = os.environ.get('PERPLEXITY_API_KEY')
        if not api_key:
            return {
                "success": False,
                "error": "PERPLEXITY_API_KEY environment variable not set. "
                        "Get your API key from https://perplexity.ai",
                "error_type": "configuration"
            }

        # Build request payload for Perplexity chat completion API
        # For deep-research, use larger max_tokens to accommodate comprehensive results
        max_tokens = 4096 if model == "sonar-deep-research" else 1024

        payload = {
            "model": model,
            "messages": [
                {
                    "role": "system",
                    "content": "You are a helpful search assistant. Provide concise, accurate search results."
                },
                {
                    "role": "user",
                    "content": query
                }
            ],
            "max_tokens": max_tokens,
            "return_citations": True,
            "return_related_questions": False
        }

        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json"
        }

        try:
            # Use provided timeout (longer for deep-research which does multi-step retrieval)
            request_timeout = timeout if model == "sonar-deep-research" else min(timeout, 30)
            response = requests.post(
                "https://api.perplexity.ai/chat/completions",
                json=payload,
                headers=headers,
                timeout=request_timeout
            )

            # Handle rate limiting
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "Rate limit exceeded. Please wait and try again.",
                    "error_type": "rate_limit"
                }

            # Handle other HTTP errors
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"Perplexity API error: {response.text}",
                    "error_type": "api_error"
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
                    results.append({
                        "title": citation,
                        "url": citation,
                        "snippet": "",
                        "position": i + 1
                    })
                elif isinstance(citation, dict):
                    results.append({
                        "title": citation.get("title", citation.get("url", "")),
                        "url": citation.get("url", ""),
                        "snippet": citation.get("snippet", citation.get("text", "")),
                        "position": i + 1
                    })

            return {
                "success": True,
                "results": results,
                "query": query,
                "total_results": len(results),
                "answer": answer,
                "model": model
            }

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": "Request timed out",
                "error_type": "timeout"
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Connection error: {str(e)}",
                "error_type": "connection"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "api_error"
            }

    registry['web.search'] = web_search
    registry['actions.web_search'] = web_search
