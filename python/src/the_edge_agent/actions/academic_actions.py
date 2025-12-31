"""
Academic Research Actions for YAMLEngine.

This module provides actions for searching academic databases including
PubMed (NCBI), ArXiv, and CrossRef for research papers and articles.

Actions:
    - academic.pubmed: Search PubMed database via NCBI E-utilities API
    - academic.arxiv: Search ArXiv preprint server via ArXiv API
    - academic.crossref: Query CrossRef API for DOI metadata and search

Required Environment Variables:
    - NCBI_API_KEY: Optional API key for PubMed (increases rate limit from 3 to 10 req/s)

Rate Limits:
    - PubMed: 3 requests/second (10/s with API key)
    - ArXiv: 1 request/3 seconds
    - CrossRef: 1 request/second (50/s with mailto for polite pool)

Example:
    >>> # Search PubMed
    >>> result = registry['academic.pubmed'](
    ...     state={},
    ...     query="machine learning cancer diagnosis",
    ...     max_results=5
    ... )
    >>> for article in result['results']:
    ...     print(f"{article['title']} - {article['journal']}")

    >>> # Search ArXiv
    >>> result = registry['academic.arxiv'](
    ...     state={},
    ...     query="transformer neural networks",
    ...     max_results=10
    ... )
    >>> for paper in result['results']:
    ...     print(f"{paper['title']} - {paper['pdf_url']}")

    >>> # Lookup DOI via CrossRef
    >>> result = registry['academic.crossref'](
    ...     state={},
    ...     doi="10.1038/nature12373",
    ... )
    >>> if result['success']:
    ...     print(f"{result['results'][0]['title']} - {result['results'][0]['container_title']}")
"""

import logging
import os
import re
import threading
import time
import xml.etree.ElementTree as ET
from typing import Any, Callable, Dict, List, Optional
from urllib.parse import quote_plus, urlencode

logger = logging.getLogger(__name__)


class RateLimiter:
    """
    Thread-safe rate limiter using a lock to ensure correct timing.

    This ensures that concurrent calls from parallel YAML nodes
    respect the rate limit correctly.
    """

    def __init__(self, min_interval: float):
        """
        Initialize the rate limiter.

        Args:
            min_interval: Minimum time in seconds between requests.
        """
        self._lock = threading.Lock()
        self._last_request: float = 0.0
        self._min_interval = min_interval

    def wait(self) -> None:
        """
        Wait if necessary to respect the rate limit.

        This method is thread-safe and ensures only one thread
        can update the timing at a time.
        """
        with self._lock:
            now = time.time()
            elapsed = now - self._last_request
            if elapsed < self._min_interval:
                time.sleep(self._min_interval - elapsed)
            self._last_request = time.time()

    def update_interval(self, new_interval: float) -> None:
        """Update the minimum interval (thread-safe)."""
        with self._lock:
            self._min_interval = new_interval


def _request_with_backoff(
    url: str,
    params: Dict[str, Any],
    timeout: int,
    max_retries: int = 3,
    base_delay: float = 2.0,
    headers: Optional[Dict[str, str]] = None,
) -> "requests.Response":
    """
    Make an HTTP GET request with exponential backoff on 429 responses.

    Args:
        url: The URL to request.
        params: Query parameters.
        timeout: Request timeout in seconds.
        max_retries: Maximum number of retries on 429. Default: 3.
        base_delay: Base delay in seconds. Default: 2.0.
        headers: Optional HTTP headers to include in the request.

    Returns:
        The response object (may be 429 if retries exhausted).
    """
    import requests

    response = None
    for attempt in range(max_retries + 1):
        response = requests.get(url, params=params, timeout=timeout, headers=headers)
        if response.status_code != 429:
            return response
        if attempt < max_retries:
            delay = base_delay * (2**attempt)  # 2s, 4s, 8s
            logger.debug(
                f"Rate limited (429), retrying in {delay}s (attempt {attempt + 1}/{max_retries})"
            )
            time.sleep(delay)
    return response  # Return last 429 response


# Thread-safe rate limiters for each API
# PubMed: 3 req/s without key (0.34s), 10 req/s with key (0.1s)
_pubmed_rate_limiter = RateLimiter(0.34)
# ArXiv: 1 request per 3 seconds per terms of service
_arxiv_rate_limiter = RateLimiter(3.0)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register academic research actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # ============================================================
    # academic.pubmed - NCBI PubMed E-utilities API
    # ============================================================

    def academic_pubmed(
        state,
        query: str,
        max_results: int = 5,
        sort_by: str = "relevance",
        timeout: int = 30,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Search PubMed database for scientific articles via NCBI E-utilities.

        Uses the two-step process:
        1. esearch - Find PMIDs matching the query
        2. efetch - Retrieve article details for found PMIDs

        Args:
            state: Current workflow state
            query: Search query string (PubMed query syntax supported)
            max_results: Maximum number of results to return. Default: 5
            sort_by: Sort order - "relevance" or "date". Default: "relevance"
            timeout: Request timeout in seconds. Default: 30

        Returns:
            On success:
            {
                "success": True,
                "results": [
                    {
                        "pmid": str,
                        "title": str,
                        "authors": List[str],
                        "abstract": str,
                        "journal": str,
                        "pub_date": str,
                        "doi": str,
                        "url": str
                    },
                    ...
                ],
                "query": str,
                "total_results": int,
                "returned_results": int
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_code": str  # empty_query, network, rate_limit, timeout, api_error
            }

        Example:
            >>> result = academic_pubmed(
            ...     state={},
            ...     query="CRISPR gene editing",
            ...     max_results=10,
            ...     sort_by="date"
            ... )
            >>> if result['success']:
            ...     for article in result['results']:
            ...         print(f"[{article['pmid']}] {article['title']}")
        """
        import requests

        # Validate query
        if not query or not query.strip():
            return {
                "success": False,
                "error": "Query string cannot be empty",
                "error_code": "empty_query",
            }

        query = query.strip()

        # Get API key (optional, increases rate limit)
        api_key = os.environ.get("NCBI_API_KEY")

        # Update rate limiter interval based on API key presence
        rate_limit_delay = 0.1 if api_key else 0.34  # ~10/s or ~3/s
        _pubmed_rate_limiter.update_interval(rate_limit_delay)

        # Map sort_by to PubMed sort parameter
        sort_param = "relevance" if sort_by == "relevance" else "pub_date"

        # Step 1: esearch - Get PMIDs
        esearch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
        esearch_params = {
            "db": "pubmed",
            "term": query,
            "retmax": str(max_results),
            "sort": sort_param,
            "retmode": "xml",
        }
        if api_key:
            esearch_params["api_key"] = api_key

        try:
            # Thread-safe rate limiting
            _pubmed_rate_limiter.wait()

            # Make request with exponential backoff on 429
            esearch_response = _request_with_backoff(
                esearch_url,
                params=esearch_params,
                timeout=timeout,
            )

            # Handle rate limiting exhausted (still 429 after retries)
            if esearch_response.status_code == 429:
                return {
                    "success": False,
                    "error": "PubMed rate limit exceeded after retries. Please wait and try again.",
                    "error_code": "rate_limit_exhausted",
                }

            # Handle HTTP errors
            if esearch_response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"PubMed esearch API error: {esearch_response.status_code}",
                    "error_code": "api_error",
                }

            # Parse esearch XML response
            try:
                esearch_root = ET.fromstring(esearch_response.text)
            except ET.ParseError as e:
                return {
                    "success": False,
                    "error": f"Failed to parse esearch response: {str(e)}",
                    "error_code": "api_error",
                }

            # Extract PMIDs
            pmid_list = esearch_root.findall(".//Id")
            pmids = [pmid.text for pmid in pmid_list if pmid.text]

            # Get total count
            count_elem = esearch_root.find(".//Count")
            total_count = (
                int(count_elem.text)
                if count_elem is not None and count_elem.text
                else 0
            )

            if not pmids:
                return {
                    "success": True,
                    "results": [],
                    "query": query,
                    "total_results": total_count,
                    "returned_results": 0,
                }

            # Thread-safe rate limiting before efetch
            _pubmed_rate_limiter.wait()

            # Step 2: efetch - Get article details
            efetch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
            efetch_params = {
                "db": "pubmed",
                "id": ",".join(pmids),
                "retmode": "xml",
                "rettype": "abstract",
            }
            if api_key:
                efetch_params["api_key"] = api_key

            # Make request with exponential backoff on 429
            efetch_response = _request_with_backoff(
                efetch_url,
                params=efetch_params,
                timeout=timeout,
            )

            # Handle rate limiting exhausted (still 429 after retries)
            if efetch_response.status_code == 429:
                return {
                    "success": False,
                    "error": "PubMed rate limit exceeded during fetch after retries. Please wait and try again.",
                    "error_code": "rate_limit_exhausted",
                }

            if efetch_response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"PubMed efetch API error: {efetch_response.status_code}",
                    "error_code": "api_error",
                }

            # Parse efetch XML response
            try:
                efetch_root = ET.fromstring(efetch_response.text)
            except ET.ParseError as e:
                return {
                    "success": False,
                    "error": f"Failed to parse efetch response: {str(e)}",
                    "error_code": "api_error",
                }

            # Parse articles
            results = []
            articles = efetch_root.findall(".//PubmedArticle")
            for article in articles:
                parsed = _parse_pubmed_article(article)
                if parsed:
                    results.append(parsed)

            return {
                "success": True,
                "results": results,
                "query": query,
                "total_results": total_count,
                "returned_results": len(results),
            }

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": f"Request timed out after {timeout}s",
                "error_code": "timeout",
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Network connection error: {str(e)}",
                "error_code": "network",
            }
        except Exception as e:
            logger.exception("Unexpected error in academic.pubmed")
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_code": "api_error",
            }

    def _parse_pubmed_article(article_elem) -> Optional[Dict[str, Any]]:
        """
        Parse a PubmedArticle XML element into a structured dict.

        Handles inconsistent XML structures defensively.
        """
        try:
            # Get PMID
            pmid_elem = article_elem.find(".//PMID")
            pmid = pmid_elem.text if pmid_elem is not None else ""

            # Get MedlineCitation
            medline = article_elem.find(".//MedlineCitation")
            if medline is None:
                medline = article_elem  # fallback

            # Get Article element
            article_data = medline.find(".//Article")
            if article_data is None:
                return None

            # Title
            title_elem = article_data.find(".//ArticleTitle")
            title = _get_text_content(title_elem) if title_elem is not None else ""

            # Abstract
            abstract_elem = article_data.find(".//Abstract")
            abstract = ""
            if abstract_elem is not None:
                abstract_texts = abstract_elem.findall(".//AbstractText")
                abstract_parts = []
                for ab in abstract_texts:
                    label = ab.get("Label", "")
                    text = _get_text_content(ab) or ""
                    if label:
                        abstract_parts.append(f"{label}: {text}")
                    else:
                        abstract_parts.append(text)
                abstract = " ".join(abstract_parts)

            # Authors
            authors = []
            author_list = article_data.find(".//AuthorList")
            if author_list is not None:
                for author in author_list.findall(".//Author"):
                    last_name = author.find("LastName")
                    fore_name = author.find("ForeName")
                    initials = author.find("Initials")

                    if last_name is not None and last_name.text:
                        if fore_name is not None and fore_name.text:
                            authors.append(f"{last_name.text} {fore_name.text}")
                        elif initials is not None and initials.text:
                            authors.append(f"{last_name.text} {initials.text}")
                        else:
                            authors.append(last_name.text)
                    else:
                        # Collective name
                        collective = author.find("CollectiveName")
                        if collective is not None and collective.text:
                            authors.append(collective.text)

            # Journal
            journal_elem = article_data.find(".//Journal/Title")
            journal = journal_elem.text if journal_elem is not None else ""
            if not journal:
                # Try ISOAbbreviation
                iso_elem = article_data.find(".//Journal/ISOAbbreviation")
                journal = iso_elem.text if iso_elem is not None else ""

            # Publication date
            pub_date = ""
            pub_date_elem = article_data.find(".//Journal/JournalIssue/PubDate")
            if pub_date_elem is not None:
                year = pub_date_elem.find("Year")
                month = pub_date_elem.find("Month")
                day = pub_date_elem.find("Day")

                if year is not None and year.text:
                    pub_date = year.text
                    if month is not None and month.text:
                        pub_date = f"{pub_date}-{month.text}"
                        if day is not None and day.text:
                            pub_date = f"{pub_date}-{day.text}"
                else:
                    # MedlineDate fallback
                    medline_date = pub_date_elem.find("MedlineDate")
                    if medline_date is not None and medline_date.text:
                        pub_date = medline_date.text

            # DOI
            doi = ""
            article_ids = article_elem.find(".//PubmedData/ArticleIdList")
            if article_ids is not None:
                for article_id in article_ids.findall("ArticleId"):
                    if article_id.get("IdType") == "doi":
                        doi = article_id.text or ""
                        break

            # If DOI not in PubmedData, check ELocationID
            if not doi:
                elocation = article_data.find(".//ELocationID[@EIdType='doi']")
                if elocation is not None and elocation.text:
                    doi = elocation.text

            # URL
            url = f"https://pubmed.ncbi.nlm.nih.gov/{pmid}/" if pmid else ""

            return {
                "pmid": pmid,
                "title": title,
                "authors": authors,
                "abstract": abstract,
                "journal": journal,
                "pub_date": pub_date,
                "doi": doi,
                "url": url,
            }

        except Exception as e:
            logger.warning(f"Failed to parse PubMed article: {e}")
            return None

    def _get_text_content(elem) -> str:
        """
        Get all text content from an element, including nested elements.

        Handles cases like <ArticleTitle>Title with <i>italic</i> text</ArticleTitle>
        """
        if elem is None:
            return ""
        # Get all text including tail of nested elements
        return "".join(elem.itertext()).strip()

    registry["academic.pubmed"] = academic_pubmed
    registry["actions.academic_pubmed"] = academic_pubmed

    # ============================================================
    # academic.arxiv - ArXiv API
    # ============================================================

    def academic_arxiv(
        state,
        query: Optional[str] = None,
        arxiv_id: Optional[str] = None,
        max_results: int = 5,
        sort_by: str = "relevance",
        timeout: int = 30,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Search ArXiv preprint server for papers.

        Supports two modes:
        1. Search by query string
        2. Direct lookup by ArXiv ID

        Args:
            state: Current workflow state
            query: Search query string (ArXiv query syntax supported)
            arxiv_id: ArXiv paper ID for direct lookup (e.g., "2301.00001")
            max_results: Maximum number of results to return. Default: 5
            sort_by: Sort order - "relevance" or "date". Default: "relevance"
            timeout: Request timeout in seconds. Default: 30

        Returns:
            On success:
            {
                "success": True,
                "results": [
                    {
                        "arxiv_id": str,
                        "title": str,
                        "authors": List[str],
                        "abstract": str,
                        "categories": List[str],
                        "published": str,
                        "updated": str,
                        "pdf_url": str
                    },
                    ...
                ],
                "query": str,
                "total_results": int
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_code": str  # empty_query, network, rate_limit, timeout, api_error
            }

        Example:
            >>> # Search by query
            >>> result = academic_arxiv(
            ...     state={},
            ...     query="large language models",
            ...     max_results=5,
            ...     sort_by="date"
            ... )
            >>> if result['success']:
            ...     for paper in result['results']:
            ...         print(f"[{paper['arxiv_id']}] {paper['title']}")

            >>> # Direct lookup by ID
            >>> result = academic_arxiv(
            ...     state={},
            ...     arxiv_id="2301.00001"
            ... )
        """
        import requests

        # Validate input
        if not query and not arxiv_id:
            return {
                "success": False,
                "error": "Either query or arxiv_id must be provided",
                "error_code": "empty_query",
            }

        # Thread-safe rate limiting: 1 request per 3 seconds per ArXiv terms of service
        _arxiv_rate_limiter.wait()

        # Build ArXiv API URL
        base_url = "http://export.arxiv.org/api/query"

        if arxiv_id:
            # Direct ID lookup
            # Normalize ID (remove version if present for base lookup)
            clean_id = arxiv_id.strip()
            search_query = f"id:{clean_id}"
            actual_query = arxiv_id
        else:
            # Search by query
            query = query.strip()
            if not query:
                return {
                    "success": False,
                    "error": "Query string cannot be empty",
                    "error_code": "empty_query",
                }
            # ArXiv uses 'all:' prefix for full-text search
            search_query = f"all:{query}"
            actual_query = query

        # Map sort_by to ArXiv parameters
        if sort_by == "date":
            sort_param = "submittedDate"
            sort_order = "descending"
        else:
            sort_param = "relevance"
            sort_order = "descending"

        params = {
            "search_query": search_query,
            "start": "0",
            "max_results": str(max_results),
            "sortBy": sort_param,
            "sortOrder": sort_order,
        }

        try:
            # Make request with exponential backoff on 429
            response = _request_with_backoff(
                base_url,
                params=params,
                timeout=timeout,
            )

            # Handle rate limiting exhausted (still 429 after retries)
            if response.status_code == 429:
                return {
                    "success": False,
                    "error": "ArXiv rate limit exceeded after retries. Please wait and try again.",
                    "error_code": "rate_limit_exhausted",
                }

            # ArXiv returns 200 even for errors, need to check content
            if response.status_code >= 400:
                return {
                    "success": False,
                    "error": f"ArXiv API error: {response.status_code}",
                    "error_code": "api_error",
                }

            # Parse Atom XML response
            try:
                root = ET.fromstring(response.text)
            except ET.ParseError as e:
                return {
                    "success": False,
                    "error": f"Failed to parse ArXiv response: {str(e)}",
                    "error_code": "api_error",
                }

            # Define namespace
            ns = {
                "atom": "http://www.w3.org/2005/Atom",
                "arxiv": "http://arxiv.org/schemas/atom",
                "opensearch": "http://a9.com/-/spec/opensearch/1.1/",
            }

            # Get total results
            total_elem = root.find("opensearch:totalResults", ns)
            total_results = (
                int(total_elem.text)
                if total_elem is not None and total_elem.text
                else 0
            )

            # Parse entries
            results = []
            entries = root.findall("atom:entry", ns)
            for entry in entries:
                parsed = _parse_arxiv_entry(entry, ns)
                if parsed:
                    results.append(parsed)

            return {
                "success": True,
                "results": results,
                "query": actual_query,
                "total_results": total_results,
            }

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": f"Request timed out after {timeout}s",
                "error_code": "timeout",
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Network connection error: {str(e)}",
                "error_code": "network",
            }
        except Exception as e:
            logger.exception("Unexpected error in academic.arxiv")
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_code": "api_error",
            }

    def _parse_arxiv_entry(entry, ns: dict) -> Optional[Dict[str, Any]]:
        """
        Parse an ArXiv Atom entry into a structured dict.
        """
        try:
            # ArXiv ID from id element
            id_elem = entry.find("atom:id", ns)
            arxiv_url = id_elem.text if id_elem is not None else ""
            # Extract ID from URL: http://arxiv.org/abs/2301.00001v1 -> 2301.00001v1
            arxiv_id = ""
            if arxiv_url:
                match = re.search(r"arxiv\.org/abs/(.+)$", arxiv_url)
                if match:
                    arxiv_id = match.group(1)

            # Title
            title_elem = entry.find("atom:title", ns)
            title = (
                title_elem.text.strip().replace("\n", " ")
                if title_elem is not None and title_elem.text
                else ""
            )
            # Normalize whitespace
            title = re.sub(r"\s+", " ", title)

            # Authors
            authors = []
            for author in entry.findall("atom:author", ns):
                name_elem = author.find("atom:name", ns)
                if name_elem is not None and name_elem.text:
                    authors.append(name_elem.text.strip())

            # Abstract (summary)
            summary_elem = entry.find("atom:summary", ns)
            abstract = (
                summary_elem.text.strip()
                if summary_elem is not None and summary_elem.text
                else ""
            )
            # Normalize whitespace in abstract
            abstract = re.sub(r"\s+", " ", abstract)

            # Categories
            categories = []
            for category in entry.findall("atom:category", ns):
                term = category.get("term")
                if term:
                    categories.append(term)

            # Also check arxiv:primary_category
            primary = entry.find("arxiv:primary_category", ns)
            if primary is not None:
                primary_term = primary.get("term")
                if primary_term and primary_term not in categories:
                    categories.insert(0, primary_term)

            # Published date
            published_elem = entry.find("atom:published", ns)
            published = published_elem.text if published_elem is not None else ""

            # Updated date
            updated_elem = entry.find("atom:updated", ns)
            updated = updated_elem.text if updated_elem is not None else ""

            # PDF URL - find link with type="application/pdf"
            pdf_url = ""
            for link in entry.findall("atom:link", ns):
                if link.get("type") == "application/pdf":
                    pdf_url = link.get("href", "")
                    break

            # If no PDF link found, construct from ID
            if not pdf_url and arxiv_id:
                # Remove version for PDF URL
                base_id = re.sub(r"v\d+$", "", arxiv_id)
                pdf_url = f"https://arxiv.org/pdf/{base_id}"

            return {
                "arxiv_id": arxiv_id,
                "title": title,
                "authors": authors,
                "abstract": abstract,
                "categories": categories,
                "published": published,
                "updated": updated,
                "pdf_url": pdf_url,
            }

        except Exception as e:
            logger.warning(f"Failed to parse ArXiv entry: {e}")
            return None

    registry["academic.arxiv"] = academic_arxiv
    registry["actions.academic_arxiv"] = academic_arxiv

    # ============================================================
    # academic.crossref - CrossRef API
    # ============================================================

    # CrossRef rate limiter: 1 req/s default, 50 req/s with mailto (polite pool)
    _crossref_rate_limiter = RateLimiter(1.0)

    def academic_crossref(
        state,
        doi: Optional[str] = None,
        query: Optional[str] = None,
        max_results: int = 5,
        timeout: int = 30,
        mailto: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Query CrossRef API for DOI metadata or search by query string.

        Supports two modes:
        1. Direct DOI lookup via `https://api.crossref.org/works/{doi}`
        2. Search by query via `https://api.crossref.org/works?query={query}`

        Args:
            state: Current workflow state
            doi: DOI for direct lookup (e.g., "10.1038/nature12373")
            query: Search query string for text search
            max_results: Maximum number of results to return. Default: 5
            timeout: Request timeout in seconds. Default: 30
            mailto: Email for polite pool access (recommended for higher rate limits)

        Returns:
            On success:
            {
                "success": True,
                "results": [
                    {
                        "doi": str,
                        "title": str,
                        "authors": List[str],
                        "abstract": str,
                        "container_title": str,  # journal/book name
                        "published_date": str,
                        "type": str,  # journal-article, book-chapter, etc.
                        "url": str
                    },
                    ...
                ],
                "query": str,
                "total_results": int
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_code": str  # empty_query, not_found, network, rate_limit_exhausted, timeout, api_error
            }

        Example:
            >>> # Direct DOI lookup
            >>> result = academic_crossref(
            ...     state={},
            ...     doi="10.1038/nature12373"
            ... )
            >>> if result['success']:
            ...     print(f"Title: {result['results'][0]['title']}")

            >>> # Search by query
            >>> result = academic_crossref(
            ...     state={},
            ...     query="machine learning cancer",
            ...     max_results=10,
            ...     mailto="your@email.com"
            ... )
        """
        import requests

        # Validate input
        if not doi and not query:
            return {
                "success": False,
                "error": "Either doi or query must be provided",
                "error_code": "empty_query",
            }

        # Build User-Agent header (polite pool requires mailto)
        headers = {}
        if mailto:
            headers["User-Agent"] = f"TEA-Agent/1.0 (mailto:{mailto})"
            # Use polite pool rate limit (50 req/s = 0.02s interval)
            _crossref_rate_limiter.update_interval(0.02)
        else:
            headers["User-Agent"] = "TEA-Agent/1.0"
            # Default rate limit (1 req/s)
            _crossref_rate_limiter.update_interval(1.0)

        # Thread-safe rate limiting
        _crossref_rate_limiter.wait()

        base_url = "https://api.crossref.org/works"

        try:
            if doi:
                # Direct DOI lookup
                doi = doi.strip()
                if not doi:
                    return {
                        "success": False,
                        "error": "DOI string cannot be empty",
                        "error_code": "empty_query",
                    }

                # URL-encode the DOI for the path
                encoded_doi = quote_plus(doi)
                url = f"{base_url}/{encoded_doi}"

                response = _request_with_backoff(
                    url,
                    params={},
                    timeout=timeout,
                    headers=headers,
                )

                if response.status_code == 429:
                    return {
                        "success": False,
                        "error": "CrossRef rate limit exceeded after retries. Please wait and try again.",
                        "error_code": "rate_limit_exhausted",
                    }

                if response.status_code == 404:
                    return {
                        "success": False,
                        "error": f"DOI not found: {doi}",
                        "error_code": "not_found",
                    }

                if response.status_code >= 400:
                    return {
                        "success": False,
                        "error": f"CrossRef API error: {response.status_code}",
                        "error_code": "api_error",
                    }

                try:
                    data = response.json()
                except Exception as e:
                    return {
                        "success": False,
                        "error": f"Failed to parse CrossRef response: {str(e)}",
                        "error_code": "api_error",
                    }

                if data.get("status") != "ok":
                    return {
                        "success": False,
                        "error": f"CrossRef API returned status: {data.get('status')}",
                        "error_code": "api_error",
                    }

                # Parse single result
                message = data.get("message", {})
                parsed = _parse_crossref_work(message)
                if parsed:
                    return {
                        "success": True,
                        "results": [parsed],
                        "query": doi,
                        "total_results": 1,
                    }
                else:
                    return {
                        "success": True,
                        "results": [],
                        "query": doi,
                        "total_results": 0,
                    }

            else:
                # Search by query
                query = query.strip()
                if not query:
                    return {
                        "success": False,
                        "error": "Query string cannot be empty",
                        "error_code": "empty_query",
                    }

                params = {
                    "query": query,
                    "rows": str(max_results),
                }

                response = _request_with_backoff(
                    base_url,
                    params=params,
                    timeout=timeout,
                    headers=headers,
                )

                if response.status_code == 429:
                    return {
                        "success": False,
                        "error": "CrossRef rate limit exceeded after retries. Please wait and try again.",
                        "error_code": "rate_limit_exhausted",
                    }

                if response.status_code >= 400:
                    return {
                        "success": False,
                        "error": f"CrossRef API error: {response.status_code}",
                        "error_code": "api_error",
                    }

                try:
                    data = response.json()
                except Exception as e:
                    return {
                        "success": False,
                        "error": f"Failed to parse CrossRef response: {str(e)}",
                        "error_code": "api_error",
                    }

                if data.get("status") != "ok":
                    return {
                        "success": False,
                        "error": f"CrossRef API returned status: {data.get('status')}",
                        "error_code": "api_error",
                    }

                message = data.get("message", {})
                total_results = message.get("total-results", 0)
                items = message.get("items", [])

                results = []
                for item in items:
                    parsed = _parse_crossref_work(item)
                    if parsed:
                        results.append(parsed)

                return {
                    "success": True,
                    "results": results,
                    "query": query,
                    "total_results": total_results,
                }

        except requests.exceptions.Timeout:
            return {
                "success": False,
                "error": f"Request timed out after {timeout}s",
                "error_code": "timeout",
            }
        except requests.exceptions.ConnectionError as e:
            return {
                "success": False,
                "error": f"Network connection error: {str(e)}",
                "error_code": "network",
            }
        except Exception as e:
            logger.exception("Unexpected error in academic.crossref")
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_code": "api_error",
            }

    def _parse_crossref_work(work: dict) -> Optional[Dict[str, Any]]:
        """
        Parse a CrossRef work item into a structured dict.

        Handles the CrossRef API response format which includes:
        - DOI, title (array), author (array with given/family)
        - abstract (may contain JATS XML), container-title (array)
        - published date-parts, type

        Args:
            work: CrossRef work dictionary from API response

        Returns:
            Parsed work dictionary or None if parsing fails
        """
        try:
            # DOI
            doi = work.get("DOI", "")

            # Title - CrossRef returns as array
            title_list = work.get("title", [])
            title = title_list[0] if title_list else ""

            # Authors - format: [{"given": "John", "family": "Smith"}, ...]
            authors = []
            author_list = work.get("author", [])
            for author in author_list:
                family = author.get("family", "")
                given = author.get("given", "")
                if family:
                    if given:
                        authors.append(f"{family}, {given}")
                    else:
                        authors.append(family)
                elif author.get("name"):
                    # Some entries use "name" for organizations
                    authors.append(author.get("name"))

            # Abstract - may contain JATS XML tags, strip them
            abstract = work.get("abstract", "")
            if abstract:
                # Remove JATS XML tags like <jats:p>, <jats:italic>, etc.
                abstract = re.sub(r"<[^>]+>", "", abstract)
                abstract = abstract.strip()

            # Container title (journal/book name) - array
            container_list = work.get("container-title", [])
            container_title = container_list[0] if container_list else ""

            # Published date - format: {"date-parts": [[2023, 1, 15]]}
            published_date = ""
            published = (
                work.get("published", {})
                or work.get("published-print", {})
                or work.get("published-online", {})
            )
            if published:
                date_parts = published.get("date-parts", [[]])
                if date_parts and date_parts[0]:
                    parts = date_parts[0]
                    if len(parts) >= 1:
                        published_date = str(parts[0])  # Year
                    if len(parts) >= 2:
                        published_date = (
                            f"{published_date}-{str(parts[1]).zfill(2)}"  # Month
                        )
                    if len(parts) >= 3:
                        published_date = (
                            f"{published_date}-{str(parts[2]).zfill(2)}"  # Day
                        )

            # Type
            work_type = work.get("type", "")

            # URL
            url = f"https://doi.org/{doi}" if doi else ""

            return {
                "doi": doi,
                "title": title,
                "authors": authors,
                "abstract": abstract,
                "container_title": container_title,
                "published_date": published_date,
                "type": work_type,
                "url": url,
            }

        except Exception as e:
            logger.warning(f"Failed to parse CrossRef work: {e}")
            return None

    registry["academic.crossref"] = academic_crossref
    registry["actions.academic_crossref"] = academic_crossref
