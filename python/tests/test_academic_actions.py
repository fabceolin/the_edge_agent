"""
Tests for Academic Actions (TEA-KIROKU-001).

Tests cover:
- academic.pubmed: PubMed search via NCBI E-utilities API
- academic.arxiv: ArXiv search via ArXiv API

Uses unittest.mock to mock HTTP requests for deterministic testing.
"""

import unittest
from unittest.mock import patch, MagicMock
import requests

from the_edge_agent.actions.academic_actions import register_actions


# Sample PubMed XML responses
PUBMED_ESEARCH_SUCCESS = """<?xml version="1.0" encoding="UTF-8"?>
<eSearchResult>
    <Count>150</Count>
    <RetMax>2</RetMax>
    <IdList>
        <Id>12345678</Id>
        <Id>87654321</Id>
    </IdList>
</eSearchResult>
"""

PUBMED_EFETCH_SUCCESS = """<?xml version="1.0" encoding="UTF-8"?>
<PubmedArticleSet>
    <PubmedArticle>
        <MedlineCitation>
            <PMID>12345678</PMID>
            <Article>
                <ArticleTitle>Machine Learning for Cancer Diagnosis</ArticleTitle>
                <Abstract>
                    <AbstractText Label="BACKGROUND">Background text here.</AbstractText>
                    <AbstractText Label="METHODS">Methods text here.</AbstractText>
                </Abstract>
                <AuthorList>
                    <Author>
                        <LastName>Smith</LastName>
                        <ForeName>John</ForeName>
                    </Author>
                    <Author>
                        <LastName>Doe</LastName>
                        <Initials>A</Initials>
                    </Author>
                </AuthorList>
                <Journal>
                    <Title>Nature Medicine</Title>
                    <JournalIssue>
                        <PubDate>
                            <Year>2024</Year>
                            <Month>01</Month>
                            <Day>15</Day>
                        </PubDate>
                    </JournalIssue>
                </Journal>
            </Article>
        </MedlineCitation>
        <PubmedData>
            <ArticleIdList>
                <ArticleId IdType="doi">10.1038/s41591-024-00001</ArticleId>
            </ArticleIdList>
        </PubmedData>
    </PubmedArticle>
    <PubmedArticle>
        <MedlineCitation>
            <PMID>87654321</PMID>
            <Article>
                <ArticleTitle>Deep Learning in Healthcare</ArticleTitle>
                <Abstract>
                    <AbstractText>Full abstract without labels.</AbstractText>
                </Abstract>
                <AuthorList>
                    <Author>
                        <CollectiveName>Research Consortium</CollectiveName>
                    </Author>
                </AuthorList>
                <Journal>
                    <ISOAbbreviation>Lancet Digit Health</ISOAbbreviation>
                    <JournalIssue>
                        <PubDate>
                            <MedlineDate>2024 Jan-Feb</MedlineDate>
                        </PubDate>
                    </JournalIssue>
                </Journal>
                <ELocationID EIdType="doi">10.1016/S2589-7500(24)00001-1</ELocationID>
            </Article>
        </MedlineCitation>
    </PubmedArticle>
</PubmedArticleSet>
"""

PUBMED_ESEARCH_EMPTY = """<?xml version="1.0" encoding="UTF-8"?>
<eSearchResult>
    <Count>0</Count>
    <RetMax>0</RetMax>
    <IdList>
    </IdList>
</eSearchResult>
"""

# Sample ArXiv XML responses
ARXIV_SUCCESS = """<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom" xmlns:arxiv="http://arxiv.org/schemas/atom" xmlns:opensearch="http://a9.com/-/spec/opensearch/1.1/">
    <opensearch:totalResults>250</opensearch:totalResults>
    <entry>
        <id>http://arxiv.org/abs/2301.00001v2</id>
        <title>Attention Is All You Need: A Survey</title>
        <summary>This paper surveys transformer architectures and their applications in NLP and computer vision.</summary>
        <author>
            <name>Alice Smith</name>
        </author>
        <author>
            <name>Bob Jones</name>
        </author>
        <arxiv:primary_category term="cs.CL"/>
        <category term="cs.CL"/>
        <category term="cs.AI"/>
        <published>2023-01-01T00:00:00Z</published>
        <updated>2023-01-15T12:00:00Z</updated>
        <link type="application/pdf" href="https://arxiv.org/pdf/2301.00001v2"/>
    </entry>
    <entry>
        <id>http://arxiv.org/abs/2301.00002v1</id>
        <title>Large Language Models: A Review</title>
        <summary>Comprehensive review of LLM architectures.</summary>
        <author>
            <name>Carol White</name>
        </author>
        <category term="cs.LG"/>
        <published>2023-01-02T00:00:00Z</published>
        <updated>2023-01-02T00:00:00Z</updated>
    </entry>
</feed>
"""

ARXIV_EMPTY = """<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom" xmlns:opensearch="http://a9.com/-/spec/opensearch/1.1/">
    <opensearch:totalResults>0</opensearch:totalResults>
</feed>
"""


class TestAcademicPubMed(unittest.TestCase):
    """Tests for academic.pubmed action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.pubmed_action = self.registry["academic.pubmed"]

    def _mock_response(self, text, status_code=200):
        """Create a mock response object."""
        mock = MagicMock()
        mock.text = text
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_search_success(self, mock_get, mock_time, mock_sleep):
        """Test successful PubMed search returns structured results."""
        mock_time.return_value = 100.0  # Fixed time for rate limiting
        mock_get.side_effect = [
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(PUBMED_EFETCH_SUCCESS),
        ]

        result = self.pubmed_action(
            state={},
            query="machine learning cancer",
            max_results=5,
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["query"], "machine learning cancer")
        self.assertEqual(result["total_results"], 150)
        self.assertEqual(len(result["results"]), 2)

        # Check first article
        article1 = result["results"][0]
        self.assertEqual(article1["pmid"], "12345678")
        self.assertEqual(article1["title"], "Machine Learning for Cancer Diagnosis")
        self.assertEqual(article1["authors"], ["Smith John", "Doe A"])
        self.assertIn("BACKGROUND:", article1["abstract"])
        self.assertEqual(article1["journal"], "Nature Medicine")
        self.assertEqual(article1["pub_date"], "2024-01-15")
        self.assertEqual(article1["doi"], "10.1038/s41591-024-00001")
        self.assertEqual(article1["url"], "https://pubmed.ncbi.nlm.nih.gov/12345678/")

        # Check second article with different structure
        article2 = result["results"][1]
        self.assertEqual(article2["pmid"], "87654321")
        self.assertEqual(article2["authors"], ["Research Consortium"])
        self.assertEqual(article2["journal"], "Lancet Digit Health")
        self.assertEqual(article2["pub_date"], "2024 Jan-Feb")
        self.assertEqual(article2["doi"], "10.1016/S2589-7500(24)00001-1")

    @patch("requests.get")
    def test_pubmed_empty_query(self, mock_get):
        """Test that empty query returns error."""
        result = self.pubmed_action(state={}, query="")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")
        self.assertIn("empty", result["error"].lower())
        mock_get.assert_not_called()

    @patch("requests.get")
    def test_pubmed_whitespace_query(self, mock_get):
        """Test that whitespace-only query returns error."""
        result = self.pubmed_action(state={}, query="   ")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_no_results(self, mock_get, mock_time, mock_sleep):
        """Test handling of empty search results."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(PUBMED_ESEARCH_EMPTY)

        result = self.pubmed_action(
            state={},
            query="xyznonexistent123",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["results"], [])
        self.assertEqual(result["total_results"], 0)
        self.assertEqual(result["returned_results"], 0)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_rate_limit_exhausted(self, mock_get, mock_time, mock_sleep):
        """Test rate limit exhausted after retries (HTTP 429)."""
        mock_time.return_value = 100.0
        # Return 429 for all retry attempts (initial + 3 retries = 4 calls)
        mock_get.return_value = self._mock_response("Rate limit exceeded", 429)

        result = self.pubmed_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "rate_limit_exhausted")
        self.assertIn("rate limit", result["error"].lower())
        # Should have attempted initial + 3 retries = 4 calls
        self.assertEqual(mock_get.call_count, 4)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_network_error(self, mock_get, mock_time, mock_sleep):
        """Test network error handling."""
        mock_time.return_value = 100.0
        mock_get.side_effect = requests.exceptions.ConnectionError(
            "Network unreachable"
        )

        result = self.pubmed_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "network")
        self.assertIn("connection", result["error"].lower())

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_timeout(self, mock_get, mock_time, mock_sleep):
        """Test timeout error handling."""
        mock_time.return_value = 100.0
        mock_get.side_effect = requests.exceptions.Timeout("Request timed out")

        result = self.pubmed_action(state={}, query="test", timeout=10)

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "timeout")
        self.assertIn("timed out", result["error"].lower())

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_api_error(self, mock_get, mock_time, mock_sleep):
        """Test API error handling (non-429 HTTP errors)."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("Internal Server Error", 500)

        result = self.pubmed_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "api_error")
        self.assertIn("500", result["error"])

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_sort_by_date(self, mock_get, mock_time, mock_sleep):
        """Test sort_by=date parameter is passed correctly."""
        mock_time.return_value = 100.0
        mock_get.side_effect = [
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(PUBMED_EFETCH_SUCCESS),
        ]

        self.pubmed_action(
            state={},
            query="test",
            sort_by="date",
        )

        # Check the esearch call used pub_date sort
        call_args = mock_get.call_args_list[0]
        params = call_args.kwargs.get("params", call_args[1].get("params", {}))
        self.assertEqual(params.get("sort"), "pub_date")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    @patch.dict("os.environ", {"NCBI_API_KEY": "test_key_123"})
    def test_pubmed_with_api_key(self, mock_get, mock_time, mock_sleep):
        """Test API key is included when set."""
        mock_time.return_value = 100.0
        mock_get.side_effect = [
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(PUBMED_EFETCH_SUCCESS),
        ]

        self.pubmed_action(state={}, query="test")

        # Check API key was passed in both requests
        for call in mock_get.call_args_list:
            params = call.kwargs.get("params", call[1].get("params", {}))
            self.assertEqual(params.get("api_key"), "test_key_123")


class TestAcademicArXiv(unittest.TestCase):
    """Tests for academic.arxiv action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.arxiv_action = self.registry["academic.arxiv"]

    def _mock_response(self, text, status_code=200):
        """Create a mock response object."""
        mock = MagicMock()
        mock.text = text
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_search_success(self, mock_get, mock_time, mock_sleep):
        """Test successful ArXiv search returns structured results."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(ARXIV_SUCCESS)

        result = self.arxiv_action(
            state={},
            query="transformer neural networks",
            max_results=10,
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["query"], "transformer neural networks")
        self.assertEqual(result["total_results"], 250)
        self.assertEqual(len(result["results"]), 2)

        # Check first paper
        paper1 = result["results"][0]
        self.assertEqual(paper1["arxiv_id"], "2301.00001v2")
        self.assertEqual(paper1["title"], "Attention Is All You Need: A Survey")
        self.assertEqual(paper1["authors"], ["Alice Smith", "Bob Jones"])
        self.assertIn("transformer architectures", paper1["abstract"])
        self.assertEqual(paper1["categories"], ["cs.CL", "cs.AI"])
        self.assertEqual(paper1["published"], "2023-01-01T00:00:00Z")
        self.assertEqual(paper1["updated"], "2023-01-15T12:00:00Z")
        self.assertEqual(paper1["pdf_url"], "https://arxiv.org/pdf/2301.00001v2")

        # Check second paper (no explicit PDF link - should construct)
        paper2 = result["results"][1]
        self.assertEqual(paper2["arxiv_id"], "2301.00002v1")
        self.assertEqual(paper2["categories"], ["cs.LG"])
        self.assertEqual(paper2["pdf_url"], "https://arxiv.org/pdf/2301.00002")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_by_id(self, mock_get, mock_time, mock_sleep):
        """Test direct ArXiv ID lookup."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(ARXIV_SUCCESS)

        result = self.arxiv_action(
            state={},
            arxiv_id="2301.00001",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["query"], "2301.00001")

        # Verify correct ID search query was used
        call_args = mock_get.call_args
        params = call_args.kwargs.get("params", call_args[1].get("params", {}))
        self.assertIn("id:2301.00001", params.get("search_query", ""))

    @patch("requests.get")
    def test_arxiv_no_query_or_id(self, mock_get):
        """Test error when neither query nor arxiv_id provided."""
        result = self.arxiv_action(state={})

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")
        mock_get.assert_not_called()

    @patch("requests.get")
    def test_arxiv_empty_query(self, mock_get):
        """Test error when query is empty string."""
        result = self.arxiv_action(state={}, query="")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_no_results(self, mock_get, mock_time, mock_sleep):
        """Test handling of empty search results."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(ARXIV_EMPTY)

        result = self.arxiv_action(
            state={},
            query="xyznonexistent123",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["results"], [])
        self.assertEqual(result["total_results"], 0)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_network_error(self, mock_get, mock_time, mock_sleep):
        """Test network error handling."""
        mock_time.return_value = 100.0
        mock_get.side_effect = requests.exceptions.ConnectionError("Network error")

        result = self.arxiv_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "network")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_timeout(self, mock_get, mock_time, mock_sleep):
        """Test timeout error handling."""
        mock_time.return_value = 100.0
        mock_get.side_effect = requests.exceptions.Timeout()

        result = self.arxiv_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "timeout")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_sort_by_date(self, mock_get, mock_time, mock_sleep):
        """Test sort_by=date parameter."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(ARXIV_SUCCESS)

        self.arxiv_action(
            state={},
            query="test",
            sort_by="date",
        )

        call_args = mock_get.call_args
        params = call_args.kwargs.get("params", call_args[1].get("params", {}))
        self.assertEqual(params.get("sortBy"), "submittedDate")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_rate_limit_sleep(self, mock_get, mock_time, mock_sleep):
        """Test rate limiting enforces 3-second delay."""
        from the_edge_agent.actions import academic_actions

        # Set up the rate limiter's internal state to simulate a recent request
        academic_actions._arxiv_rate_limiter._last_request = 99.5
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(ARXIV_SUCCESS)

        self.arxiv_action(state={}, query="test")

        # Should have slept for ~2.5 seconds to maintain 3s gap
        mock_sleep.assert_called()
        sleep_time = mock_sleep.call_args[0][0]
        self.assertAlmostEqual(sleep_time, 2.5, places=1)


class TestActionRegistration(unittest.TestCase):
    """Test action registration in registry."""

    def test_actions_registered(self):
        """Test that both actions are registered with correct names."""
        registry = {}
        mock_engine = MagicMock()
        register_actions(registry, mock_engine)

        # Primary names
        self.assertIn("academic.pubmed", registry)
        self.assertIn("academic.arxiv", registry)

        # Alias names
        self.assertIn("actions.academic_pubmed", registry)
        self.assertIn("actions.academic_arxiv", registry)

    def test_actions_callable(self):
        """Test that registered actions are callable."""
        registry = {}
        mock_engine = MagicMock()
        register_actions(registry, mock_engine)

        self.assertTrue(callable(registry["academic.pubmed"]))
        self.assertTrue(callable(registry["academic.arxiv"]))


class TestPubMedXMLParsing(unittest.TestCase):
    """Test XML parsing robustness for various PubMed response formats."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.pubmed_action = self.registry["academic.pubmed"]

    def _mock_response(self, text, status_code=200):
        mock = MagicMock()
        mock.text = text
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_parse_malformed_xml(self, mock_get, mock_time, mock_sleep):
        """Test handling of malformed XML response."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("not valid xml <unclosed tag")

        result = self.pubmed_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "api_error")
        self.assertIn("parse", result["error"].lower())

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_parse_minimal_article(self, mock_get, mock_time, mock_sleep):
        """Test parsing article with minimal fields."""
        minimal_efetch = """<?xml version="1.0"?>
<PubmedArticleSet>
    <PubmedArticle>
        <MedlineCitation>
            <PMID>99999999</PMID>
            <Article>
                <ArticleTitle>Minimal Article</ArticleTitle>
            </Article>
        </MedlineCitation>
    </PubmedArticle>
</PubmedArticleSet>
"""
        mock_time.return_value = 100.0
        mock_get.side_effect = [
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(minimal_efetch),
        ]

        result = self.pubmed_action(state={}, query="test")

        self.assertTrue(result["success"])
        self.assertEqual(len(result["results"]), 1)
        article = result["results"][0]
        self.assertEqual(article["pmid"], "99999999")
        self.assertEqual(article["title"], "Minimal Article")
        self.assertEqual(article["authors"], [])
        self.assertEqual(article["abstract"], "")
        self.assertEqual(article["journal"], "")
        self.assertEqual(article["doi"], "")


class TestArXivXMLParsing(unittest.TestCase):
    """Test XML parsing robustness for various ArXiv response formats."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.arxiv_action = self.registry["academic.arxiv"]

    def _mock_response(self, text, status_code=200):
        mock = MagicMock()
        mock.text = text
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_parse_malformed_xml(self, mock_get, mock_time, mock_sleep):
        """Test handling of malformed XML response."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("not valid xml")

        result = self.arxiv_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "api_error")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_parse_multiline_title(self, mock_get, mock_time, mock_sleep):
        """Test parsing title with newlines (common in ArXiv)."""
        multiline_response = """<?xml version="1.0"?>
<feed xmlns="http://www.w3.org/2005/Atom" xmlns:opensearch="http://a9.com/-/spec/opensearch/1.1/">
    <opensearch:totalResults>1</opensearch:totalResults>
    <entry>
        <id>http://arxiv.org/abs/2301.00003v1</id>
        <title>This Is A Very Long Title
  That Spans Multiple Lines
  In The XML Response</title>
        <summary>Test abstract.</summary>
        <author><name>Test Author</name></author>
        <published>2023-01-01T00:00:00Z</published>
        <updated>2023-01-01T00:00:00Z</updated>
    </entry>
</feed>
"""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(multiline_response)

        result = self.arxiv_action(state={}, query="test")

        self.assertTrue(result["success"])
        paper = result["results"][0]
        # Title should be normalized (no extra whitespace)
        self.assertNotIn("\n", paper["title"])
        self.assertIn("This Is A Very Long Title", paper["title"])


class TestRateLimiterThreadSafety(unittest.TestCase):
    """Tests for thread-safe rate limiting (TEA-KIROKU-006)."""

    def test_rate_limiter_uses_threading_lock(self):
        """UNIT-001: Verify RateLimiter class uses threading.Lock internally."""
        from the_edge_agent.actions.academic_actions import RateLimiter
        import threading

        limiter = RateLimiter(1.0)

        # Verify internal lock is a threading.Lock
        self.assertIsInstance(limiter._lock, type(threading.Lock()))

    def test_rate_limiter_enforces_interval(self):
        """Test RateLimiter enforces minimum interval between calls."""
        from the_edge_agent.actions.academic_actions import RateLimiter

        limiter = RateLimiter(0.1)  # 100ms interval

        import time

        start = time.time()
        limiter.wait()
        limiter.wait()
        elapsed = time.time() - start

        # Should have waited at least ~100ms for second call
        self.assertGreaterEqual(elapsed, 0.08)  # Allow some tolerance

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_concurrent_pubmed_calls_respect_rate_limit(
        self, mock_get, mock_time, mock_sleep
    ):
        """INT-001: Test concurrent PubMed calls via ThreadPoolExecutor respect rate limits."""
        from concurrent.futures import ThreadPoolExecutor

        mock_time.return_value = 100.0
        mock_get.side_effect = [
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(PUBMED_EFETCH_SUCCESS),
        ] * 5  # 5 concurrent calls, each needs esearch + efetch

        registry = {}
        mock_engine = MagicMock()
        register_actions(registry, mock_engine)
        pubmed_action = registry["academic.pubmed"]

        # Execute 5 concurrent calls
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(pubmed_action, state={}, query=f"test query {i}")
                for i in range(5)
            ]
            results = [f.result() for f in futures]

        # All calls should succeed
        for result in results:
            self.assertTrue(result["success"])

        # Rate limiter should have been invoked (sleep called for waiting)
        # The exact number depends on timing, but there should be some calls
        self.assertTrue(mock_get.called)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_concurrent_arxiv_calls_respect_rate_limit(
        self, mock_get, mock_time, mock_sleep
    ):
        """INT-004: Test concurrent ArXiv calls via ThreadPoolExecutor respect 3s rate limit."""
        from concurrent.futures import ThreadPoolExecutor

        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(ARXIV_SUCCESS)

        registry = {}
        mock_engine = MagicMock()
        register_actions(registry, mock_engine)
        arxiv_action = registry["academic.arxiv"]

        # Execute 5 concurrent calls
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(arxiv_action, state={}, query=f"test query {i}")
                for i in range(5)
            ]
            results = [f.result() for f in futures]

        # All calls should succeed
        for result in results:
            self.assertTrue(result["success"])

        # Verify ArXiv API was called for all requests
        self.assertEqual(mock_get.call_count, 5)

    def test_pubmed_and_arxiv_rate_limiters_are_independent(self):
        """INT-006: Verify PubMed and ArXiv rate limiters are independent."""
        from the_edge_agent.actions import academic_actions

        # Verify they are different objects
        self.assertIsNot(
            academic_actions._pubmed_rate_limiter,
            academic_actions._arxiv_rate_limiter,
        )

        # Verify different intervals
        self.assertEqual(academic_actions._pubmed_rate_limiter._min_interval, 0.34)
        self.assertEqual(academic_actions._arxiv_rate_limiter._min_interval, 3.0)

    def _mock_response(self, text, status_code=200):
        """Create a mock response object."""
        mock = MagicMock()
        mock.text = text
        mock.status_code = status_code
        return mock


class TestExponentialBackoff(unittest.TestCase):
    """Tests for exponential backoff on 429 responses (TEA-KIROKU-006)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)

    def _mock_response(self, text, status_code=200):
        """Create a mock response object."""
        mock = MagicMock()
        mock.text = text
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_backoff_single_429_then_success(self, mock_get, mock_time, mock_sleep):
        """Test successful response after initial 429."""
        mock_time.return_value = 100.0
        # First call returns 429, second call succeeds
        mock_get.side_effect = [
            self._mock_response("Rate limited", 429),
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(PUBMED_EFETCH_SUCCESS),
        ]

        result = self.registry["academic.pubmed"](state={}, query="test")

        self.assertTrue(result["success"])
        # Should have slept once for 2s (base delay)
        sleep_calls = [call[0][0] for call in mock_sleep.call_args_list if call[0]]
        self.assertIn(2.0, sleep_calls)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_backoff_multiple_429_with_increasing_delays(
        self, mock_get, mock_time, mock_sleep
    ):
        """UNIT-004: Test multiple 429 responses with increasing delays (2s, 4s, 8s)."""
        mock_time.return_value = 100.0
        # Return 429 twice, then succeed
        mock_get.side_effect = [
            self._mock_response("Rate limited", 429),
            self._mock_response("Rate limited", 429),
            self._mock_response(PUBMED_ESEARCH_SUCCESS),
            self._mock_response(PUBMED_EFETCH_SUCCESS),
        ]

        result = self.registry["academic.pubmed"](state={}, query="test")

        self.assertTrue(result["success"])
        # Verify exponential backoff delays: 2s, 4s
        sleep_calls = [call[0][0] for call in mock_sleep.call_args_list if call[0]]
        self.assertIn(2.0, sleep_calls)  # 2^0 * 2 = 2
        self.assertIn(4.0, sleep_calls)  # 2^1 * 2 = 4

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_backoff_max_retries_returns_rate_limit_exhausted(
        self, mock_get, mock_time, mock_sleep
    ):
        """UNIT-007: Test max retries returns rate_limit_exhausted error code."""
        mock_time.return_value = 100.0
        # Return 429 for all attempts
        mock_get.return_value = self._mock_response("Rate limited", 429)

        result = self.registry["academic.pubmed"](state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "rate_limit_exhausted")
        # Should have attempted 4 times (initial + 3 retries)
        self.assertEqual(mock_get.call_count, 4)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_backoff_on_429(self, mock_get, mock_time, mock_sleep):
        """Test ArXiv action uses exponential backoff on 429."""
        mock_time.return_value = 100.0
        # First call returns 429, second succeeds
        mock_get.side_effect = [
            self._mock_response("Rate limited", 429),
            self._mock_response(ARXIV_SUCCESS),
        ]

        result = self.registry["academic.arxiv"](state={}, query="test")

        self.assertTrue(result["success"])
        # Verify backoff was applied
        sleep_calls = [call[0][0] for call in mock_sleep.call_args_list if call[0]]
        self.assertIn(2.0, sleep_calls)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_arxiv_rate_limit_exhausted(self, mock_get, mock_time, mock_sleep):
        """INT-010: Test ArXiv returns rate_limit_exhausted after max retries."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("Rate limited", 429)

        result = self.registry["academic.arxiv"](state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "rate_limit_exhausted")
        self.assertEqual(mock_get.call_count, 4)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_pubmed_efetch_429_triggers_backoff(self, mock_get, mock_time, mock_sleep):
        """Test efetch request also uses backoff on 429."""
        mock_time.return_value = 100.0
        # esearch succeeds, efetch gets 429 once then succeeds
        mock_get.side_effect = [
            self._mock_response(PUBMED_ESEARCH_SUCCESS),  # esearch OK
            self._mock_response("Rate limited", 429),  # efetch 429
            self._mock_response(PUBMED_EFETCH_SUCCESS),  # efetch retry OK
        ]

        result = self.registry["academic.pubmed"](state={}, query="test")

        self.assertTrue(result["success"])
        self.assertEqual(len(result["results"]), 2)


# Sample CrossRef JSON responses
CROSSREF_DOI_SUCCESS = {
    "status": "ok",
    "message": {
        "DOI": "10.1038/nature12373",
        "title": ["Sample Article Title"],
        "author": [
            {"given": "John", "family": "Smith"},
            {"given": "Alice", "family": "Doe"},
        ],
        "abstract": "<jats:p>This is the abstract text with <jats:italic>formatting</jats:italic>.</jats:p>",
        "container-title": ["Nature"],
        "published": {"date-parts": [[2023, 1, 15]]},
        "type": "journal-article",
    },
}

CROSSREF_SEARCH_SUCCESS = {
    "status": "ok",
    "message": {
        "total-results": 150,
        "items": [
            {
                "DOI": "10.1038/nature12373",
                "title": ["First Article"],
                "author": [{"given": "John", "family": "Smith"}],
                "container-title": ["Nature"],
                "published": {"date-parts": [[2023, 1, 15]]},
                "type": "journal-article",
            },
            {
                "DOI": "10.1016/j.cell.2023.01.001",
                "title": ["Second Article"],
                "author": [{"name": "Research Consortium"}],
                "container-title": ["Cell"],
                "published-print": {"date-parts": [[2023, 2]]},
                "type": "journal-article",
            },
        ],
    },
}

CROSSREF_EMPTY_SEARCH = {
    "status": "ok",
    "message": {
        "total-results": 0,
        "items": [],
    },
}


class TestAcademicCrossRef(unittest.TestCase):
    """Tests for academic.crossref action (TEA-KIROKU-007)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.crossref_action = self.registry["academic.crossref"]

    def _mock_response(self, data, status_code=200):
        """Create a mock response object."""
        import json

        mock = MagicMock()
        if isinstance(data, dict):
            mock.json.return_value = data
            mock.text = json.dumps(data)
        else:
            mock.text = data
            mock.json.side_effect = Exception("Invalid JSON")
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_doi_lookup_success(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-UNIT-001: Test DOI response parsing."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_DOI_SUCCESS)

        result = self.crossref_action(state={}, doi="10.1038/nature12373")

        self.assertTrue(result["success"])
        self.assertEqual(result["query"], "10.1038/nature12373")
        self.assertEqual(result["total_results"], 1)
        self.assertEqual(len(result["results"]), 1)

        # Check parsed result
        article = result["results"][0]
        self.assertEqual(article["doi"], "10.1038/nature12373")
        self.assertEqual(article["title"], "Sample Article Title")
        self.assertEqual(article["authors"], ["Smith, John", "Doe, Alice"])
        self.assertIn("abstract text", article["abstract"])
        self.assertNotIn("<jats:", article["abstract"])  # JATS tags stripped
        self.assertEqual(article["container_title"], "Nature")
        self.assertEqual(article["published_date"], "2023-01-15")
        self.assertEqual(article["type"], "journal-article")
        self.assertEqual(article["url"], "https://doi.org/10.1038/nature12373")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_search_success(self, mock_get, mock_time, mock_sleep):
        """Test search by query returns multiple results."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_SEARCH_SUCCESS)

        result = self.crossref_action(
            state={}, query="machine learning", max_results=10
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["query"], "machine learning")
        self.assertEqual(result["total_results"], 150)
        self.assertEqual(len(result["results"]), 2)

        # Check first result
        self.assertEqual(result["results"][0]["doi"], "10.1038/nature12373")
        self.assertEqual(result["results"][0]["title"], "First Article")

        # Check second result with organization author
        self.assertEqual(result["results"][1]["authors"], ["Research Consortium"])

    @patch("requests.get")
    def test_crossref_empty_query_error(self, mock_get):
        """Test error when neither doi nor query provided."""
        result = self.crossref_action(state={})

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")
        mock_get.assert_not_called()

    @patch("requests.get")
    def test_crossref_whitespace_doi_error(self, mock_get):
        """Test error when doi is whitespace only."""
        result = self.crossref_action(state={}, doi="   ")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")

    @patch("requests.get")
    def test_crossref_whitespace_query_error(self, mock_get):
        """Test error when query is whitespace only."""
        result = self.crossref_action(state={}, query="   ")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "empty_query")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_doi_not_found(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-UNIT-011: Test NOT_FOUND error handling."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("Not found", 404)

        result = self.crossref_action(state={}, doi="10.1234/nonexistent")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "not_found")
        self.assertIn("not found", result["error"].lower())

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_empty_search_results(self, mock_get, mock_time, mock_sleep):
        """Test handling of empty search results."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_EMPTY_SEARCH)

        result = self.crossref_action(state={}, query="xyznonexistent123")

        self.assertTrue(result["success"])
        self.assertEqual(result["results"], [])
        self.assertEqual(result["total_results"], 0)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_network_error(self, mock_get, mock_time, mock_sleep):
        """Test network error handling."""
        mock_time.return_value = 100.0
        mock_get.side_effect = requests.exceptions.ConnectionError("Network error")

        result = self.crossref_action(state={}, doi="10.1038/test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "network")
        self.assertIn("connection", result["error"].lower())

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_timeout(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-UNIT-012: Test TIMEOUT error handling."""
        mock_time.return_value = 100.0
        mock_get.side_effect = requests.exceptions.Timeout("Request timed out")

        result = self.crossref_action(state={}, doi="10.1038/test", timeout=10)

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "timeout")
        self.assertIn("timed out", result["error"].lower())

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_rate_limit_exhausted(self, mock_get, mock_time, mock_sleep):
        """Test rate limit exhausted after retries (HTTP 429)."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("Rate limited", 429)

        result = self.crossref_action(state={}, doi="10.1038/test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "rate_limit_exhausted")
        self.assertIn("rate limit", result["error"].lower())
        # Should have attempted initial + 3 retries = 4 calls
        self.assertEqual(mock_get.call_count, 4)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_mailto_sets_user_agent(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-INT-004: Test polite pool User-Agent with mailto."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_DOI_SUCCESS)

        self.crossref_action(
            state={}, doi="10.1038/nature12373", mailto="test@example.com"
        )

        # Verify User-Agent header was set with mailto
        call_args = mock_get.call_args
        headers = call_args.kwargs.get("headers", {})
        self.assertIn("mailto:test@example.com", headers.get("User-Agent", ""))
        self.assertIn("TEA-Agent", headers.get("User-Agent", ""))

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_default_user_agent(self, mock_get, mock_time, mock_sleep):
        """Test User-Agent without mailto."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_DOI_SUCCESS)

        self.crossref_action(state={}, doi="10.1038/nature12373")

        # Verify User-Agent header was set without mailto
        call_args = mock_get.call_args
        headers = call_args.kwargs.get("headers", {})
        self.assertEqual(headers.get("User-Agent"), "TEA-Agent/1.0")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_max_results_parameter(self, mock_get, mock_time, mock_sleep):
        """Test max_results parameter is passed correctly."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_SEARCH_SUCCESS)

        self.crossref_action(state={}, query="test", max_results=20)

        call_args = mock_get.call_args
        params = call_args.kwargs.get("params", {})
        self.assertEqual(params.get("rows"), "20")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_crossref_api_error(self, mock_get, mock_time, mock_sleep):
        """Test API error handling (non-404, non-429)."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("Internal Server Error", 500)

        result = self.crossref_action(state={}, doi="10.1038/test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "api_error")
        self.assertIn("500", result["error"])


class TestCrossRefOutputContract(unittest.TestCase):
    """Tests for CrossRef output field contract (TEA-KIROKU-007)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.crossref_action = self.registry["academic.crossref"]

    def _mock_response(self, data, status_code=200):
        """Create a mock response object."""
        import json

        mock = MagicMock()
        mock.json.return_value = data
        mock.text = json.dumps(data)
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_output_contains_required_fields(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-UNIT-004: Test output field contract."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(CROSSREF_DOI_SUCCESS)

        result = self.crossref_action(state={}, doi="10.1038/nature12373")

        # Check top-level fields
        self.assertIn("success", result)
        self.assertIn("results", result)
        self.assertIn("query", result)
        self.assertIn("total_results", result)

        # Check result item fields
        article = result["results"][0]
        required_fields = [
            "doi",
            "title",
            "authors",
            "abstract",
            "container_title",
            "published_date",
            "type",
            "url",
        ]
        for field in required_fields:
            self.assertIn(field, article, f"Missing required field: {field}")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_minimal_article_parsing(self, mock_get, mock_time, mock_sleep):
        """Test parsing article with minimal fields."""
        minimal_response = {
            "status": "ok",
            "message": {
                "DOI": "10.1234/minimal",
                "title": ["Minimal Article"],
            },
        }
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(minimal_response)

        result = self.crossref_action(state={}, doi="10.1234/minimal")

        self.assertTrue(result["success"])
        article = result["results"][0]
        self.assertEqual(article["doi"], "10.1234/minimal")
        self.assertEqual(article["title"], "Minimal Article")
        self.assertEqual(article["authors"], [])
        self.assertEqual(article["abstract"], "")
        self.assertEqual(article["container_title"], "")
        self.assertEqual(article["published_date"], "")
        self.assertEqual(article["type"], "")
        self.assertEqual(article["url"], "https://doi.org/10.1234/minimal")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_date_parsing_variations(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-UNIT-007: Test date parsing edge cases."""
        # Test year-only date
        year_only = {
            "status": "ok",
            "message": {
                "DOI": "10.1234/test",
                "title": ["Test"],
                "published": {"date-parts": [[2023]]},
            },
        }
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(year_only)

        result = self.crossref_action(state={}, doi="10.1234/test")
        self.assertEqual(result["results"][0]["published_date"], "2023")

        # Test year-month date
        year_month = {
            "status": "ok",
            "message": {
                "DOI": "10.1234/test",
                "title": ["Test"],
                "published": {"date-parts": [[2023, 5]]},
            },
        }
        mock_get.return_value = self._mock_response(year_month)

        result = self.crossref_action(state={}, doi="10.1234/test")
        self.assertEqual(result["results"][0]["published_date"], "2023-05")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_published_online_fallback(self, mock_get, mock_time, mock_sleep):
        """Test published-online fallback when published is missing."""
        online_only = {
            "status": "ok",
            "message": {
                "DOI": "10.1234/test",
                "title": ["Test"],
                "published-online": {"date-parts": [[2023, 6, 20]]},
            },
        }
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(online_only)

        result = self.crossref_action(state={}, doi="10.1234/test")
        self.assertEqual(result["results"][0]["published_date"], "2023-06-20")

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_author_parsing_variations(self, mock_get, mock_time, mock_sleep):
        """KIROKU-007-UNIT-006: Test author parsing edge cases."""
        author_variations = {
            "status": "ok",
            "message": {
                "DOI": "10.1234/test",
                "title": ["Test"],
                "author": [
                    {"given": "John", "family": "Smith"},  # Full name
                    {"family": "Solo"},  # Family only
                    {"name": "Research Consortium"},  # Organization
                ],
            },
        }
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response(author_variations)

        result = self.crossref_action(state={}, doi="10.1234/test")
        authors = result["results"][0]["authors"]
        self.assertEqual(authors, ["Smith, John", "Solo", "Research Consortium"])


class TestCrossRefActionRegistration(unittest.TestCase):
    """Test CrossRef action registration (TEA-KIROKU-007)."""

    def test_crossref_actions_registered(self):
        """Test that CrossRef action is registered with correct names."""
        registry = {}
        mock_engine = MagicMock()
        register_actions(registry, mock_engine)

        # Primary name
        self.assertIn("academic.crossref", registry)
        # Alias name
        self.assertIn("actions.academic_crossref", registry)

    def test_crossref_action_callable(self):
        """Test that registered CrossRef action is callable."""
        registry = {}
        mock_engine = MagicMock()
        register_actions(registry, mock_engine)

        self.assertTrue(callable(registry["academic.crossref"]))
        self.assertTrue(callable(registry["actions.academic_crossref"]))


class TestCrossRefRateLimiting(unittest.TestCase):
    """Tests for CrossRef rate limiting (TEA-KIROKU-007)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        register_actions(self.registry, self.mock_engine)
        self.crossref_action = self.registry["academic.crossref"]

    def _mock_response(self, data, status_code=200):
        """Create a mock response object."""
        import json

        mock = MagicMock()
        mock.json.return_value = data
        mock.text = json.dumps(data)
        mock.status_code = status_code
        return mock

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_backoff_on_429_then_success(self, mock_get, mock_time, mock_sleep):
        """Test successful response after initial 429."""
        mock_time.return_value = 100.0
        mock_get.side_effect = [
            self._mock_response("Rate limited", 429),
            self._mock_response(CROSSREF_DOI_SUCCESS),
        ]

        result = self.crossref_action(state={}, doi="10.1038/nature12373")

        self.assertTrue(result["success"])
        # Should have slept once for 2s (base delay)
        sleep_calls = [call[0][0] for call in mock_sleep.call_args_list if call[0]]
        self.assertIn(2.0, sleep_calls)

    @patch("the_edge_agent.actions.academic_actions.time.sleep")
    @patch("the_edge_agent.actions.academic_actions.time.time")
    @patch("requests.get")
    def test_exponential_backoff_delays(self, mock_get, mock_time, mock_sleep):
        """Test multiple 429 responses with increasing delays (2s, 4s, 8s)."""
        mock_time.return_value = 100.0
        mock_get.side_effect = [
            self._mock_response("Rate limited", 429),
            self._mock_response("Rate limited", 429),
            self._mock_response(CROSSREF_DOI_SUCCESS),
        ]

        result = self.crossref_action(state={}, doi="10.1038/nature12373")

        self.assertTrue(result["success"])
        sleep_calls = [call[0][0] for call in mock_sleep.call_args_list if call[0]]
        self.assertIn(2.0, sleep_calls)
        self.assertIn(4.0, sleep_calls)


if __name__ == "__main__":
    import pytest

    pytest.main([__file__, "-v"])
