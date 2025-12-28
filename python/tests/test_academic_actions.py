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
    def test_pubmed_rate_limit(self, mock_get, mock_time, mock_sleep):
        """Test rate limit error handling (HTTP 429)."""
        mock_time.return_value = 100.0
        mock_get.return_value = self._mock_response("Rate limit exceeded", 429)

        result = self.pubmed_action(state={}, query="test")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_code"], "rate_limit")
        self.assertIn("rate limit", result["error"].lower())

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
        # Simulate recent request (0.5 seconds ago)
        from the_edge_agent.actions import academic_actions

        academic_actions._last_arxiv_request = 99.5
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


if __name__ == "__main__":
    import pytest

    pytest.main([__file__, "-v"])
