"""
Tests for web actions (TEA-BUILTIN-002.1).

Tests cover:
- web.scrape: Firecrawl API integration
- web.crawl: Firecrawl crawl API with job polling
- web.search: Perplexity API integration
- Error handling for missing API keys, rate limits, timeouts
- Dual namespace registration (web.* and actions.web_*)

All external API calls are mocked for unit testing.
"""

import unittest
from unittest.mock import patch, MagicMock
import os

from the_edge_agent import YAMLEngine


class TestWebActionsRegistration(unittest.TestCase):
    """Test that web actions are properly registered."""

    def setUp(self):
        self.engine = YAMLEngine()

    def test_web_actions_dual_namespace(self):
        """Verify web actions are registered under both namespaces."""
        # web.* namespace
        self.assertIn('web.scrape', self.engine.actions_registry)
        self.assertIn('web.crawl', self.engine.actions_registry)
        self.assertIn('web.search', self.engine.actions_registry)

        # actions.web_* namespace
        self.assertIn('actions.web_scrape', self.engine.actions_registry)
        self.assertIn('actions.web_crawl', self.engine.actions_registry)
        self.assertIn('actions.web_search', self.engine.actions_registry)

    def test_web_scrape_and_actions_web_scrape_same_function(self):
        """Verify both namespaces point to the same function."""
        self.assertIs(
            self.engine.actions_registry['web.scrape'],
            self.engine.actions_registry['actions.web_scrape']
        )

    def test_web_crawl_and_actions_web_crawl_same_function(self):
        """Verify both namespaces point to the same function."""
        self.assertIs(
            self.engine.actions_registry['web.crawl'],
            self.engine.actions_registry['actions.web_crawl']
        )

    def test_web_search_and_actions_web_search_same_function(self):
        """Verify both namespaces point to the same function."""
        self.assertIs(
            self.engine.actions_registry['web.search'],
            self.engine.actions_registry['actions.web_search']
        )


class TestWebScrapeMissingApiKey(unittest.TestCase):
    """Test web.scrape error handling for missing API key."""

    def setUp(self):
        self.engine = YAMLEngine()
        self.web_scrape = self.engine.actions_registry['web.scrape']
        # Clear the API key for these tests
        self.original_key = os.environ.get('FIRECRAWL_API_KEY')
        if 'FIRECRAWL_API_KEY' in os.environ:
            del os.environ['FIRECRAWL_API_KEY']

    def tearDown(self):
        if self.original_key:
            os.environ['FIRECRAWL_API_KEY'] = self.original_key
        elif 'FIRECRAWL_API_KEY' in os.environ:
            del os.environ['FIRECRAWL_API_KEY']

    def test_web_scrape_missing_api_key(self):
        """web.scrape returns error when FIRECRAWL_API_KEY not set."""
        result = self.web_scrape(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'configuration')
        self.assertIn('FIRECRAWL_API_KEY', result['error'])


class TestWebCrawlMissingApiKey(unittest.TestCase):
    """Test web.crawl error handling for missing API key."""

    def setUp(self):
        self.engine = YAMLEngine()
        self.web_crawl = self.engine.actions_registry['web.crawl']
        self.original_key = os.environ.get('FIRECRAWL_API_KEY')
        if 'FIRECRAWL_API_KEY' in os.environ:
            del os.environ['FIRECRAWL_API_KEY']

    def tearDown(self):
        if self.original_key:
            os.environ['FIRECRAWL_API_KEY'] = self.original_key
        elif 'FIRECRAWL_API_KEY' in os.environ:
            del os.environ['FIRECRAWL_API_KEY']

    def test_web_crawl_missing_api_key(self):
        """web.crawl returns error when FIRECRAWL_API_KEY not set."""
        result = self.web_crawl(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'configuration')
        self.assertIn('FIRECRAWL_API_KEY', result['error'])


class TestWebSearchMissingApiKey(unittest.TestCase):
    """Test web.search error handling for missing API key."""

    def setUp(self):
        self.engine = YAMLEngine()
        self.web_search = self.engine.actions_registry['web.search']
        self.original_key = os.environ.get('PERPLEXITY_API_KEY')
        if 'PERPLEXITY_API_KEY' in os.environ:
            del os.environ['PERPLEXITY_API_KEY']

    def tearDown(self):
        if self.original_key:
            os.environ['PERPLEXITY_API_KEY'] = self.original_key
        elif 'PERPLEXITY_API_KEY' in os.environ:
            del os.environ['PERPLEXITY_API_KEY']

    def test_web_search_missing_api_key(self):
        """web.search returns error when PERPLEXITY_API_KEY not set."""
        result = self.web_search(state={}, query="test query")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'configuration')
        self.assertIn('PERPLEXITY_API_KEY', result['error'])


class TestWebScrapeWithMocks(unittest.TestCase):
    """Test web.scrape functionality with mocked API responses."""

    def setUp(self):
        self.engine = YAMLEngine()
        self.web_scrape = self.engine.actions_registry['web.scrape']

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_returns_markdown(self, mock_post):
        """web.scrape returns markdown content on success."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "success": True,
            "data": {
                "markdown": "# Test Page\n\nThis is test content.",
                "metadata": {
                    "title": "Test Page",
                    "description": "A test page",
                    "language": "en",
                    "statusCode": 200
                }
            }
        }
        mock_post.return_value = mock_response

        result = self.web_scrape(
            state={},
            url="https://example.com"
        )

        self.assertTrue(result['success'])
        self.assertEqual(result['url'], "https://example.com")
        self.assertEqual(result['markdown'], "# Test Page\n\nThis is test content.")
        self.assertEqual(result['metadata']['title'], "Test Page")

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_with_formats(self, mock_post):
        """web.scrape respects formats parameter."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "success": True,
            "data": {
                "markdown": "# Test",
                "html": "<h1>Test</h1>",
                "links": ["https://example.com/page1", "https://example.com/page2"],
                "metadata": {}
            }
        }
        mock_post.return_value = mock_response

        result = self.web_scrape(
            state={},
            url="https://example.com",
            formats=["markdown", "html", "links"]
        )

        self.assertTrue(result['success'])
        self.assertIn('markdown', result)
        self.assertIn('html', result)
        self.assertIn('links', result)
        self.assertEqual(len(result['links']), 2)

        # Verify formats were sent in request
        call_args = mock_post.call_args
        payload = call_args.kwargs.get('json') or call_args[1].get('json')
        self.assertEqual(payload['formats'], ["markdown", "html", "links"])

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_with_extract_schema(self, mock_post):
        """web.scrape handles structured extraction with schema."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "success": True,
            "data": {
                "markdown": "# Products",
                "extract": {
                    "products": [
                        {"name": "Widget", "price": 9.99},
                        {"name": "Gadget", "price": 19.99}
                    ]
                },
                "metadata": {}
            }
        }
        mock_post.return_value = mock_response

        schema = {
            "type": "object",
            "properties": {
                "products": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "price": {"type": "number"}
                        }
                    }
                }
            }
        }

        result = self.web_scrape(
            state={},
            url="https://example.com/products",
            extract_schema=schema
        )

        self.assertTrue(result['success'])
        self.assertIn('extract', result)
        self.assertEqual(len(result['extract']['products']), 2)

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_with_extract_prompt(self, mock_post):
        """web.scrape handles structured extraction with natural language prompt."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "success": True,
            "data": {
                "markdown": "Article by John Doe",
                "extract": {
                    "author": "John Doe",
                    "date": "2025-01-15"
                },
                "metadata": {}
            }
        }
        mock_post.return_value = mock_response

        result = self.web_scrape(
            state={},
            url="https://example.com/article",
            extract_prompt="Extract the author name and publication date"
        )

        self.assertTrue(result['success'])
        self.assertIn('extract', result)
        self.assertEqual(result['extract']['author'], "John Doe")

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_with_browser_actions(self, mock_post):
        """web.scrape handles browser actions before scraping."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "success": True,
            "data": {
                "markdown": "# Loaded Content",
                "metadata": {}
            }
        }
        mock_post.return_value = mock_response

        actions = [
            {"type": "click", "selector": "#load-more"},
            {"type": "wait", "milliseconds": 2000}
        ]

        result = self.web_scrape(
            state={},
            url="https://example.com",
            actions=actions
        )

        self.assertTrue(result['success'])

        # Verify actions were sent in request
        call_args = mock_post.call_args
        payload = call_args.kwargs.get('json') or call_args[1].get('json')
        self.assertEqual(payload['actions'], actions)

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_handles_rate_limit(self, mock_post):
        """web.scrape returns rate_limit error on HTTP 429."""
        mock_response = MagicMock()
        mock_response.status_code = 429
        mock_response.text = "Rate limit exceeded"
        mock_post.return_value = mock_response

        result = self.web_scrape(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'rate_limit')

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_handles_payment_required(self, mock_post):
        """web.scrape returns payment_required error on HTTP 402."""
        mock_response = MagicMock()
        mock_response.status_code = 402
        mock_response.text = "Payment required"
        mock_post.return_value = mock_response

        result = self.web_scrape(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'payment_required')

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_handles_timeout(self, mock_post):
        """web.scrape returns timeout error on request timeout."""
        import requests
        mock_post.side_effect = requests.exceptions.Timeout()

        result = self.web_scrape(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'timeout')

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_handles_connection_error(self, mock_post):
        """web.scrape returns connection error on network failure."""
        import requests
        mock_post.side_effect = requests.exceptions.ConnectionError("Network unreachable")

        result = self.web_scrape(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'connection')


class TestWebCrawlWithMocks(unittest.TestCase):
    """Test web.crawl functionality with mocked API responses."""

    def setUp(self):
        self.engine = YAMLEngine()
        self.web_crawl = self.engine.actions_registry['web.crawl']

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.get')
    @patch('requests.post')
    def test_web_crawl_returns_pages(self, mock_post, mock_get):
        """web.crawl returns crawled pages on success."""
        # Mock the job start response
        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {
            "success": True,
            "id": "job-123"
        }
        mock_post.return_value = mock_post_response

        # Mock the job completion response
        mock_get_response = MagicMock()
        mock_get_response.status_code = 200
        mock_get_response.json.return_value = {
            "status": "completed",
            "data": [
                {
                    "markdown": "# Page 1",
                    "metadata": {"sourceURL": "https://example.com/page1", "title": "Page 1"}
                },
                {
                    "markdown": "# Page 2",
                    "metadata": {"sourceURL": "https://example.com/page2", "title": "Page 2"}
                }
            ]
        }
        mock_get.return_value = mock_get_response

        result = self.web_crawl(
            state={},
            url="https://example.com",
            max_depth=2,
            limit=10
        )

        self.assertTrue(result['success'])
        self.assertEqual(result['total_pages'], 2)
        self.assertEqual(result['job_id'], 'job-123')
        self.assertEqual(len(result['pages']), 2)
        self.assertEqual(result['pages'][0]['url'], 'https://example.com/page1')

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.get')
    @patch('requests.post')
    def test_web_crawl_with_path_filters(self, mock_post, mock_get):
        """web.crawl respects include/exclude path filters."""
        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"success": True, "id": "job-123"}
        mock_post.return_value = mock_post_response

        mock_get_response = MagicMock()
        mock_get_response.status_code = 200
        mock_get_response.json.return_value = {"status": "completed", "data": []}
        mock_get.return_value = mock_get_response

        result = self.web_crawl(
            state={},
            url="https://example.com",
            include_paths=["/docs/*"],
            exclude_paths=["/admin/*"]
        )

        # Verify path filters were sent in request
        call_args = mock_post.call_args
        payload = call_args.kwargs.get('json') or call_args[1].get('json')
        self.assertEqual(payload['includePaths'], ["/docs/*"])
        self.assertEqual(payload['excludePaths'], ["/admin/*"])

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.get')
    @patch('requests.post')
    def test_web_crawl_handles_job_failure(self, mock_post, mock_get):
        """web.crawl returns error when crawl job fails."""
        mock_post_response = MagicMock()
        mock_post_response.status_code = 200
        mock_post_response.json.return_value = {"success": True, "id": "job-123"}
        mock_post.return_value = mock_post_response

        mock_get_response = MagicMock()
        mock_get_response.status_code = 200
        mock_get_response.json.return_value = {
            "status": "failed",
            "error": "Crawl job failed due to invalid URL"
        }
        mock_get.return_value = mock_get_response

        result = self.web_crawl(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'api_error')
        self.assertIn('Crawl job failed', result['error'])

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_crawl_handles_rate_limit(self, mock_post):
        """web.crawl returns rate_limit error on HTTP 429."""
        mock_response = MagicMock()
        mock_response.status_code = 429
        mock_response.text = "Rate limit exceeded"
        mock_post.return_value = mock_response

        result = self.web_crawl(state={}, url="https://example.com")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'rate_limit')


class TestWebSearchWithMocks(unittest.TestCase):
    """Test web.search functionality with mocked API responses."""

    def setUp(self):
        self.engine = YAMLEngine()
        self.web_search = self.engine.actions_registry['web.search']

    @patch.dict(os.environ, {'PERPLEXITY_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_search_returns_results(self, mock_post):
        """web.search returns search results on success."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "choices": [
                {
                    "message": {
                        "content": "Here is information about AI developments in 2025..."
                    }
                }
            ],
            "citations": [
                {"title": "AI News Article", "url": "https://example.com/ai-news", "snippet": "Latest AI news"},
                {"title": "Tech Report", "url": "https://example.com/report", "snippet": "AI developments"}
            ]
        }
        mock_post.return_value = mock_response

        result = self.web_search(
            state={},
            query="AI developments 2025",
            num_results=5
        )

        self.assertTrue(result['success'])
        self.assertEqual(result['query'], "AI developments 2025")
        self.assertEqual(result['total_results'], 2)
        self.assertEqual(len(result['results']), 2)
        self.assertIn('answer', result)
        self.assertEqual(result['results'][0]['title'], "AI News Article")
        self.assertEqual(result['results'][0]['position'], 1)

    @patch.dict(os.environ, {'PERPLEXITY_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_search_handles_url_only_citations(self, mock_post):
        """web.search handles citations that are just URLs."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "choices": [{"message": {"content": "Answer"}}],
            "citations": [
                "https://example.com/page1",
                "https://example.com/page2"
            ]
        }
        mock_post.return_value = mock_response

        result = self.web_search(state={}, query="test")

        self.assertTrue(result['success'])
        self.assertEqual(result['results'][0]['url'], "https://example.com/page1")

    @patch.dict(os.environ, {'PERPLEXITY_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_search_handles_rate_limit(self, mock_post):
        """web.search returns rate_limit error on HTTP 429."""
        mock_response = MagicMock()
        mock_response.status_code = 429
        mock_response.text = "Rate limit exceeded"
        mock_post.return_value = mock_response

        result = self.web_search(state={}, query="test")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'rate_limit')

    @patch.dict(os.environ, {'PERPLEXITY_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_search_handles_timeout(self, mock_post):
        """web.search returns timeout error on request timeout."""
        import requests
        mock_post.side_effect = requests.exceptions.Timeout()

        result = self.web_search(state={}, query="test")

        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'timeout')

    @patch.dict(os.environ, {'PERPLEXITY_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_search_zero_results(self, mock_post):
        """web.search handles empty search results gracefully."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "choices": [{"message": {"content": "No results found"}}],
            "citations": []
        }
        mock_post.return_value = mock_response

        result = self.web_search(state={}, query="very obscure query")

        self.assertTrue(result['success'])
        self.assertEqual(result['total_results'], 0)
        self.assertEqual(result['results'], [])


class TestWebActionsInWorkflow(unittest.TestCase):
    """Integration tests for web actions in YAML workflows."""

    def setUp(self):
        self.engine = YAMLEngine()

    @patch.dict(os.environ, {'FIRECRAWL_API_KEY': 'test-api-key'})
    @patch('requests.post')
    def test_web_scrape_in_yaml_workflow(self, mock_post):
        """web.scrape can be used in a YAML workflow."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "success": True,
            "data": {
                "markdown": "# Test Content",
                "metadata": {"title": "Test"}
            }
        }
        mock_post.return_value = mock_response

        config = {
            "state_schema": {"url": "str", "content": "dict"},
            "nodes": [
                {
                    "name": "scrape_page",
                    "uses": "web.scrape",
                    "with": {
                        "url": "{{ state.url }}"
                    },
                    "output": "content"
                }
            ],
            "edges": [
                {"from": "__start__", "to": "scrape_page"},
                {"from": "scrape_page", "to": "__end__"}
            ]
        }

        graph = self.engine.load_from_dict(config)
        events = list(graph.invoke({"url": "https://example.com"}))

        # Find final event
        final_event = next(e for e in events if e.get('type') == 'final')
        state = final_event['state']

        self.assertIn('content', state)
        self.assertTrue(state['content']['success'])
        self.assertEqual(state['content']['markdown'], "# Test Content")


if __name__ == '__main__':
    unittest.main()
