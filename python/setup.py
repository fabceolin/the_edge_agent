from setuptools import setup, find_packages
from pathlib import Path

# Read README.md from parent directory if available (for development installs)
# For wheel builds, use a fallback description
readme_path = Path(__file__).parent.parent / "README.md"
if readme_path.exists():
    long_description = readme_path.read_text(encoding="utf-8")
else:
    long_description = "A lightweight, single-app state graph library inspired by LangGraph, to run on edge computing. See https://github.com/fabceolin/the_edge_agent for full documentation."

setup(
    name="the_edge_agent",
    version="0.9.100",
    author="Fabricio Ceolin",
    author_email="fabceolin@gmail.com",
    description="A lightweight, single-app state graph library inspired by LangGraph, to run on edge computing",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/fabceolin/the_edge_agent",
    package_dir={"": "src"},
    packages=find_packages(where="src"),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.7",
    install_requires=[
        "networkx==3.3",
        # "pygraphviz==1.13",  # REMOVED: Requires gcc, not available in Firebase emulator
        "pyyaml==6.0.3",
        "jinja2==3.1.6",  # TEA-YAML-001: Template engine for declarative workflows
        "jmespath==1.0.1",
        "jsonschema==4.25.1",
        "fsspec==2025.12.0",
        "typer==0.20.0",  # TEA-CLI-004: CLI subcommand support
        "phart==1.1.4",  # TEA-CLI-006: ASCII graph rendering for --show-graph
        "python-dotenv==1.2.1",  # TEA-KIROKU-005: Load .env files for API keys
        "pydantic==2.12.4",  # TEA-BUILTIN-015: Required for session, error_handling, http settings
        "pydot==4.0.1",  # TEA-TOOLS-001: DOT/Graphviz parsing for tea from dot
        "requests==2.32.5",  # TEA-RALPHY-001.3: Required by github_actions, web_actions, core_actions
    ],
    entry_points={
        "console_scripts": [
            "tea=the_edge_agent.cli:main",
            # Legacy alias (deprecated, will be removed in v1.0)
            "tea-agent=the_edge_agent.cli:main",
            # TEA-BUILTIN-005.4: Experiment framework CLI
            "tea-experiments=the_edge_agent.experiments.cli:main",
        ],
    },
    extras_require={
        "dev": [
            "pytest==9.0.1",
            "pytest-asyncio==0.25.3",  # For async test support
            "coverage==7.12.0",
            "hypothesis==6.148.7",
            "parameterized==0.9.0",
            # Test dependencies for optional features
            "openai==2.9.0",  # For LLM/RAG tests (mocked but module must be importable)
            "litellm==1.82.6",  # For LiteLLM provider tests (TEA-LLM-003)
            "requests==2.32.5",  # For web actions tests
            "RestrictedPython==8.1",  # For code execution tests
            "duckdb==1.2.2",  # Pinned: DuckPGQ extension only available up to v1.2.2
            "opik==1.9.57",  # For observability tests
            "pygraphviz==1.13",  # For graph visualization tests (requires libgraphviz-dev)
            "pycozo[embedded]==0.7.6",  # For graph memory tests (CozoDB backend)
            "pandas==3.0.1",  # Required by pycozo
            "lupa==2.6",  # For Lua runtime tests
            "janus-swi==1.5.2",  # For Prolog runtime tests (requires SWI-Prolog 9.1+)
            "nltk==3.9.4",  # For text_actions tests (sentence tokenization)
        ],
        "rag": ["openai==2.9.0", "numpy==2.3.5"],
        "rag-chroma": ["openai==2.9.0", "numpy==2.3.5", "chromadb==1.1.1"],
        "search": [
            "model2vec>=0.3.0",  # TEA-BUILTIN-002.4: Local embeddings (no installed version available)
            "lancedb==0.25.3",  # TEA-BUILTIN-002.5: Persistent vector store
            "pyarrow==22.0.0",  # Required by lancedb
            "numpy==2.3.5",
        ],
        "llm": ["openai==2.9.0"],
        "litellm": ["litellm==1.82.6"],  # TEA-LLM-003: LiteLLM multi-provider support
        "web": ["requests==2.32.5"],
        "web-ai-scrape": [
            "scrapegraph-py==1.46.0",
            "pydantic==2.12.4",
        ],  # TEA-BUILTIN-008.4: ScrapeGraphAI
        "code": ["RestrictedPython==8.1"],
        "graph": [
            "pycozo[embedded]==0.7.6",
            "pandas==3.0.1",
        ],  # pandas required by pycozo
        "graph-kuzu": ["kuzu==0.11.3"],
        # Note: graph-bighorn should be installed manually from GitHub:
        # pip install git+https://github.com/Kineviz/bighorn.git
        # Using kuzu as fallback since bighorn is a kuzu fork
        "graph-bighorn": ["kuzu==0.11.3"],
        "tools-crewai": ["crewai==1.6.1", "crewai-tools==1.6.1"],
        "tools-mcp": ["mcp==1.23.1"],
        "tools-langchain": ["langchain==1.1.2", "langchain-community==0.4.1"],
        "tools": [
            "crewai==1.6.1",
            "crewai-tools==1.6.1",
            "mcp==1.23.1",
            "langchain==1.1.2",
            "langchain-community==0.4.1",
        ],
        "lua": ["lupa==2.6"],
        "prolog": ["janus-swi==1.5.2"],  # Requires SWI-Prolog 9.1+
        # TEA-BUILTIN-005.4: Opik experiment framework
        "opik": ["opik==1.9.57"],
        "experiments": ["opik==1.9.57"],  # Alias for opik
        "storage-s3": ["s3fs==2026.2.0"],
        "storage-gcs": ["gcsfs==2026.2.0"],
        "storage-azure": ["adlfs==2026.2.0"],
        "storage-all": ["s3fs==2026.2.0", "gcsfs==2026.2.0", "adlfs==2026.2.0"],
        # TEA-LTM-012: PostgreSQL support for SQLAlchemy LTM backend
        "ltm-postgres": ["psycopg2-binary==2.9.11", "sqlalchemy==2.0.44"],
        # TEA-BUILTIN-012.2: Cloud secrets backends
        "aws": ["boto3==1.42.77"],
        "azure": ["azure-identity==1.25.3", "azure-keyvault-secrets==4.10.0"],
        "gcp": ["google-cloud-secret-manager==2.27.0"],
        "secrets": [
            "boto3==1.42.77",
            "azure-identity==1.25.3",
            "azure-keyvault-secrets==4.10.0",
            "google-cloud-secret-manager==2.27.0",
        ],
        # TEA-AGENT-001.6: Mem0 Memory Integration
        "mem0": ["mem0ai==1.0.8"],
        # TEA-AGENT-001.7: DSPy Prompt Optimization
        "dspy": ["dspy-ai==3.1.3"],
        # TEA-AGENT-001.8: LlamaIndex RAG Bridge
        "llamaindex": [
            "llama-index==0.14.19",
            "llama-index-core==0.14.19",
        ],
        # TEA-AGENT-001.9: TextGrad Learning (P2)
        "textgrad": ["textgrad==0.1.8"],
        # Convenience bundle for all agentic pattern integrations
        "agentic": [
            "mem0ai==1.0.8",
            "dspy-ai==3.1.3",
            "llama-index==0.14.19",
            "llama-index-core==0.14.19",
            # textgrad excluded by default (P2)
        ],
        # TEA-RELEASE-004.5: Local LLM support via llama-cpp-python
        "llm-local": [
            "llama-cpp-python==0.3.19",
        ],
        "all": [
            "openai==2.9.0",
            "litellm==1.82.6",  # TEA-LLM-003: LiteLLM multi-provider support
            "numpy==2.3.5",
            "chromadb==1.1.1",
            "requests==2.32.5",
            "RestrictedPython==8.1",
            "pycozo[embedded]==0.7.6",
            "pandas==3.0.1",  # required by pycozo
            "s3fs==2026.2.0",
            "gcsfs==2026.2.0",
            "adlfs==2026.2.0",
            "lupa==2.6",
            "janus-swi==1.5.2",  # Requires SWI-Prolog 9.1+
            "scrapegraph-py==1.46.0",  # TEA-BUILTIN-008.4: ScrapeGraphAI
            "pydantic==2.12.4",
            "psycopg2-binary==2.9.11",  # TEA-LTM-012: PostgreSQL for SQLAlchemy LTM
            "sqlalchemy==2.0.44",  # TEA-LTM-012: SQLAlchemy LTM backend
        ],
    },
)
