from setuptools import setup, find_packages
import sys
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
    version="0.8.7",
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
        "pyyaml>=6.0",
        "jinja2>=3.0",  # TEA-YAML-001: Template engine for declarative workflows
        "jmespath>=1.0.0",
        "jsonschema>=4.20.0",
        "fsspec>=2023.1.0",
        "typer>=0.9.0",  # TEA-CLI-004: CLI subcommand support
    ],
    entry_points={
        "console_scripts": [
            "tea=the_edge_agent.cli:main",
            # Legacy alias (deprecated, will be removed in v1.0)
            "tea-agent=the_edge_agent.cli:main",
        ],
    },
    extras_require={
        "dev": [
            "pytest",
            "coverage",
            "hypothesis",
            "parameterized==0.9.0",
            # Test dependencies for optional features
            "openai>=1.13.3",  # For LLM/RAG tests (mocked but module must be importable)
            "litellm>=1.0.0",  # For LiteLLM provider tests (TEA-LLM-003)
            "requests>=2.32.5",  # For web actions tests
            "RestrictedPython>=8.0",  # For code execution tests
            "duckdb>=0.10.0",  # For tabular data tests (data_query, data_consolidate)
            "opik>=1.9.0",  # For observability tests
            "pygraphviz>=1.13",  # For graph visualization tests (requires libgraphviz-dev)
            "pycozo[embedded]>=0.7.0",  # For graph memory tests (CozoDB backend)
            "pandas>=2.0.0",  # Required by pycozo
            "lupa>=2.0",  # For Lua runtime tests
            "janus-swi>=0.1.0",  # For Prolog runtime tests (requires SWI-Prolog 9.1+)
        ],
        "rag": ["openai>=1.13.3", "numpy>=2.1.0"],
        "rag-chroma": ["openai>=1.13.3", "numpy>=2.1.0", "chromadb>=0.4.0"],
        "llm": ["openai>=1.13.3"],
        "litellm": ["litellm>=1.0.0"],  # TEA-LLM-003: LiteLLM multi-provider support
        "web": ["requests>=2.32.5"],
        "web-ai-scrape": [
            "scrapegraph-py>=1.0.0",
            "pydantic>=2.0.0",
        ],  # TEA-BUILTIN-008.4: ScrapeGraphAI
        "code": ["RestrictedPython>=8.0"],
        "graph": [
            "pycozo[embedded]>=0.7.0",
            "pandas>=2.0.0",
        ],  # pandas required by pycozo
        "graph-kuzu": ["kuzu>=0.11.0"],
        # Note: graph-bighorn should be installed manually from GitHub:
        # pip install git+https://github.com/Kineviz/bighorn.git
        # Using kuzu as fallback since bighorn is a kuzu fork
        "graph-bighorn": ["kuzu>=0.11.0"],
        "tools-crewai": ["crewai>=1.6.0", "crewai-tools>=0.38.0"],
        "tools-mcp": ["mcp>=1.23.0"],
        "tools-langchain": ["langchain>=1.1.0", "langchain-community>=0.4.0"],
        "tools": [
            "crewai>=1.6.0",
            "crewai-tools>=0.38.0",
            "mcp>=1.23.0",
            "langchain>=1.1.0",
            "langchain-community>=0.4.0",
        ],
        "lua": ["lupa>=2.0"],
        "prolog": ["janus-swi>=0.1.0"],  # Requires SWI-Prolog 9.1+
        "storage-s3": ["s3fs"],
        "storage-gcs": ["gcsfs"],
        "storage-azure": ["adlfs"],
        "storage-all": ["s3fs", "gcsfs", "adlfs"],
        "all": [
            "openai>=1.13.3",
            "litellm>=1.0.0",  # TEA-LLM-003: LiteLLM multi-provider support
            "numpy>=2.1.0",
            "chromadb>=0.4.0",
            "requests>=2.32.5",
            "RestrictedPython>=8.0",
            "pycozo[embedded]>=0.7.0",
            "pandas>=2.0.0",  # required by pycozo
            "s3fs",
            "gcsfs",
            "adlfs",
            "lupa>=2.0",
            "janus-swi>=0.1.0",  # Requires SWI-Prolog 9.1+
            "scrapegraph-py>=1.0.0",  # TEA-BUILTIN-008.4: ScrapeGraphAI
            "pydantic>=2.0.0",
        ],
    },
)
