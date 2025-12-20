from setuptools import setup, find_packages
import sys

setup(
    name="the_edge_agent",
    version="0.6.12",
    author="Fabricio Ceolin",
    author_email="fabceolin@gmail.com",
    description="A lightweight, single-app state graph library inspired by LangGraph, to run on edge computing",
    long_description=open("../README.md").read(),
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
        "jmespath>=1.0.0",
        "jsonschema>=4.20.0",
        "fsspec>=2023.1.0",
    ],
    entry_points={
        "console_scripts": [
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
            "openai>=1.13.3",       # For LLM/RAG tests (mocked but module must be importable)
            "requests>=2.32.5",     # For web actions tests
            "RestrictedPython>=8.0",  # For code execution tests
            "duckdb>=0.10.0",       # For tabular data tests (data_query, data_consolidate)
            "opik>=1.9.0",          # For observability tests
            "pygraphviz>=1.13",     # For graph visualization tests (requires libgraphviz-dev)
            "pycozo[embedded]>=0.7.0",  # For graph memory tests (CozoDB backend)
        ],
        "rag": ["openai>=1.13.3", "numpy>=2.1.0"],
        "rag-chroma": ["openai>=1.13.3", "numpy>=2.1.0", "chromadb>=0.4.0"],
        "llm": ["openai>=1.13.3"],
        "web": ["requests>=2.32.5"],
        "code": ["RestrictedPython>=8.0"],
        "graph": ["pycozo[embedded]>=0.7.0"],
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
        "storage-s3": ["s3fs"],
        "storage-gcs": ["gcsfs"],
        "storage-azure": ["adlfs"],
        "storage-all": ["s3fs", "gcsfs", "adlfs"],
        "all": [
            "openai>=1.13.3",
            "numpy>=2.1.0",
            "chromadb>=0.4.0",
            "requests>=2.32.5",
            "RestrictedPython>=8.0",
            "pycozo[embedded]>=0.7.0",
            "s3fs",
            "gcsfs",
            "adlfs",
        ],
    },
)
