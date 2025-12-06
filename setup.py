from setuptools import setup, find_packages
import sys

setup(
    name="the_edge_agent",
    version="0.4.1",
    author="Fabricio Ceolin",
    author_email="fabceolin@gmail.com",
    description="A lightweight, single-app state graph library inspired by LangGraph, to run on edge computing",
    long_description=open("README.md").read(),
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
        "pygraphviz==1.13",
        "pyyaml>=6.0",
    ],
    extras_require={
        "dev": ["pytest", "coverage", "hypothesis","parameterized==0.9.0"],
    },
)
