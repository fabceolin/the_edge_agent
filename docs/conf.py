# Configuration file for the Sphinx documentation builder.
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
project = "The Edge Agent"
copyright = "2024, Fabricio Ceolin"
author = "Fabricio Ceolin"

# -- General configuration ---------------------------------------------------
extensions = [
    "myst_parser",  # Markdown support
    "sphinx_external_toc",  # Support for _toc.yml
    "sphinx.ext.autodoc",  # API documentation
    "sphinx.ext.viewcode",  # Source code links
    "sphinx_copybutton",  # Copy button for code blocks
    "sphinx_togglebutton",  # Collapsible content
]

# External TOC configuration
external_toc_path = "_toc.yml"
external_toc_exclude_missing = True

# MyST Parser configuration
myst_enable_extensions = [
    "colon_fence",  # ::: directive syntax
    "deflist",  # Definition lists
    "dollarmath",  # Math with $...$
    "fieldlist",  # Field lists
    "html_admonition",  # HTML admonitions
    "html_image",  # HTML images
    "linkify",  # Auto-link URLs
    "replacements",  # Text replacements
    "smartquotes",  # Smart quotes
    "strikethrough",  # ~~strikethrough~~
    "substitution",  # Substitutions
    "tasklist",  # Task lists
]

myst_heading_anchors = 3

# Source file settings
source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}
master_doc = "index"

# Exclude patterns
exclude_patterns = [
    "_build",
    "Thumbs.db",
    ".DS_Store",
    "qa/**",  # Exclude QA assessments
    "stories/**",  # Exclude stories
]

# -- Options for HTML output -------------------------------------------------
html_theme = "sphinx_book_theme"

html_theme_options = {
    "repository_url": "https://github.com/fabceolin/the_edge_agent",
    "repository_branch": "main",
    "path_to_docs": "docs",
    "use_repository_button": True,
    "use_edit_page_button": True,
    "use_issues_button": True,
    "use_download_button": True,
    "use_fullscreen_button": True,
    "home_page_in_toc": True,
    "show_navbar_depth": 2,
    "show_toc_level": 2,
    "navigation_with_keys": True,
    "collapse_navigation": False,
    "logo": {
        "text": "The Edge Agent",
    },
    "icon_links": [
        {
            "name": "GitHub",
            "url": "https://github.com/fabceolin/the_edge_agent",
            "icon": "fa-brands fa-github",
            "type": "fontawesome",
        },
    ],
}

html_title = "The Edge Agent"
html_baseurl = "https://fabceolin.github.io/the_edge_agent/"

# Static files
html_static_path = ["_static"]

# Custom CSS (create if you want to customize further)
html_css_files = [
    "custom.css",
]

# -- Options for linkcheck ---------------------------------------------------
linkcheck_ignore = [
    r"http://localhost:\d+",
]
