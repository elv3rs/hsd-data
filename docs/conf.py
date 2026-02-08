# Configuration file for the Sphinx documentation builder.

import os

# -- Project information -----------------------------------------------------

project = 'hsd-data'
copyright = '2026, DFTB+ developers group'
author = 'DFTB+ developers group'
release = '1.0.0'
version = '1.0.0'

# -- General configuration ---------------------------------------------------

extensions = [
    'myst_parser',  # For Markdown support
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

master_doc = 'index'

# -- Options for HTML output -------------------------------------------------

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

html_theme_options = {
    'navigation_depth': 4,
    'collapse_navigation': False,
    'sticky_navigation': True,
    'includehidden': True,
    'titles_only': False,
}

html_context = {
    'display_github': True,
    'github_user': 'dftbplus',
    'github_repo': 'hsd-data',
    'github_version': 'main',
    'conf_py_path': '/docs/',
}

# -- Options for MyST parser -------------------------------------------------

myst_enable_extensions = [
    'colon_fence',
    'deflist',
    'fieldlist',
    'tasklist',
]

myst_heading_anchors = 3

# -- Custom settings ---------------------------------------------------------

os.makedirs('_static', exist_ok=True)
