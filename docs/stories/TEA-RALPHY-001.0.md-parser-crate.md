# Story TEA-RALPHY-001.0: md-parser Shared Crate

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** library maintainer,
**I want** a shared Rust crate for structured Markdown parsing,
**So that** both TEA and agentfs can use the same parser with guaranteed schema parity.

## Repository

- **URL**: https://github.com/fabceolin/md-parser
- **Local scaffold**: `/home/fabricio/src/md-parser/`

## Acceptance Criteria

### Core Parsing (P0)
1. Parse Markdown into structured sections (heading, paragraph, list, code, blockquote, hr)
2. Extract document title from first H1
3. Generate section IDs (UUID or sequential)
4. Track section order index
5. Create "follows" edges between sequential sections

### Checklist Extraction (P0)
6. Extract `- [ ]` and `- [x]` checklist items with completion status
7. Support nested items with indent tracking (2 spaces = 1 level)
8. Extract AC references from `(AC: 1, 2)` pattern
9. Provide `ChecklistSummary` with completion percentage

### Variable Detection (P0)
10. Detect `{{variable_name}}` template variables
11. Collect unique variable names per section and document-wide

### Frontmatter (P1 - Feature-gated)
12. Parse YAML frontmatter (`---` delimited)
13. Return as `HashMap<String, Value>`

### PyO3 Python Bindings (P1 - Feature-gated)
14. Export `MarkdownParser` class to Python
15. Export `ParsedDocument`, `ParsedSection`, `ChecklistItem` as Python classes
16. Pre-built wheels available via GitHub Release (no PyPI)

### CI/CD (P0)
17. GitHub Actions workflow for Rust CI (test, clippy, fmt)
18. GitHub Actions workflow for Python wheel builds (maturin, multi-platform)
19. GitHub Actions workflow for WASM builds
20. Automated GitHub Release with wheels + WASM artifacts (no crates.io/PyPI)

### Quality
21. Serde serialization for all types (feature-gated)
22. Comprehensive unit tests (>90% coverage)
23. No unsafe code
24. WASM-compatible (no std dependencies that break wasm32, pyo3 feature excluded)

## Tasks / Subtasks

### Phase 1: Repository Setup
- [ ] Create GitHub repository (AC: N/A)
  - [ ] Initialize at https://github.com/fabceolin/md-parser
  - [ ] Add MIT license
  - [ ] Add .gitignore for Rust
- [ ] Set up Cargo.toml (AC: 21)
  - [ ] Define features: `default`, `serde`, `frontmatter`, `pyo3`
  - [ ] Add dependencies: `pulldown-cmark`, `regex`, `thiserror`
  - [ ] Add optional deps: `serde`, `serde_yaml`, `pyo3`
- [ ] Set up GitHub Actions (AC: 17, 18, 19, 20)
  - [ ] Create `.github/workflows/ci.yml` (Rust CI)
  - [ ] Create `.github/workflows/python.yml` (Python wheels)
  - [ ] Create `.github/workflows/wasm.yml` (WASM builds)
  - [ ] Create `.github/workflows/release.yml` (automated release)

### Phase 2: Core Types
- [ ] Create `section.rs` (AC: 1, 3, 4)
  - [ ] `SectionType` enum with `as_str()` method
  - [ ] `ParsedSection` struct with builder pattern
  - [ ] Serde derives behind feature flag
- [ ] Create `document.rs` (AC: 2, 5)
  - [ ] `ParsedDocument` struct
  - [ ] `ParsedEdge` struct with `EdgeType` enum
- [ ] Create `checklist.rs` (AC: 6, 7, 8, 9)
  - [ ] `ChecklistItem` struct
  - [ ] `ChecklistSummary` struct
  - [ ] `extract_checklist_items()` function
  - [ ] AC reference regex extraction
- [ ] Create `variables.rs` (AC: 10, 11)
  - [ ] `extract_variables()` function
  - [ ] Regex for `{{variable_name}}`

### Phase 3: Parser Implementation
- [ ] Create `parser.rs` (AC: 1, 2, 3, 4, 5)
  - [ ] Port from agentfs `graphdocs/parser.rs`
  - [ ] `MarkdownParser` struct with `parse()` method
  - [ ] Section flushing logic
  - [ ] Edge generation
- [ ] Create `frontmatter.rs` (AC: 12, 13)
  - [ ] Feature-gated behind `frontmatter`
  - [ ] Detect and strip YAML header
  - [ ] Parse with `serde_yaml`

### Phase 4: Public API
- [ ] Create `lib.rs` (AC: 21)
  - [ ] Re-export all public types
  - [ ] Module organization
  - [ ] Crate-level documentation
- [ ] Create `error.rs`
  - [ ] `ParseError` enum with thiserror

### Phase 5: PyO3 Python Bindings
- [ ] Create `python.rs` (AC: 14, 15)
  - [ ] Feature-gated behind `pyo3`
  - [ ] `#[pyclass]` for `MarkdownParser`, `ParsedDocument`, `ParsedSection`, `ChecklistItem`
  - [ ] `#[pymethods]` for parser API
  - [ ] `#[pymodule]` for `md_parser` module
- [ ] Configure maturin build (AC: 16)
  - [ ] Add `pyproject.toml` with maturin backend
  - [ ] Configure wheel metadata for GitHub Release

### Phase 6: Testing & Release
- [ ] Add unit tests (AC: 22)
  - [ ] Test each section type
  - [ ] Test checklist extraction
  - [ ] Test AC reference parsing
  - [ ] Test variable detection
  - [ ] Test frontmatter parsing
  - [ ] Test malformed input handling
- [ ] Add integration tests
  - [ ] Parse real BMad story files
  - [ ] Parse agentfs GraphDocs examples
- [ ] Add Python binding tests (AC: 14, 15)
  - [ ] Test `MarkdownParser` from Python
  - [ ] Test dataclass-like access to results
  - [ ] Test round-trip with JSON serialization
- [ ] Verify WASM compatibility (AC: 24)
  - [ ] Build with `--target wasm32-unknown-unknown` (without pyo3 feature)
- [ ] Verify CI passes (AC: 17, 18, 19)
  - [ ] All Rust tests pass
  - [ ] Clippy has no warnings
  - [ ] Python wheels build for all platforms
  - [ ] WASM builds successfully
- [ ] Tag v0.1.0 release (AC: 20)
  - [ ] Push tag to trigger release workflow
  - [ ] Verify GitHub Release with wheels + WASM artifacts

## Dev Notes

### Crate Structure

```
md-parser/
├── .github/
│   └── workflows/
│       ├── ci.yml              # Rust CI (test, clippy, fmt)
│       ├── python.yml          # Python wheel builds
│       ├── wasm.yml            # WASM builds
│       └── release.yml         # Automated release
├── Cargo.toml
├── pyproject.toml          # Maturin build config
├── LICENSE
├── README.md
├── src/
│   ├── lib.rs              # Public API exports
│   ├── error.rs            # ParseError
│   ├── section.rs          # ParsedSection, SectionType
│   ├── document.rs         # ParsedDocument, ParsedEdge
│   ├── parser.rs           # MarkdownParser
│   ├── checklist.rs        # ChecklistItem, extraction
│   ├── variables.rs        # Variable extraction
│   ├── frontmatter.rs      # YAML frontmatter (feature-gated)
│   └── python.rs           # PyO3 bindings (feature-gated)
└── tests/
    ├── integration_tests.rs
    ├── python_tests.py     # Python binding tests
    └── fixtures/
        ├── simple.md
        ├── bmad_story.md
        └── with_frontmatter.md
```

### Cargo.toml

```toml
[package]
name = "md-parser"
version = "0.1.0"
edition = "2021"
description = "Structured Markdown parsing with sections, variables, and checklists"
license = "MIT"
repository = "https://github.com/fabceolin/md-parser"
keywords = ["markdown", "parser", "sections", "checklist", "frontmatter"]
categories = ["parsing", "text-processing"]

[lib]
name = "md_parser"
crate-type = ["cdylib", "rlib"]  # cdylib for Python, rlib for Rust

[dependencies]
pulldown-cmark = { version = "0.12", default-features = false }
regex = "1"
thiserror = "1"

# Optional dependencies
serde = { version = "1", features = ["derive"], optional = true }
serde_yaml = { version = "0.9", optional = true }
pyo3 = { version = "0.22", features = ["extension-module"], optional = true }

[dev-dependencies]
pretty_assertions = "1"

[features]
default = []
serde = ["dep:serde"]
frontmatter = ["dep:serde_yaml", "serde"]
pyo3 = ["dep:pyo3", "serde"]  # Python bindings require serde for dict conversion
```

### pyproject.toml

```toml
[build-system]
requires = ["maturin>=1.0,<2.0"]
build-backend = "maturin"

[project]
name = "md-parser"
description = "Structured Markdown parsing with sections, variables, and checklists"
readme = "README.md"
license = { text = "MIT" }
requires-python = ">=3.8"
classifiers = [
    "Programming Language :: Rust",
    "Programming Language :: Python :: Implementation :: CPython",
]

[tool.maturin]
features = ["pyo3"]
```

### GitHub Actions Workflows

#### `.github/workflows/ci.yml` - Rust CI

```yaml
name: Rust CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  CARGO_TERM_COLOR: always

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      - name: Run tests
        run: cargo test --all-features
      - name: Run tests (no default features)
        run: cargo test --no-default-features

  clippy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: clippy
      - uses: Swatinem/rust-cache@v2
      - name: Clippy
        run: cargo clippy --all-features -- -D warnings

  fmt:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: rustfmt
      - name: Format check
        run: cargo fmt --all -- --check

  docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      - name: Build docs
        run: cargo doc --all-features --no-deps
        env:
          RUSTDOCFLAGS: -D warnings
```

#### `.github/workflows/python.yml` - Python Wheels

```yaml
name: Python Wheels

on:
  push:
    branches: [main]
    tags: ["v*"]
  pull_request:
    branches: [main]

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        python-version: ["3.9", "3.10", "3.11", "3.12"]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: ${{ matrix.python-version }}
      - name: Install maturin
        run: pip install maturin
      - name: Build wheel
        run: maturin build --release --features pyo3
      - name: Upload wheel
        uses: actions/upload-artifact@v4
        with:
          name: wheel-${{ matrix.os }}-py${{ matrix.python-version }}
          path: target/wheels/*.whl

  # Build for Linux with manylinux
  linux-manylinux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: PyO3/maturin-action@v1
        with:
          manylinux: auto
          command: build
          args: --release --features pyo3 -o dist
      - name: Upload wheels
        uses: actions/upload-artifact@v4
        with:
          name: wheels-linux-manylinux
          path: dist/*.whl

  # Build for macOS (universal2 for ARM + Intel)
  macos-universal:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v4
      - uses: PyO3/maturin-action@v1
        with:
          command: build
          args: --release --features pyo3 --target universal2-apple-darwin -o dist
      - name: Upload wheels
        uses: actions/upload-artifact@v4
        with:
          name: wheels-macos-universal
          path: dist/*.whl

  test-import:
    needs: [build]
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        python-version: ["3.11"]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: ${{ matrix.python-version }}
      - uses: actions/download-artifact@v4
        with:
          name: wheel-${{ matrix.os }}-py${{ matrix.python-version }}
          path: dist
      - name: Install wheel
        run: pip install dist/*.whl
      - name: Test import
        run: python -c "from md_parser import MarkdownParser; p = MarkdownParser(); print('OK')"
```

#### `.github/workflows/wasm.yml` - WASM Builds

```yaml
name: WASM Build

on:
  push:
    branches: [main]
    tags: ["v*"]
  pull_request:
    branches: [main]

jobs:
  build-wasm:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: wasm32-unknown-unknown
      - uses: Swatinem/rust-cache@v2
      - name: Build WASM (no pyo3)
        run: cargo build --release --target wasm32-unknown-unknown --no-default-features --features serde,frontmatter
      - name: Upload WASM artifact
        uses: actions/upload-artifact@v4
        with:
          name: wasm-build
          path: target/wasm32-unknown-unknown/release/*.wasm

  # Optional: Build with wasm-pack for JS bindings
  build-wasm-pack:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: wasm32-unknown-unknown
      - name: Install wasm-pack
        run: curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
      - name: Build with wasm-pack
        run: wasm-pack build --target web --no-default-features --features serde,frontmatter
      - name: Upload pkg
        uses: actions/upload-artifact@v4
        with:
          name: wasm-pack-pkg
          path: pkg/
```

#### `.github/workflows/release.yml` - GitHub Release Only

```yaml
name: Release

on:
  push:
    tags: ["v*"]

permissions:
  contents: write

jobs:
  # Build wheels for all platforms
  build-wheels:
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            target: x86_64-unknown-linux-gnu
            manylinux: auto
          - os: ubuntu-latest
            target: aarch64-unknown-linux-gnu
            manylinux: auto
          - os: macos-latest
            target: x86_64-apple-darwin
          - os: macos-latest
            target: aarch64-apple-darwin
          - os: windows-latest
            target: x86_64-pc-windows-msvc
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: PyO3/maturin-action@v1
        with:
          target: ${{ matrix.target }}
          manylinux: ${{ matrix.manylinux }}
          command: build
          args: --release --features pyo3 -o dist
      - uses: actions/upload-artifact@v4
        with:
          name: wheels-${{ matrix.target }}
          path: dist/*.whl

  # Build WASM and create GitHub Release
  github-release:
    needs: [build-wheels]
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: wasm32-unknown-unknown
      - name: Build WASM
        run: cargo build --release --target wasm32-unknown-unknown --no-default-features --features serde,frontmatter
      - name: Install wasm-pack
        run: curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
      - name: Build wasm-pack
        run: wasm-pack build --target web --no-default-features --features serde,frontmatter
      - name: Package WASM artifacts
        run: |
          mkdir -p release-artifacts
          cp target/wasm32-unknown-unknown/release/*.wasm release-artifacts/
          tar -czvf release-artifacts/md-parser-wasm-pkg.tar.gz pkg/
      - uses: actions/download-artifact@v4
        with:
          pattern: wheels-*
          merge-multiple: true
          path: release-artifacts/wheels
      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          files: |
            release-artifacts/*.wasm
            release-artifacts/*.tar.gz
            release-artifacts/wheels/*.whl
          generate_release_notes: true
```

### Required Repository Secrets

No secrets required - GitHub Release uses built-in `GITHUB_TOKEN`.

### Public API

```rust
// lib.rs
pub use parser::MarkdownParser;
pub use document::{ParsedDocument, ParsedEdge, EdgeType};
pub use section::{ParsedSection, SectionType};
pub use checklist::{ChecklistItem, ChecklistSummary, extract_checklist_items};
pub use variables::extract_variables;
pub use error::ParseError;

#[cfg(feature = "frontmatter")]
pub use frontmatter::parse_frontmatter;

#[cfg(feature = "pyo3")]
mod python;

#[cfg(feature = "pyo3")]
use pyo3::prelude::*;

#[cfg(feature = "pyo3")]
#[pymodule]
fn md_parser(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<python::PyMarkdownParser>()?;
    m.add_class::<python::PyParsedDocument>()?;
    m.add_class::<python::PyParsedSection>()?;
    m.add_class::<python::PyChecklistItem>()?;
    Ok(())
}
```

### Usage Example (Rust)

```rust
use md_parser::{MarkdownParser, extract_checklist_items, ChecklistSummary};

let content = r#"
# My Document

## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2 (AC: 2, 3)
"#;

// Full document parsing
let parser = MarkdownParser::new();
let doc = parser.parse(content).unwrap();

println!("Title: {:?}", doc.title);
println!("Sections: {}", doc.sections.len());
println!("Variables: {:?}", doc.variables);

// Standalone checklist extraction
let items = extract_checklist_items(content);
let summary = ChecklistSummary::from_items(&items);
println!("Completion: {:.1}%", summary.percentage);
```

### Usage Example (Python)

```python
# Install from GitHub Release:
# pip install https://github.com/fabceolin/md-parser/releases/download/v0.1.0/md_parser-0.1.0-cp311-cp311-manylinux_2_17_x86_64.whl
#
# Or add to requirements.txt:
# md-parser @ https://github.com/fabceolin/md-parser/releases/download/v0.1.0/md_parser-0.1.0-cp311-cp311-manylinux_2_17_x86_64.whl

from md_parser import MarkdownParser, ParsedDocument

content = """
# My Document

## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2 (AC: 2, 3)

Some text with {{variable}} template.
"""

# Parse document
parser = MarkdownParser()
doc = parser.parse(content)

print(f"Title: {doc.title}")
print(f"Sections: {len(doc.sections)}")
print(f"Variables: {doc.variables}")
print(f"Tasks: {len(doc.tasks)}")

# Access checklist items
for task in doc.tasks:
    status = "✓" if task.checked else "○"
    print(f"  {status} {task.text} (indent={task.indent}, AC refs={task.ac_refs})")

# Get completion summary
summary = doc.checklist_summary()
print(f"Completion: {summary.percentage:.1f}%")
```

### Reference Implementation

| Source | Location | Notes |
|--------|----------|-------|
| **agentfs GraphDocs** | `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs` | Original implementation to port |
| **Local scaffold** | `/home/fabricio/src/md-parser/` | Initial structure (Cargo.toml, section.rs, checklist.rs) |

## Dependencies

This story has no dependencies. It is a prerequisite for:
- TEA-RALPHY-001.1 (Python markdown.parse action)
- TEA-RALPHY-001.2 (Rust markdown.parse action)

## Testing

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_parse_headings | Parse H1-H6 correctly | 1 |
| test_extract_title | Title from first H1 | 2 |
| test_section_ids | UUIDs generated | 3 |
| test_order_idx | Sequential ordering | 4 |
| test_edges | Follows relationships | 5 |
| test_checklist_basic | Extract checkboxes | 6 |
| test_checklist_nested | Indent tracking | 7 |
| test_ac_refs | AC reference extraction | 8 |
| test_checklist_summary | Completion percentage | 9 |
| test_variables | `{{var}}` detection | 10, 11 |
| test_frontmatter | YAML header parsing | 12, 13 |
| test_pyo3_parser | Python MarkdownParser class | 14 |
| test_pyo3_dataclasses | Python result access | 15 |
| test_pyo3_wheel | Wheel builds and imports from GitHub Release | 16 |
| test_ci_rust | GitHub Actions Rust CI passes | 17 |
| test_ci_python | GitHub Actions Python wheels build | 18 |
| test_ci_wasm | GitHub Actions WASM builds | 19 |
| test_release | GitHub Release contains wheels + WASM | 20 |
| test_serde | JSON serialization | 21 |
| test_malformed | Graceful error handling | 22 |
| test_wasm_build | Compiles for wasm32 | 24 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-18 | 0.1 | Initial story creation | Sarah (PO) |
| 2025-01-19 | 0.2 | Renamed crate from md-graph-parser to md-parser (no graph features) | Sarah (PO) |
| 2025-01-19 | 0.3 | Added PyO3 Python bindings feature (AC 14-16), maturin build config | Sarah (PO) |
| 2025-01-19 | 0.4 | Added GitHub Actions CI/CD workflows (AC 17-20): Rust CI, Python wheels, WASM, release | Sarah (PO) |
| 2025-01-19 | 0.5 | Removed crates.io/PyPI publishing; GitHub Release only with pre-built wheels | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
