# Story TEA-RELEASE-004.3c: WASM LLM Release and Testing

## Status

Ready for Review

**Validation Notes (2026-01-08):**
- All 13 acceptance criteria have test coverage (100%)
- 28 test scenarios defined across unit, integration, and E2E
- Risk areas identified with mitigations in place
- Dependencies (TEA-RELEASE-004.3a, TEA-RELEASE-004.3b) documented
- Test infrastructure requirements clearly specified

## Story

**As a** maintainer releasing TEA WASM LLM package,
**I want** automated GitHub Release workflow and browser tests,
**So that** releases are consistent and quality is verified before distribution.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-RELEASE-004.3a (Core), TEA-RELEASE-004.3b (Model Bundling)
- Technology: GitHub Actions, Playwright, wasm-pack
- Follows pattern: Existing release workflow in `.github/workflows/release.yaml`
- Touch points: `.github/workflows/`, `docs/`, test infrastructure

**Dependencies:**
- TEA-RELEASE-004.3a (WASM LLM Core Package) - must be completed
- TEA-RELEASE-004.3b (Model Bundling and Caching) - must be completed

## Acceptance Criteria

### Release Requirements

1. **AC-1**: GitHub Actions workflow builds WASM package on release tag
2. **AC-2**: Workflow uploads `tea-wasm-llm-{version}.tar.gz` to GitHub Releases
3. **AC-3**: Workflow uploads `microsoft_Phi-4-mini-instruct-Q3_K_S.gguf` (~1.9GB single file) to GitHub Releases
4. **AC-4**: Workflow uploads `model-manifest.json` to GitHub Releases
5. **AC-5**: SHA256SUMS generated for all release assets

### Testing Requirements

6. **AC-6**: Playwright test loads package in headless browser
7. **AC-7**: Playwright test verifies model initialization
8. **AC-8**: Playwright test executes simple LLM workflow
9. **AC-9**: Playwright test verifies IndexedDB caching

### Documentation Requirements

10. **AC-10**: Package README with usage examples
11. **AC-11**: COOP/COEP header requirements documented
12. **AC-12**: Package size breakdown documented
13. **AC-13**: Troubleshooting guide for common issues

## Tasks / Subtasks

- [x] Task 1: Create GitHub Actions release workflow (AC: 1, 2, 3, 4, 5)
  - [x] Add `build-wasm-llm` job to release.yaml
  - [x] Install wasm-pack in workflow
  - [x] Build WASM package with `wasm-pack build --target web --release`
  - [x] Run model download script (Phi-4-mini Q3_K_S, single file)
  - [x] Create tarball of pkg/ and js/ directories
  - [x] Upload all assets via artifact upload
  - [x] Generate SHA256SUMS for WASM assets

- [x] Task 2: Set up Playwright test infrastructure (AC: 6)
  - [x] Add Playwright to dev dependencies
  - [x] Create `tests/e2e/wasm-llm.spec.ts`
  - [x] Configure test server with COOP/COEP headers
  - [x] Create test fixtures directory

- [x] Task 3: Implement browser tests (AC: 7, 8, 9)
  - [x] Test: Package loads without errors
  - [x] Test: `initTeaLlm()` completes (with mock handler)
  - [x] Test: `executeLlmYaml()` returns valid result
  - [x] Test: Second page load uses cached model
  - [x] Test: Cache clear triggers re-download

- [x] Task 4: Create package documentation (AC: 10, 11, 12, 13)
  - [x] Create `rust/tea-wasm-llm/README.md`
  - [x] Document installation steps
  - [x] Document COOP/COEP requirements with nginx/Apache/Caddy examples
  - [x] Add size breakdown table
  - [x] Add troubleshooting section

- [x] Task 5: Update project documentation (AC: 10)
  - [x] Update `docs/installation.md` with WASM section
  - [x] Add link to package README
  - [x] Add browser deployment guide reference

## Dev Notes

### GitHub Actions Workflow

```yaml
# .github/workflows/release.yaml (addition)

  build-wasm-llm:
    name: Build WASM LLM Package
    runs-on: ubuntu-latest
    if: startsWith(github.ref, 'refs/tags/')
    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: dtolnay/rust-action@stable
        with:
          targets: wasm32-unknown-unknown

      - name: Install wasm-pack
        run: cargo install wasm-pack

      - name: Install Node.js
        uses: actions/setup-node@v4
        with:
          node-version: '20'

      - name: Build WASM package
        run: |
          cd rust/tea-wasm-llm
          wasm-pack build --target web --release

      - name: Compile TypeScript
        run: |
          cd rust/tea-wasm-llm
          npm install typescript
          npx tsc js/index.ts --outDir pkg/ --declaration --module esnext --target esnext

      - name: Download model (single file, no chunking needed)
        run: |
          cd rust/tea-wasm-llm
          ./scripts/download-model.sh  # Downloads Phi-4-mini Q3_K_S (~1.9GB)

      - name: Create release tarball
        run: |
          cd rust/tea-wasm-llm
          tar -czvf tea-wasm-llm-${{ github.ref_name }}.tar.gz pkg/ js/

      - name: Upload WASM package
        run: |
          gh release upload ${{ github.ref_name }} \
            rust/tea-wasm-llm/tea-wasm-llm-${{ github.ref_name }}.tar.gz
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}

      - name: Upload model (single file - fits GitHub 2GB limit)
        run: |
          gh release upload ${{ github.ref_name }} \
            rust/tea-wasm-llm/models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf \
            rust/tea-wasm-llm/models/manifest.json
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}

      - name: Generate SHA256SUMS
        run: |
          cd rust/tea-wasm-llm
          sha256sum tea-wasm-llm-*.tar.gz models/*.gguf models/manifest.json > SHA256SUMS.wasm
          gh release upload ${{ github.ref_name }} SHA256SUMS.wasm
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Playwright Test Configuration

```typescript
// playwright.config.ts
import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: './tests/e2e',
  use: {
    baseURL: 'http://localhost:8080',
  },
  webServer: {
    command: 'node tests/server.js',
    port: 8080,
    reuseExistingServer: !process.env.CI,
  },
});
```

### Test Server with COOP/COEP Headers

```javascript
// tests/server.js
const http = require('http');
const fs = require('fs');
const path = require('path');

const PORT = 8080;
const ROOT = path.join(__dirname, '..');

const server = http.createServer((req, res) => {
  // Required for SharedArrayBuffer (multi-threading)
  res.setHeader('Cross-Origin-Opener-Policy', 'same-origin');
  res.setHeader('Cross-Origin-Embedder-Policy', 'require-corp');

  let filePath = path.join(ROOT, req.url === '/' ? 'tests/test.html' : req.url);

  const ext = path.extname(filePath);
  const contentTypes = {
    '.html': 'text/html',
    '.js': 'application/javascript',
    '.wasm': 'application/wasm',
    '.json': 'application/json',
  };

  fs.readFile(filePath, (err, data) => {
    if (err) {
      res.writeHead(404);
      res.end('Not found');
      return;
    }
    res.setHeader('Content-Type', contentTypes[ext] || 'application/octet-stream');
    res.writeHead(200);
    res.end(data);
  });
});

server.listen(PORT, () => console.log(`Server running on http://localhost:${PORT}`));
```

### Playwright Tests

```typescript
// tests/e2e/wasm-llm.spec.ts
import { test, expect } from '@playwright/test';

test.describe('WASM LLM Package', () => {
  test('should load package without errors', async ({ page }) => {
    await page.goto('/');

    // Wait for WASM to initialize
    const result = await page.evaluate(async () => {
      // @ts-ignore
      return window.teaLlmLoaded;
    });

    expect(result).toBe(true);
  });

  test('should initialize with tiny model', async ({ page }) => {
    await page.goto('/');

    const initResult = await page.evaluate(async () => {
      // @ts-ignore
      return await window.initTeaLlm();
    });

    expect(initResult).toBe('initialized');
  });

  test('should execute simple LLM workflow', async ({ page }) => {
    await page.goto('/');

    const result = await page.evaluate(async () => {
      // @ts-ignore
      const yaml = `
name: test
nodes:
  - name: gen
    action: llm.call
    params:
      prompt: "Say hello"
      max_tokens: 5
edges:
  - from: __start__
    to: gen
  - from: gen
    to: __end__
`;
      // @ts-ignore
      return await window.executeLlmYaml(yaml, {});
    });

    expect(result).toHaveProperty('gen');
  });

  test('should cache model in IndexedDB', async ({ page }) => {
    // First load
    await page.goto('/');
    await page.evaluate(async () => {
      // @ts-ignore
      await window.initTeaLlm();
    });

    // Check cache exists
    const cacheExists = await page.evaluate(async () => {
      const db = await new Promise<IDBDatabase>((resolve, reject) => {
        const req = indexedDB.open('tea-llm-cache');
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
      });
      const tx = db.transaction('models', 'readonly');
      const store = tx.objectStore('models');
      const count = await new Promise<number>((resolve) => {
        const req = store.count();
        req.onsuccess = () => resolve(req.result);
      });
      return count > 0;
    });

    expect(cacheExists).toBe(true);
  });
});
```

### Package README

```markdown
# TEA WASM LLM

Run TEA YAML workflows with local LLM inference in the browser.

## Installation

Download from [GitHub Releases](https://github.com/.../releases):

1. `tea-wasm-llm-{version}.tar.gz` - WASM package
2. `gemma-3n-E4B-it-Q4_K_M.chunk-*` - Model chunks
3. `manifest.json` - Chunk metadata

Extract all files to your web server.

## Server Configuration

**Required HTTP headers for multi-threading:**

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

### Nginx

```nginx
location / {
    add_header Cross-Origin-Opener-Policy same-origin;
    add_header Cross-Origin-Embedder-Policy require-corp;
}
```

### Apache

```apache
Header set Cross-Origin-Opener-Policy "same-origin"
Header set Cross-Origin-Embedder-Policy "require-corp"
```

## Usage

```html
<script type="module">
import { initTeaLlm, executeLlmYaml } from './tea-wasm-llm/index.js';

// Initialize (loads model on first run, caches in IndexedDB)
await initTeaLlm({
  modelBasePath: './models',
  useCache: true,
  onProgress: (loaded, total) => {
    console.log(`Loading: ${(loaded/total*100).toFixed(1)}%`);
  }
});

// Execute workflow
const result = await executeLlmYaml(yaml, { question: "Hello!" });
</script>
```

## Package Size

| Asset | Size |
|-------|------|
| WASM + JS | ~50 MB |
| Model (Phi-4-mini Q3_K_S) | ~1.9 GB |
| **Total** | **~2.0 GB** |

> **Note:** Single file model fits GitHub 2GB limit - no chunking needed!

## Troubleshooting

### "SharedArrayBuffer is not defined"

Your server is missing COOP/COEP headers. See Server Configuration above.

### Model fails to load

1. Verify model file is present (`microsoft_Phi-4-mini-instruct-Q3_K_S.gguf`)
2. Check `manifest.json` matches model file
3. Clear IndexedDB cache: `indexedDB.deleteDatabase('tea-llm-cache')`

### Out of memory

The model requires ~3GB RAM. Try:
- Closing other browser tabs
- Using a device with more memory
```

## Definition of Done

- [x] GitHub Actions workflow builds and uploads WASM package
- [x] Model file uploaded to GitHub Releases (single 1.9GB file)
- [x] SHA256SUMS generated for all assets
- [x] Playwright tests implemented
- [x] Package README created with examples
- [x] COOP/COEP documentation complete
- [x] Troubleshooting guide covers common issues

## Risk and Compatibility Check

**Primary Risk:** Large model download in CI may timeout

**Mitigation:**
- Cache model in CI artifacts
- Use smaller test model for automated tests
- Only download full model on release tags

**Rollback:** Manual release possible without workflow

## Compatibility Verification

- [x] No breaking changes to existing APIs
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: CI build time increase (~10 min)

## QA Notes

**Test Design Assessment Date:** 2026-01-08
**Test Architect:** Quinn

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 28 |
| Unit Tests | 6 (21%) |
| Integration Tests | 12 (43%) |
| E2E Tests | 10 (36%) |
| ACs Covered | 13/13 (100%) |

**Priority Distribution:**
- P0 (Critical): 8 tests
- P1 (High): 12 tests
- P2 (Medium): 6 tests
- P3 (Low): 2 tests

### Risk Areas Identified

| Risk | Impact | Probability | Mitigation Status |
|------|--------|-------------|-------------------|
| CI timeout during model download | High | High | Mitigated - cache model, use small test model |
| GitHub Release upload fails (>2GB) | High | Medium | Mitigated - single 1.9GB file fits limit |
| Browser compatibility (SharedArrayBuffer) | High | Medium | Full coverage - COOP/COEP header tests |
| Model initialization timeout | Medium | Medium | Full coverage - use small test model in CI |
| WASM build failure | High | Low | Full coverage - wasm-pack well-tested |
| IndexedDB quota exceeded | Medium | Low | Partial - documented 3GB requirement |

### Recommended Test Scenarios

**P0 Critical Path (must pass before release):**
1. `4.3c-INT-001` - Workflow triggers on release tag push
2. `4.3c-INT-002` - wasm-pack build succeeds with `--target web --release`
3. `4.3c-INT-004` - Tarball created with correct structure
4. `4.3c-INT-005` - gh release upload succeeds
5. `4.3c-INT-006` - Model file download completes
6. `4.3c-INT-009` - SHA256SUMS contains all assets
7. `4.3c-E2E-001` - Test server starts with COOP/COEP headers
8. `4.3c-E2E-002` - WASM package loads without errors

**E2E Browser Tests (Playwright):**
- Model initialization with progress callback
- LLM workflow execution via `executeLlmYaml()`
- IndexedDB caching verification
- Cache management (clear/re-download)

### Test Infrastructure Required

- Playwright with Chromium
- Node.js HTTP server with COOP/COEP headers
- Small test model (~100MB) for fast CI execution
- Test fixtures: `test-model.gguf`, `test.html`, `server.js`

### Concerns and Blockers

**No blockers identified.**

**Concerns:**
1. **CI Time Budget:** Full model testing should only run on release tags to avoid 10-15 minute CI overhead on every PR
2. **Test Model Availability:** A small (~100MB) quantized test model must be created/sourced for fast CI execution
3. **IndexedDB Quota:** 3GB storage requirement should be prominently documented for end users

### Test Design Reference

Full test design document: `docs/qa/assessments/TEA-RELEASE-004.3c-test-design-20260108.md`

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blocking issues encountered during implementation.

### Completion Notes
1. Added `build-wasm-llm` job to `.github/workflows/release.yaml`
2. Created model download script at `rust/tea-wasm-llm/scripts/download-model.sh`
3. Set up Playwright test infrastructure with COOP/COEP-enabled test server
4. Implemented comprehensive E2E tests in `tests/e2e/wasm-llm.spec.ts`
5. Created detailed README with installation, usage, and troubleshooting
6. Updated `docs/installation.md` with WASM LLM package section

### File List
**New Files:**
- `rust/tea-wasm-llm/scripts/download-model.sh` - Model download script
- `rust/tea-wasm-llm/playwright.config.ts` - Playwright configuration
- `rust/tea-wasm-llm/tests/server.js` - Test server with COOP/COEP headers
- `rust/tea-wasm-llm/tests/e2e/test-page.html` - E2E test page
- `rust/tea-wasm-llm/tests/e2e/wasm-llm.spec.ts` - Playwright E2E tests
- `rust/tea-wasm-llm/tests/fixtures/simple-workflow.yaml` - Test fixture
- `rust/tea-wasm-llm/README.md` - Package documentation

**Modified Files:**
- `.github/workflows/release.yaml` - Added build-wasm-llm job
- `rust/tea-wasm-llm/package.json` - Added Playwright dependency and scripts
- `docs/installation.md` - Added WASM LLM section

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Split from TEA-RELEASE-004.3 | Bob (SM Agent) |
| 2026-01-08 | 0.2 | Updated for Phi-4-mini Q3_K_S (1.9GB single file), simplified workflow | Sarah (PO Agent) |
| 2026-01-08 | 0.3 | Added QA Notes section with test coverage and risk assessment | Quinn (QA Agent) |
| 2026-01-08 | 0.4 | Implemented all tasks: GitHub Actions workflow, Playwright tests, documentation | James (Dev Agent) |
