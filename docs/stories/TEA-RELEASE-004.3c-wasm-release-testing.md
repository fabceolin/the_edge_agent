# Story TEA-RELEASE-004.3c: WASM LLM Release and Testing

## Status

Draft

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

- [ ] Task 1: Create GitHub Actions release workflow (AC: 1, 2, 3, 4, 5)
  - [ ] Add `build-wasm-llm` job to release.yaml
  - [ ] Install wasm-pack in workflow
  - [ ] Build WASM package with `wasm-pack build --target web --release`
  - [ ] Run model split script to create chunks
  - [ ] Create tarball of pkg/ and js/ directories
  - [ ] Upload all assets with `gh release upload`
  - [ ] Generate SHA256SUMS for WASM assets

- [ ] Task 2: Set up Playwright test infrastructure (AC: 6)
  - [ ] Add Playwright to dev dependencies
  - [ ] Create `tests/e2e/wasm-llm.spec.ts`
  - [ ] Configure test server with COOP/COEP headers
  - [ ] Create test fixtures directory

- [ ] Task 3: Implement browser tests (AC: 7, 8, 9)
  - [ ] Test: Package loads without errors
  - [ ] Test: `initTeaLlm()` completes (with tiny model)
  - [ ] Test: `executeLlmYaml()` returns valid result
  - [ ] Test: Second page load uses cached model
  - [ ] Test: Cache clear triggers re-download

- [ ] Task 4: Create package documentation (AC: 10, 11, 12, 13)
  - [ ] Create `rust/tea-wasm-llm/README.md`
  - [ ] Document installation steps
  - [ ] Document COOP/COEP requirements with nginx/Apache examples
  - [ ] Add size breakdown table
  - [ ] Add troubleshooting section

- [ ] Task 5: Update project documentation (AC: 10)
  - [ ] Update `docs/installation.md` with WASM section
  - [ ] Add link to package README
  - [ ] Add browser deployment guide reference

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

- [ ] GitHub Actions workflow builds and uploads WASM package
- [ ] Model chunks uploaded to GitHub Releases
- [ ] SHA256SUMS generated for all assets
- [ ] Playwright tests pass in CI
- [ ] Package README created with examples
- [ ] COOP/COEP documentation complete
- [ ] Troubleshooting guide covers common issues

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

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Split from TEA-RELEASE-004.3 | Bob (SM Agent) |
| 2026-01-08 | 0.2 | Updated for Phi-4-mini Q3_K_S (1.9GB single file), simplified workflow | Sarah (PO Agent) |
