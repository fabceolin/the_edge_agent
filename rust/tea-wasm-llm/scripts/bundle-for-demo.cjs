#!/usr/bin/env node
/**
 * Bundle TEA WASM LLM for Demo Deployment
 *
 * Creates a browser-ready bundle for the docs/wasm-demo/ directory.
 * This bundles the TypeScript wrapper + @wllama/wllama into a single file.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const DOCS_PKG = path.resolve(ROOT, '../../docs/extra/wasm-demo/pkg');

console.log('📦 Bundling TEA WASM LLM for demo...\n');

// Step 1: Create docs/wasm-demo/pkg directory
console.log(`📁 Creating ${DOCS_PKG}...`);
fs.mkdirSync(DOCS_PKG, { recursive: true });

// Step 2: Bundle with esbuild
console.log('🔧 Bundling with esbuild...');
const esbuildCmd = [
  'npx esbuild',
  'js/index.ts',
  '--bundle',
  '--format=esm',
  '--platform=browser',
  '--target=es2020',
  '--sourcemap',
  `--outfile=${DOCS_PKG}/index.js`,
  // Mark WASM imports as external - we'll copy them separately
  "--external:'../pkg/*'",
].join(' ');

try {
  execSync(esbuildCmd, { cwd: ROOT, stdio: 'inherit' });
} catch (e) {
  console.error('❌ esbuild failed:', e.message);
  process.exit(1);
}

// Step 3: Copy WASM files from pkg/
console.log('\n📋 Copying WASM files...');
const pkgFiles = ['tea_wasm_llm.js', 'tea_wasm_llm_bg.wasm', 'tea_wasm_llm.d.ts'];
const pkgDir = path.join(ROOT, 'pkg');

for (const file of pkgFiles) {
  const src = path.join(pkgDir, file);
  const dst = path.join(DOCS_PKG, file);
  if (fs.existsSync(src)) {
    fs.copyFileSync(src, dst);
    console.log(`  ✓ ${file}`);
  } else {
    console.log(`  ⚠️ ${file} not found (run wasm-pack build first)`);
  }
}

// Step 4: Post-process the bundle to fix WASM import paths
console.log('\n🔧 Fixing import paths in bundle...');
const bundlePath = path.join(DOCS_PKG, 'index.js');
if (fs.existsSync(bundlePath)) {
  let bundleContent = fs.readFileSync(bundlePath, 'utf8');

  // Replace '../pkg/tea_wasm_llm.js' with './tea_wasm_llm.js'
  // This handles the case where esbuild marks it as external
  bundleContent = bundleContent.replace(
    /from\s*["']\.\.\/pkg\/tea_wasm_llm\.js["']/g,
    'from "./tea_wasm_llm.js"'
  );
  bundleContent = bundleContent.replace(
    /import\s*\(["']\.\.\/pkg\/tea_wasm_llm\.js["']\)/g,
    'import("./tea_wasm_llm.js")'
  );

  fs.writeFileSync(bundlePath, bundleContent);
  console.log('  ✓ Fixed import paths');
}

// Step 5: Also fix the source map if it exists
const mapPath = path.join(DOCS_PKG, 'index.js.map');
if (fs.existsSync(mapPath)) {
  console.log('  ✓ Source map preserved');
}

console.log('\n✨ Bundle complete!');
console.log(`   Output: ${DOCS_PKG}/`);
console.log('\nFiles:');
const files = fs.readdirSync(DOCS_PKG);
for (const file of files) {
  const stat = fs.statSync(path.join(DOCS_PKG, file));
  const size = (stat.size / 1024).toFixed(1);
  console.log(`  ${file.padEnd(30)} ${size} KB`);
}

console.log('\n📝 Usage in app.js:');
console.log("  import { initLlm, chat } from './pkg/index.js';");
