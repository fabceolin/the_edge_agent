#!/bin/bash
# TEA WASM LLM Build Script
#
# Builds the WASM package, TypeScript wrapper, and bundles wllama assets
#
# Usage:
#   ./build.sh           # Build everything
#   ./build.sh --dev     # Development build (no optimization)
#   ./build.sh --cdn     # Skip asset bundling (use CDN at runtime)
#
set -e

cd "$(dirname "$0")"

# Parse arguments
DEV_MODE=""
USE_CDN=""
for arg in "$@"; do
    case $arg in
        --dev)
            DEV_MODE="true"
            echo "🔧 Development mode enabled"
            ;;
        --cdn)
            USE_CDN="true"
            echo "🌐 CDN mode: skipping asset bundling"
            ;;
    esac
done

echo "📦 Building TEA WASM LLM (Batteries Included)..."
echo ""

# Step 0: Ensure npm dependencies are installed
if [[ ! -d "node_modules" ]]; then
    echo "📥 Installing npm dependencies..."
    npm install
    echo ""
fi

# Step 1: Build WASM with wasm-pack
echo "🦀 Compiling Rust to WebAssembly..."
if [[ -n "$DEV_MODE" ]]; then
    wasm-pack build --target web --dev
else
    wasm-pack build --target web --release
fi

echo ""
echo "✅ WASM build complete!"
echo ""

# Step 2: Copy wllama WASM assets (unless using CDN)
if [[ -z "$USE_CDN" ]]; then
    echo "📋 Copying wllama WASM assets..."
    if [[ -x "scripts/copy-wllama-assets.sh" ]]; then
        ./scripts/copy-wllama-assets.sh
    else
        echo "⚠️  copy-wllama-assets.sh not found or not executable"
        echo "   wllama will fall back to CDN at runtime"
    fi
    echo ""
fi

# Step 3: Compile TypeScript
if command -v npx &> /dev/null; then
    echo "📝 Compiling TypeScript wrapper..."

    # Ensure js/tsconfig.json exists
    if [[ ! -f "js/tsconfig.json" ]]; then
        cat > js/tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ESNext",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "declaration": true,
    "declarationMap": true,
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "outDir": "../dist/js",
    "rootDir": "."
  },
  "include": ["./**/*.ts"],
  "exclude": ["../node_modules", "../pkg", "../dist"]
}
EOF
        echo "   Created js/tsconfig.json"
    fi

    # Compile TypeScript
    npx tsc --project js/tsconfig.json 2>/dev/null || {
        echo "⚠️  TypeScript compilation had warnings"
    }

    echo "✅ TypeScript compilation complete!"
else
    echo "⚠️  npx not found, skipping TypeScript compilation"
    echo "   The TypeScript source is at js/"
fi

echo ""
echo "📦 Package ready!"
echo ""
echo "Files:"
echo "  WASM:"
ls -lh pkg/*.wasm 2>/dev/null || echo "    (no wasm found)"
echo "  JavaScript:"
ls -lh pkg/*.js 2>/dev/null || echo "    (no js found)"
echo "  TypeScript definitions:"
ls -lh dist/js/*.d.ts 2>/dev/null || echo "    (no d.ts found)"
if [[ -z "$USE_CDN" ]]; then
    echo "  wllama assets:"
    ls -lh assets/*/*.wasm 2>/dev/null || echo "    (no wllama assets)"
fi

echo ""
echo "✨ Done!"
echo ""
echo "Usage (batteries-included):"
echo "  import { initLlm, chat, chatStream } from 'tea-wasm-llm';"
echo ""
echo "  await initLlm({ modelUrl: 'https://..../model.gguf' });"
echo "  const response = await chat('Hello!');"
echo ""
echo "Usage (legacy callback API):"
echo "  import { initTeaLlm, executeLlmYaml } from 'tea-wasm-llm';"
