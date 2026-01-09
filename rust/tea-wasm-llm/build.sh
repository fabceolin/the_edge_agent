#!/bin/bash
# TEA WASM LLM Build Script
#
# Builds the WASM package and TypeScript wrapper
#
# Usage:
#   ./build.sh         # Build everything
#   ./build.sh --dev   # Development build (no optimization)
#
set -e

cd "$(dirname "$0")"

# Parse arguments
DEV_MODE=""
if [[ "$1" == "--dev" ]]; then
    DEV_MODE="true"
    echo "🔧 Development mode enabled"
fi

echo "📦 Building TEA WASM LLM..."
echo ""

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

# Step 2: Check if TypeScript compiler is available
if command -v npx &> /dev/null; then
    echo "📝 Compiling TypeScript wrapper..."

    # Create a minimal tsconfig if it doesn't exist
    if [[ ! -f "tsconfig.json" ]]; then
        cat > tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ESNext",
    "module": "ESNext",
    "moduleResolution": "node",
    "declaration": true,
    "declarationMap": true,
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "outDir": "./pkg"
  },
  "include": ["js/**/*.ts"],
  "exclude": ["node_modules", "pkg"]
}
EOF
        echo "   Created tsconfig.json"
    fi

    # Compile TypeScript
    npx tsc --project tsconfig.json 2>/dev/null || {
        echo "⚠️  TypeScript compilation skipped (tsc not available)"
        echo "   Install TypeScript with: npm install -g typescript"
    }
else
    echo "⚠️  npx not found, skipping TypeScript compilation"
    echo "   The TypeScript wrapper is at js/index.ts"
fi

echo ""
echo "📦 Package ready in pkg/"
echo ""
echo "Files:"
ls -lh pkg/*.js pkg/*.wasm pkg/*.d.ts 2>/dev/null || ls -lh pkg/

echo ""
echo "✨ Done!"
echo ""
echo "Usage in browser:"
echo "  import { initTeaLlm, executeLlmYaml } from './pkg/tea_wasm_llm.js';"
echo "  // or with TypeScript wrapper:"
echo "  import { initTeaLlm, executeLlmYaml } from './js/index.js';"
