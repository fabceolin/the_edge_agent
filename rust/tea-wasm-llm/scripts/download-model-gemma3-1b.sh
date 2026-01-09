#!/bin/bash
# Download Gemma 3 1B Q8_0 model for TEA WASM LLM
#
# This script downloads the Google Gemma 3 1B Instruct model in Q8_0 quantization
# (~1.07GB) from Hugging Face and generates the manifest file with SHA256 checksum.
#
# Usage:
#   ./download-model-gemma3-1b.sh                    # Download to models/ directory
#   ./download-model-gemma3-1b.sh --output-dir /tmp  # Specify custom output directory
#
# The model is ideal for:
# - Edge devices with limited memory
# - Fast prototyping and demos
# - Safari IndexedDB compatibility (1.07GB < 1GB compressed)

set -e

# Default values
MODEL_URL="https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q8_0.gguf"
MODEL_NAME="gemma-3-1b-it-Q8_0"
MODEL_FILE="${MODEL_NAME}.gguf"
OUTPUT_DIR="$(dirname "$0")/../models"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [--output-dir DIR]"
            echo ""
            echo "Downloads Gemma 3 1B Q8_0 model (~1.07GB) for TEA WASM LLM"
            echo ""
            echo "Options:"
            echo "  --output-dir DIR   Output directory (default: ../models)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Resolve to absolute path
OUTPUT_DIR="$(cd "$OUTPUT_DIR" 2>/dev/null && pwd)" || mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR="$(cd "$OUTPUT_DIR" && pwd)"

echo "======================================"
echo "TEA WASM LLM Model Downloader"
echo "======================================"
echo ""
echo "Model: ${MODEL_NAME}"
echo "Output: ${OUTPUT_DIR}/${MODEL_FILE}"
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Check if model already exists
if [[ -f "${OUTPUT_DIR}/${MODEL_FILE}" ]]; then
    echo "Model already exists: ${OUTPUT_DIR}/${MODEL_FILE}"
    echo "Delete it to re-download."

    # Still generate/update manifest if needed
    EXISTING_SIZE=$(stat -c%s "${OUTPUT_DIR}/${MODEL_FILE}" 2>/dev/null || stat -f%z "${OUTPUT_DIR}/${MODEL_FILE}")
    echo "Existing file size: ${EXISTING_SIZE} bytes"
else
    echo "Downloading model (this may take a while)..."
    echo ""

    # Download with progress using curl
    if command -v curl &> /dev/null; then
        curl -L --progress-bar -o "${OUTPUT_DIR}/${MODEL_FILE}" "$MODEL_URL"
    elif command -v wget &> /dev/null; then
        wget --progress=bar:force -O "${OUTPUT_DIR}/${MODEL_FILE}" "$MODEL_URL"
    else
        echo "Error: Neither curl nor wget found. Please install one of them."
        exit 1
    fi
fi

# Calculate SHA256
echo ""
echo "Calculating SHA256 checksum..."
if command -v sha256sum &> /dev/null; then
    SHA256=$(sha256sum "${OUTPUT_DIR}/${MODEL_FILE}" | cut -d' ' -f1)
elif command -v shasum &> /dev/null; then
    SHA256=$(shasum -a 256 "${OUTPUT_DIR}/${MODEL_FILE}" | cut -d' ' -f1)
else
    echo "Warning: No sha256sum or shasum found, skipping checksum"
    SHA256="CHECKSUM_NOT_AVAILABLE"
fi

# Get file size
if [[ "$(uname)" == "Darwin" ]]; then
    TOTAL_SIZE=$(stat -f%z "${OUTPUT_DIR}/${MODEL_FILE}")
else
    TOTAL_SIZE=$(stat -c%s "${OUTPUT_DIR}/${MODEL_FILE}")
fi

# Generate manifest.json for Gemma 3 1B
MANIFEST_FILE="${OUTPUT_DIR}/manifest-gemma3-1b.json"
echo "Generating manifest: ${MANIFEST_FILE}"

cat > "$MANIFEST_FILE" << EOF
{
  "model": "${MODEL_NAME}",
  "version": "v1",
  "totalSize": ${TOTAL_SIZE},
  "file": "${MODEL_FILE}",
  "sha256": "${SHA256}",
  "description": "Google Gemma 3 1B Instruct quantized to Q8_0 (~1.07GB)",
  "source": "${MODEL_URL}",
  "downloadedAt": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "chatTemplate": "gemma"
}
EOF

echo ""
echo "======================================"
echo "Download Complete!"
echo "======================================"
echo ""
echo "Model file: ${OUTPUT_DIR}/${MODEL_FILE}"
echo "Manifest:   ${MANIFEST_FILE}"
echo "Size:       $(echo "scale=2; ${TOTAL_SIZE}/1024/1024/1024" | bc 2>/dev/null || echo "$((TOTAL_SIZE/1024/1024)) MB") GB"
echo "SHA256:     ${SHA256}"
echo ""
echo "Usage in browser:"
echo ""
echo "  import { initLlm, chat } from 'tea-wasm-llm';"
echo ""
echo "  await initLlm({"
echo "    modelUrl: 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q8_0.gguf',"
echo "    onProgress: (loaded, total) => console.log(\`\${Math.round(loaded/total*100)}%\`),"
echo "  });"
echo ""
echo "  const response = await chat('Hello!');"
echo "  console.log(response.content);"
echo ""
