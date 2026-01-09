#!/bin/bash
# Download Phi-4-mini Q3_K_S model for TEA WASM LLM
#
# This script downloads the Microsoft Phi-4-mini-instruct model in Q3_K_S quantization
# (~1.9GB) from Hugging Face and generates the manifest file with SHA256 checksum.
#
# Usage:
#   ./download-model.sh                    # Download to models/ directory
#   ./download-model.sh --output-dir /tmp  # Specify custom output directory
#
# The model fits within GitHub's 2GB release asset limit without chunking.

set -e

# Default values
MODEL_URL="https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF/resolve/main/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf"
MODEL_NAME="microsoft_Phi-4-mini-instruct-Q3_K_S"
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
            echo "Downloads Phi-4-mini Q3_K_S model (~1.9GB) for TEA WASM LLM"
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

# Generate manifest.json
MANIFEST_FILE="${OUTPUT_DIR}/manifest.json"
echo "Generating manifest: ${MANIFEST_FILE}"

cat > "$MANIFEST_FILE" << EOF
{
  "model": "${MODEL_NAME}",
  "version": "v1",
  "totalSize": ${TOTAL_SIZE},
  "file": "${MODEL_FILE}",
  "sha256": "${SHA256}",
  "description": "Microsoft Phi-4-mini-instruct quantized to Q3_K_S (~1.9GB)",
  "source": "${MODEL_URL}",
  "downloadedAt": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
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
