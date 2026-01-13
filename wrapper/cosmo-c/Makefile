# Makefile for TEA Wrapper with Cosmopolitan Libc
#
# TEA-DIST-001: Docker Distribution with Cross-Platform Wrapper
#
# This builds an Actually Portable Executable (APE) that runs on
# Linux, macOS, Windows, FreeBSD, NetBSD, and OpenBSD.

COSMO_VERSION = 4.0.2
COSMO_URL = https://cosmo.zip/pub/cosmocc/cosmocc-$(COSMO_VERSION).zip

# Output binary name
OUTPUT = tea.com

# Compiler flags
CFLAGS = -Os -Wall -Wextra -Werror -std=c11

# Source files
SRCS = tea.c

.PHONY: all clean download test help

all: $(OUTPUT)

help:
	@echo "TEA Wrapper Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build tea.com (default)"
	@echo "  download  - Download Cosmopolitan toolchain"
	@echo "  clean     - Remove build artifacts"
	@echo "  test      - Run basic tests"
	@echo "  help      - Show this message"
	@echo ""
	@echo "Requirements:"
	@echo "  - curl (for downloading cosmocc)"
	@echo "  - unzip"
	@echo ""
	@echo "Usage:"
	@echo "  make download  # First time setup"
	@echo "  make           # Build tea.com"

# Download Cosmopolitan compiler
cosmocc.zip:
	@echo "Downloading Cosmopolitan Libc $(COSMO_VERSION)..."
	curl -fsSL -o $@ $(COSMO_URL)

cosmocc: cosmocc.zip
	@echo "Extracting Cosmopolitan toolchain..."
	unzip -oq $<
	touch $@

# Build the APE binary
$(OUTPUT): $(SRCS) cosmocc
	@echo "Building $(OUTPUT)..."
	./cosmocc/bin/cosmocc $(CFLAGS) -o $@ $(SRCS)
	@echo ""
	@echo "Build complete: $(OUTPUT)"
	@echo "Binary size: $$(du -h $(OUTPUT) | cut -f1)"
	@echo ""
	@echo "This binary runs on Linux, macOS, Windows, and BSDs."
	@echo "On Windows, rename to tea.exe for easier use."

# Run basic tests
test: $(OUTPUT)
	@echo "Running tests..."
	@echo ""
	@echo "Test 1: Wrapper version"
	./$(OUTPUT) --wrapper-version
	@echo ""
	@echo "Test 2: Docker image name"
	./$(OUTPUT) --docker-image
	@echo ""
	@echo "Test 3: Wrapper help"
	./$(OUTPUT) --wrapper-help | head -5
	@echo ""
	@echo "All basic tests passed!"

# Clean build artifacts
clean:
	rm -f $(OUTPUT)
	rm -rf cosmocc cosmocc.zip

# Deep clean (remove downloaded files too)
distclean: clean
	rm -f cosmocc.zip
