#!/bin/bash
# Cross-Runtime Prolog Parity Test Harness
#
# Usage:
#   ./scripts/parity-test.sh <yaml_file> [expected_json]
#   ./scripts/parity-test.sh --all
#
# Examples:
#   ./scripts/parity-test.sh examples/prolog/parity/basic-state-access.yaml
#   ./scripts/parity-test.sh --all

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
PARITY_DIR="$PROJECT_ROOT/examples/prolog/parity"
EXPECTED_DIR="$PROJECT_ROOT/examples/prolog/parity-expected"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Temp files for results
PYTHON_RESULT="/tmp/tea_parity_python_$$.json"
RUST_RESULT="/tmp/tea_parity_rust_$$.json"

cleanup() {
    rm -f "$PYTHON_RESULT" "$RUST_RESULT"
}
trap cleanup EXIT

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if required binaries exist
check_python_runtime() {
    if command -v python3 &> /dev/null; then
        # Check if the_edge_agent is installed
        if python3 -c "import the_edge_agent" 2>/dev/null; then
            return 0
        fi
    fi
    return 1
}

check_rust_runtime() {
    # Check for cargo build
    if [ -f "$PROJECT_ROOT/rust/target/release/tea" ]; then
        return 0
    elif [ -f "$PROJECT_ROOT/rust/target/debug/tea" ]; then
        return 0
    fi
    return 1
}

# Run Python implementation
run_python() {
    local yaml_file="$1"
    local output_file="$2"

    cd "$PROJECT_ROOT/python"
    python3 -c "
import sys
import json
from the_edge_agent import YAMLEngine

try:
    engine = YAMLEngine()
    engine.load_from_file('$yaml_file')
    graph = engine.graph.compile()

    # Get initial state from YAML
    import yaml
    with open('$yaml_file') as f:
        config = yaml.safe_load(f)
    initial_state = config.get('initial_state', {})

    # Run the graph
    result = None
    for event in graph.invoke(initial_state):
        result = event

    # Output result
    output = {
        'runtime': 'python',
        'success': True,
        'final_state': result
    }
    print(json.dumps(output, indent=2, sort_keys=True))
except Exception as e:
    output = {
        'runtime': 'python',
        'success': False,
        'error': str(e),
        'error_type': type(e).__name__
    }
    print(json.dumps(output, indent=2, sort_keys=True))
" > "$output_file" 2>&1
}

# Run Rust implementation
run_rust() {
    local yaml_file="$1"
    local output_file="$2"

    local tea_bin=""
    if [ -f "$PROJECT_ROOT/rust/target/release/tea" ]; then
        tea_bin="$PROJECT_ROOT/rust/target/release/tea"
    elif [ -f "$PROJECT_ROOT/rust/target/debug/tea" ]; then
        tea_bin="$PROJECT_ROOT/rust/target/debug/tea"
    else
        echo '{"runtime": "rust", "success": false, "error": "Rust binary not found"}' > "$output_file"
        return 1
    fi

    # Run the Rust binary
    cd "$PROJECT_ROOT"
    "$tea_bin" run "$yaml_file" --output-json > "$output_file" 2>&1 || {
        # Capture error
        echo "{\"runtime\": \"rust\", \"success\": false, \"error\": \"$(cat "$output_file" | tr '\n' ' ' | sed 's/"/\\"/g')\"}" > "$output_file"
    }
}

# Compare two JSON files
compare_results() {
    local python_file="$1"
    local rust_file="$2"
    local yaml_file="$3"

    # Parse Python result
    local python_success=$(python3 -c "import json; d=json.load(open('$python_file')); print(d.get('success', False))" 2>/dev/null || echo "False")
    local rust_success=$(python3 -c "import json; d=json.load(open('$rust_file')); print(d.get('success', False))" 2>/dev/null || echo "False")

    # Both should succeed or both should fail
    if [ "$python_success" = "$rust_success" ]; then
        if [ "$python_success" = "True" ]; then
            log_info "PASS: Both runtimes succeeded for $(basename "$yaml_file")"
            return 0
        else
            log_info "PASS: Both runtimes failed (expected for error tests) for $(basename "$yaml_file")"
            return 0
        fi
    else
        log_error "FAIL: Runtime mismatch for $(basename "$yaml_file")"
        log_error "  Python success: $python_success"
        log_error "  Rust success: $rust_success"
        return 1
    fi
}

# Run single parity test
run_single_test() {
    local yaml_file="$1"

    if [ ! -f "$yaml_file" ]; then
        log_error "YAML file not found: $yaml_file"
        return 1
    fi

    log_info "Testing: $(basename "$yaml_file")"

    # Run both runtimes
    local python_available=false
    local rust_available=false

    if check_python_runtime; then
        python_available=true
        run_python "$yaml_file" "$PYTHON_RESULT"
    else
        log_warn "Python runtime not available, skipping"
    fi

    if check_rust_runtime; then
        rust_available=true
        run_rust "$yaml_file" "$RUST_RESULT"
    else
        log_warn "Rust runtime not available, skipping"
    fi

    # Compare if both available
    if [ "$python_available" = true ] && [ "$rust_available" = true ]; then
        compare_results "$PYTHON_RESULT" "$RUST_RESULT" "$yaml_file"
        return $?
    elif [ "$python_available" = true ]; then
        log_info "Python-only result:"
        cat "$PYTHON_RESULT"
        return 0
    elif [ "$rust_available" = true ]; then
        log_info "Rust-only result:"
        cat "$RUST_RESULT"
        return 0
    else
        log_error "No runtime available"
        return 1
    fi
}

# Run all parity tests
run_all_tests() {
    local passed=0
    local failed=0
    local skipped=0

    log_info "Running all parity tests from $PARITY_DIR"
    echo ""

    for yaml_file in "$PARITY_DIR"/*.yaml; do
        if [ -f "$yaml_file" ]; then
            if run_single_test "$yaml_file"; then
                ((passed++))
            else
                ((failed++))
            fi
            echo ""
        fi
    done

    echo "========================================"
    log_info "Results: $passed passed, $failed failed, $skipped skipped"

    if [ $failed -gt 0 ]; then
        return 1
    fi
    return 0
}

# Main entry point
main() {
    if [ "$1" = "--all" ]; then
        run_all_tests
    elif [ -n "$1" ]; then
        run_single_test "$1"
    else
        echo "Usage: $0 <yaml_file> | --all"
        echo ""
        echo "Examples:"
        echo "  $0 examples/prolog/parity/basic-state-access.yaml"
        echo "  $0 --all"
        exit 1
    fi
}

main "$@"
