/**
 * TEA Report Decoder - TEA-REPORT-001c
 *
 * JavaScript implementation of the URL decoder for the TEA Bug Report viewer.
 * Designed to have parity with the Python and Rust encoder implementations.
 *
 * Decoding pipeline:
 * 1. Parse URL to extract version, runtime, encoded data
 * 2. Base64url decode
 * 3. Inflate (decompress)
 * 4. JSON parse
 */

// ============================================================================
// Base64url Decoder
// ============================================================================

/**
 * Decode Base64url to bytes.
 *
 * Handles:
 * - Missing padding (adds it back)
 * - URL-safe alphabet (- -> +, _ -> /)
 *
 * @param {string} str - Base64url encoded string
 * @returns {Uint8Array} Decoded bytes
 * @throws {Error} If decoding fails
 */
function base64urlDecode(str) {
    // Add padding back if needed
    const padding = 4 - (str.length % 4);
    if (padding !== 4) {
        str += '='.repeat(padding);
    }
    // Convert URL-safe chars back to standard base64
    str = str.replace(/-/g, '+').replace(/_/g, '/');

    // Decode using built-in atob
    try {
        const binary = atob(str);
        const bytes = new Uint8Array(binary.length);
        for (let i = 0; i < binary.length; i++) {
            bytes[i] = binary.charCodeAt(i);
        }
        return bytes;
    } catch (e) {
        throw new Error(`Base64url decode failed: ${e.message}`);
    }
}

/**
 * Encode bytes to Base64url (for testing/validation).
 *
 * @param {Uint8Array} bytes - Bytes to encode
 * @returns {string} Base64url encoded string (no padding)
 */
function base64urlEncode(bytes) {
    let binary = '';
    for (let i = 0; i < bytes.length; i++) {
        binary += String.fromCharCode(bytes[i]);
    }
    // Encode to base64, then convert to URL-safe alphabet
    return btoa(binary)
        .replace(/\+/g, '-')
        .replace(/\//g, '_')
        .replace(/=+$/, ''); // Strip padding
}

// ============================================================================
// Inflate Decompression
// ============================================================================

/**
 * Inflate (decompress) bytes using pako.
 *
 * Handles zlib-compressed data from Python's zlib.compress() or
 * Rust's flate2::ZlibEncoder.
 *
 * @param {Uint8Array} bytes - Compressed bytes
 * @returns {string} Decompressed string (UTF-8)
 * @throws {Error} If decompression fails
 */
function inflate(bytes) {
    if (typeof pako === 'undefined') {
        throw new Error('pako library not loaded');
    }
    try {
        return pako.inflate(bytes, { to: 'string' });
    } catch (e) {
        throw new Error(`Inflate failed: ${e.message}`);
    }
}

// ============================================================================
// VLQ Decoder
// ============================================================================

/**
 * VLQ decode a single number from bytes.
 *
 * Variable-Length Quantity encoding uses 7 bits per byte,
 * with the high bit indicating whether more bytes follow.
 *
 * Matches the Rust and Python implementations exactly.
 *
 * @param {Uint8Array} bytes - VLQ encoded bytes
 * @param {number} [offset=0] - Starting offset in bytes
 * @returns {{value: number, consumed: number}} Decoded value and bytes consumed
 *
 * @example
 * vlqDecode(new Uint8Array([0])) // { value: 0, consumed: 1 }
 * vlqDecode(new Uint8Array([127])) // { value: 127, consumed: 1 }
 * vlqDecode(new Uint8Array([128, 1])) // { value: 128, consumed: 2 }
 * vlqDecode(new Uint8Array([255, 127])) // { value: 16383, consumed: 2 }
 */
function vlqDecode(bytes, offset = 0) {
    let value = 0;
    let shift = 0;
    let consumed = 0;

    for (let i = offset; i < bytes.length; i++) {
        const byte = bytes[i];
        consumed++;
        value |= (byte & 0x7F) << shift;
        if ((byte & 0x80) === 0) {
            break;
        }
        shift += 7;
    }

    return { value, consumed };
}

/**
 * VLQ encode a number to bytes (for testing/validation).
 *
 * @param {number} value - Non-negative integer to encode
 * @returns {Uint8Array} VLQ encoded bytes
 */
function vlqEncode(value) {
    const result = [];
    do {
        let byte = value & 0x7F;
        value >>>= 7;
        if (value !== 0) {
            byte |= 0x80;
        }
        result.push(byte);
    } while (value !== 0);
    return new Uint8Array(result);
}

// ============================================================================
// URL Parser
// ============================================================================

/**
 * Parse report URL and extract components.
 *
 * Expected URL format:
 * https://{org}.github.io/the_edge_agent/report/{version}/{runtime}_{encoded}
 *
 * Also supports path-only format for local testing:
 * /the_edge_agent/report/{version}/{runtime}_{encoded}
 *
 * @param {string} urlOrPath - Full URL or path
 * @returns {{version: string, runtime: string, encoded: string}} Extracted components
 * @throws {Error} If URL format is invalid
 */
function parseReportUrl(urlOrPath) {
    // Handle both full URLs and path-only
    let path = urlOrPath;
    if (urlOrPath.startsWith('http')) {
        try {
            const url = new URL(urlOrPath);
            path = url.pathname;
        } catch (e) {
            throw new Error('Invalid URL format');
        }
    }

    // Extract version and runtime_encoded from path
    // Format: /report/{version}/{runtime}_{encoded}
    const match = path.match(/\/report\/([^/]+)\/(.+)$/);
    if (!match) {
        throw new Error('Invalid report URL format: expected /report/{version}/{runtime}_{encoded}');
    }

    const version = match[1];
    const runtimeEncoded = match[2];

    // Split runtime_encoded on FIRST underscore only
    // (encoded data may contain underscores from base64url)
    const underscorePos = runtimeEncoded.indexOf('_');
    if (underscorePos < 0) {
        throw new Error('Invalid report URL format: missing runtime prefix (no underscore)');
    }

    const runtime = runtimeEncoded.substring(0, underscorePos);
    const encoded = runtimeEncoded.substring(underscorePos + 1);

    // Validate runtime is alphanumeric (no underscores allowed in runtime)
    if (!/^[a-zA-Z0-9]+$/.test(runtime)) {
        throw new Error(`Invalid runtime: ${runtime}`);
    }

    return { version, runtime, encoded };
}

// ============================================================================
// Full Decode Pipeline
// ============================================================================

/**
 * Full decode pipeline: URL -> Report object.
 *
 * Pipeline:
 * 1. Parse URL to extract version, runtime, encoded data
 * 2. Base64url decode the encoded data
 * 3. Inflate (decompress) the bytes
 * 4. JSON parse to get report object
 *
 * @param {string} urlOrPath - Full URL or path containing encoded report
 * @returns {Object} Decoded report object with fields:
 *   - version: string
 *   - platform: string
 *   - runtime: string
 *   - error_type: string ("Panic", "YamlError", "ExecutorError", "ActionError")
 *   - message: string
 *   - stack: Array<{addr: number, symbol?: string, file?: string, line?: number}>
 *   - context?: {node_name?: string, action_type?: string, checkpoint_id?: string}
 *   - extended?: {workflow_name?: string, nodes: Array, edges: Array, ...}
 * @throws {Error} If any step of decoding fails
 */
function decodeReport(urlOrPath) {
    // Step 1: Parse URL
    const { version, runtime, encoded } = parseReportUrl(urlOrPath);

    // Step 2: Base64url decode
    const compressed = base64urlDecode(encoded);

    // Step 3: Inflate
    const json = inflate(compressed);

    // Step 4: Parse JSON
    let report;
    try {
        report = JSON.parse(json);
    } catch (e) {
        throw new Error(`JSON parse failed: ${e.message}`);
    }

    // Verify version matches (optional warning)
    if (report.version !== version) {
        console.warn(`Version mismatch: URL=${version}, report=${report.version}`);
    }

    return report;
}

// ============================================================================
// Error Types
// ============================================================================

/**
 * Error type descriptions for display
 */
const ERROR_TYPE_LABELS = {
    'Panic': 'Panic',
    'YamlError': 'YAML Error',
    'ExecutorError': 'Executor Error',
    'ActionError': 'Action Error'
};

/**
 * Get a human-readable label for an error type.
 *
 * @param {string} errorType - Error type from report
 * @returns {string} Human-readable label
 */
function getErrorTypeLabel(errorType) {
    return ERROR_TYPE_LABELS[errorType] || errorType;
}

// ============================================================================
// Exports (for module systems)
// ============================================================================

// For use in browsers that support ES modules
if (typeof window !== 'undefined') {
    window.TEADecoder = {
        base64urlDecode,
        base64urlEncode,
        inflate,
        vlqDecode,
        vlqEncode,
        parseReportUrl,
        decodeReport,
        getErrorTypeLabel,
        ERROR_TYPE_LABELS
    };
}

// For use in Node.js (testing)
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        base64urlDecode,
        base64urlEncode,
        inflate,
        vlqDecode,
        vlqEncode,
        parseReportUrl,
        decodeReport,
        getErrorTypeLabel,
        ERROR_TYPE_LABELS
    };
}
