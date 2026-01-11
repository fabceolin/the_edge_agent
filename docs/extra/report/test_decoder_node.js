#!/usr/bin/env node
/**
 * Node.js test runner for TEA Report Decoder - TEA-REPORT-001c
 *
 * Run with: node test_decoder_node.js
 *
 * This tests the decoder functions in a Node.js environment,
 * verifying parity with the Python and Rust implementations.
 */

const zlib = require('zlib');
const assert = require('assert');

// ============================================================================
// VLQ Implementation (copy from decoder.js for standalone testing)
// ============================================================================

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
    return Buffer.from(result);
}

// ============================================================================
// Base64url Implementation
// ============================================================================

function base64urlDecode(str) {
    // Add padding back if needed
    const padding = 4 - (str.length % 4);
    if (padding !== 4) {
        str += '='.repeat(padding);
    }
    // Convert URL-safe chars back to standard base64
    str = str.replace(/-/g, '+').replace(/_/g, '/');
    return Buffer.from(str, 'base64');
}

function base64urlEncode(buffer) {
    return buffer.toString('base64')
        .replace(/\+/g, '-')
        .replace(/\//g, '_')
        .replace(/=+$/, '');
}

// ============================================================================
// Deflate Implementation
// Note: Python's zlib.compress uses zlib format (with header), not raw deflate
// ============================================================================

function inflate(buffer) {
    // zlib.inflateSync handles zlib format (with header)
    return zlib.inflateSync(buffer).toString('utf-8');
}

function deflate(str) {
    // Use zlib format to match Python's zlib.compress
    // deflateSync with default options produces zlib format
    return zlib.deflateSync(Buffer.from(str, 'utf-8'), { level: 9 });
}

// ============================================================================
// URL Parser
// ============================================================================

function parseReportUrl(urlOrPath) {
    let path = urlOrPath;
    if (urlOrPath.startsWith('http')) {
        const url = new URL(urlOrPath);
        path = url.pathname;
    }

    // Extract version and runtime_encoded from path
    const match = path.match(/\/report\/([^/]+)\/(.+)$/);
    if (!match) {
        throw new Error('Invalid report URL format');
    }

    const version = match[1];
    const runtimeEncoded = match[2];

    // Split runtime_encoded on FIRST underscore only
    // (encoded data may contain underscores from base64url)
    const underscorePos = runtimeEncoded.indexOf('_');
    if (underscorePos < 0) {
        throw new Error('Invalid report URL format: missing runtime prefix');
    }

    const runtime = runtimeEncoded.substring(0, underscorePos);
    const encoded = runtimeEncoded.substring(underscorePos + 1);

    return { version, runtime, encoded };
}

// ============================================================================
// Full Decode Pipeline
// ============================================================================

function decodeReport(urlOrPath) {
    const { version, runtime, encoded } = parseReportUrl(urlOrPath);
    const compressed = base64urlDecode(encoded);
    const json = inflate(compressed);
    const report = JSON.parse(json);

    if (report.version !== version) {
        console.warn(`Version mismatch: URL=${version}, report=${report.version}`);
    }

    return report;
}

// ============================================================================
// Test Framework
// ============================================================================

let passed = 0;
let failed = 0;

function test(name, fn) {
    try {
        fn();
        console.log(`  \x1b[32m✓\x1b[0m ${name}`);
        passed++;
    } catch (e) {
        console.log(`  \x1b[31m✗\x1b[0m ${name}`);
        console.log(`    \x1b[31m${e.message}\x1b[0m`);
        failed++;
    }
}

function assertEqual(actual, expected, msg = '') {
    const actualStr = JSON.stringify(actual);
    const expectedStr = JSON.stringify(expected);
    if (actualStr !== expectedStr) {
        throw new Error(`${msg}\nExpected: ${expectedStr}\nActual: ${actualStr}`);
    }
}

function assertTrue(value, msg = '') {
    if (!value) {
        throw new Error(msg || 'Expected true');
    }
}

// ============================================================================
// Tests
// ============================================================================

console.log('\n\x1b[1mVLQ Tests\x1b[0m');

test('VLQ decode zero', () => {
    const result = vlqDecode(Buffer.from([0]));
    assertEqual(result.value, 0);
    assertEqual(result.consumed, 1);
});

test('VLQ decode single byte max (127)', () => {
    const result = vlqDecode(Buffer.from([127]));
    assertEqual(result.value, 127);
});

test('VLQ decode two byte min (128)', () => {
    const result = vlqDecode(Buffer.from([128, 1]));
    assertEqual(result.value, 128);
});

test('VLQ decode two byte (255)', () => {
    const result = vlqDecode(Buffer.from([255, 1]));
    assertEqual(result.value, 255);
});

test('VLQ decode two byte max (16383)', () => {
    const result = vlqDecode(Buffer.from([255, 127]));
    assertEqual(result.value, 16383);
});

test('VLQ decode three byte (16384)', () => {
    const result = vlqDecode(Buffer.from([128, 128, 1]));
    assertEqual(result.value, 16384);
});

test('VLQ matches Rust/Python test vectors', () => {
    const testCases = [
        [0, [0]],
        [1, [1]],
        [127, [127]],
        [128, [128, 1]],
        [255, [255, 1]],
        [256, [128, 2]],
        [16383, [255, 127]],
        [16384, [128, 128, 1]],
        [2097151, [255, 255, 127]],
        [2097152, [128, 128, 128, 1]],
    ];
    for (const [value, expectedBytes] of testCases) {
        const encoded = vlqEncode(value);
        assertEqual(Array.from(encoded), expectedBytes, `VLQ encode ${value}`);
    }
});

console.log('\n\x1b[1mBase64url Tests\x1b[0m');

test('Base64url decode simple', () => {
    const result = base64urlDecode('SGVsbG8');
    assertEqual(result.toString(), 'Hello');
});

test('Base64url no plus or slash', () => {
    const data = Buffer.from([0xfb, 0xef, 0xbe]);
    const encoded = base64urlEncode(data);
    assertTrue(!encoded.includes('+'), 'Should not contain +');
    assertTrue(!encoded.includes('/'), 'Should not contain /');
});

test('Base64url no padding', () => {
    const data = Buffer.from('a');
    const encoded = base64urlEncode(data);
    assertTrue(!encoded.includes('='), 'Should not contain padding');
});

test('Base64url roundtrip', () => {
    const testData = ['Hello', 'Hello, world!', '', 'Test123'];
    for (const str of testData) {
        const data = Buffer.from(str);
        const encoded = base64urlEncode(data);
        const decoded = base64urlDecode(encoded);
        assertEqual(decoded.toString(), str);
    }
});

console.log('\n\x1b[1mDeflate Tests\x1b[0m');

test('Deflate roundtrip', () => {
    const testData = ['Hello, world!', 'Test', '{"key": "value"}'];
    for (const str of testData) {
        const compressed = deflate(str);
        const decompressed = inflate(compressed);
        assertEqual(decompressed, str);
    }
});

console.log('\n\x1b[1mURL Parser Tests\x1b[0m');

test('parseReportUrl extracts components', () => {
    const url = '/the_edge_agent/report/0.9.34/rust_abc123';
    const result = parseReportUrl(url);
    assertEqual(result.version, '0.9.34');
    assertEqual(result.runtime, 'rust');
    assertEqual(result.encoded, 'abc123');
});

test('parseReportUrl handles full URL', () => {
    const url = 'https://fabceolin.github.io/the_edge_agent/report/1.0.0/python_xyz789';
    const result = parseReportUrl(url);
    assertEqual(result.version, '1.0.0');
    assertEqual(result.runtime, 'python');
});

console.log('\n\x1b[1mFull Decode Pipeline Tests\x1b[0m');

test('decodeReport full pipeline', () => {
    const reportJson = '{"version":"0.9.34","platform":"linux-x86_64","runtime":"python","error_type":"Panic","message":"Test error","stack":[]}';
    const compressed = deflate(reportJson);
    const encoded = base64urlEncode(compressed);
    const url = `/report/0.9.34/python_${encoded}`;

    const report = decodeReport(url);
    assertEqual(report.version, '0.9.34');
    assertEqual(report.platform, 'linux-x86_64');
    assertEqual(report.runtime, 'python');
    assertEqual(report.error_type, 'Panic');
    assertEqual(report.message, 'Test error');
});

test('decodeReport with stack frames', () => {
    const reportJson = JSON.stringify({
        version: '0.9.34',
        platform: 'linux-x86_64',
        runtime: 'rust',
        error_type: 'YamlError',
        message: 'YAML parse failed',
        stack: [
            { addr: 12345, symbol: 'main', file: 'src/main.rs', line: 42 },
            { addr: 67890, symbol: 'process' }
        ]
    });

    const compressed = deflate(reportJson);
    const encoded = base64urlEncode(compressed);
    const url = `/report/0.9.34/rust_${encoded}`;

    const report = decodeReport(url);
    assertEqual(report.stack.length, 2);
    assertEqual(report.stack[0].symbol, 'main');
    assertEqual(report.stack[0].line, 42);
});

test('decodeReport with context', () => {
    const reportJson = JSON.stringify({
        version: '0.9.34',
        platform: 'linux-x86_64',
        runtime: 'python',
        error_type: 'ActionError',
        message: 'Action failed',
        stack: [],
        context: {
            node_name: 'process_data',
            action_type: 'http.get'
        }
    });

    const compressed = deflate(reportJson);
    const encoded = base64urlEncode(compressed);
    const url = `/report/0.9.34/python_${encoded}`;

    const report = decodeReport(url);
    assertTrue(report.context !== undefined, 'Should have context');
    assertEqual(report.context.node_name, 'process_data');
});

console.log('\n\x1b[1mCross-Runtime Parity Tests\x1b[0m');

test('decode Python-generated URL', () => {
    // This URL was generated by Python encoder
    const pythonUrl = 'https://example.github.io/the_edge_agent/report/0.9.34/python_eNpVkM1uwkAMhF9l5XNIA6Qp5BF6qkRvVRWZzQIrsj_yGpQoyrvXi4REr5894xnPcDeUbPDQQlXuy20NBcQB-RTICRusv42rcdd0TZ7QzbN1RgZx4ouoCjBEgTqeYqZf6K0W6ExKeM7k2yRWjx0lluoT73jQZCOr3ujQG1IRyfIkosSor9D-zIB9T9CuN9v6XfDkjmEQK4c2HzzZIRsn0m-ZlJQESk6B9WYpnurmY7evXtSRgpZU_w0Ge3zVr6tq-S1AB89mZGhn8BKx8_iozFKly0D2UbM87Vn7whzLs2FYlj94e26j';

    const report = decodeReport(pythonUrl);
    assertEqual(report.version, '0.9.34');
    assertEqual(report.platform, 'linux-x86_64');
    assertEqual(report.runtime, 'python');
    assertEqual(report.error_type, 'Panic');
    assertEqual(report.message, 'Test error for JavaScript decoder parity');
    assertEqual(report.stack.length, 2);
    assertEqual(report.stack[0].symbol, 'main');
    assertEqual(report.stack[0].line, 42);
    assertTrue(report.context !== undefined);
    assertEqual(report.context.node_name, 'test_node');
});

// ============================================================================
// Summary
// ============================================================================

console.log('\n' + '='.repeat(50));
if (failed === 0) {
    console.log(`\x1b[32m✓ All ${passed} tests passed!\x1b[0m`);
    process.exit(0);
} else {
    console.log(`\x1b[31m✗ ${failed} tests failed, ${passed} passed\x1b[0m`);
    process.exit(1);
}
