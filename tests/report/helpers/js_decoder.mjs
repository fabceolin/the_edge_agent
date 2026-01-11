// JavaScript decoder for bug report URLs
// Usage: node js_decoder.mjs <url>
// Requires: npm install pako

import { readFileSync } from 'fs';
import { inflate } from 'pako';

// Base64url decode (URL-safe base64 without padding)
function base64urlDecode(str) {
    // Add padding back
    const padding = 4 - (str.length % 4);
    if (padding !== 4) {
        str += '='.repeat(padding);
    }
    // Convert URL-safe chars back to standard base64
    str = str.replace(/-/g, '+').replace(/_/g, '/');
    // Decode to bytes
    return Buffer.from(str, 'base64');
}

// Decode a bug report URL to JSON
function decodeUrl(url) {
    // Parse URL: .../version/runtime_encoded
    const match = url.match(/\/([^/]+)\/(\w+)_(.+)$/);
    if (!match) {
        throw new Error('Invalid URL format');
    }

    const [, version, runtime, encoded] = match;

    // Base64url decode
    const compressed = base64urlDecode(encoded);

    // Inflate (zlib decompress)
    const jsonBytes = inflate(compressed);

    // Parse JSON
    const jsonStr = new TextDecoder().decode(jsonBytes);
    const report = JSON.parse(jsonStr);

    return {
        version,
        runtime,
        report
    };
}

// Main
const url = process.argv[2];

if (!url) {
    console.error('Usage: node js_decoder.mjs <url>');
    process.exit(1);
}

try {
    const result = decodeUrl(url);
    console.log(JSON.stringify(result, null, 2));
} catch (e) {
    console.error('Error decoding URL:', e.message);
    process.exit(1);
}
