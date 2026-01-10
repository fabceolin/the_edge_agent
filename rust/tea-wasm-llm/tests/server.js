/**
 * Test server with COOP/COEP headers for TEA WASM LLM
 *
 * This server provides the required Cross-Origin headers for SharedArrayBuffer
 * support, which is needed for multi-threaded WASM execution.
 *
 * Required headers:
 * - Cross-Origin-Opener-Policy: same-origin
 * - Cross-Origin-Embedder-Policy: require-corp
 *
 * Usage:
 *   node tests/server.js
 *   # Server runs on http://localhost:8080
 */

import http from 'http';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PORT = process.env.PORT || 8080;
const ROOT = path.join(__dirname, '..');

// MIME types for served files
const CONTENT_TYPES = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'application/javascript; charset=utf-8',
  '.mjs': 'application/javascript; charset=utf-8',
  '.ts': 'application/typescript; charset=utf-8',
  '.wasm': 'application/wasm',
  '.json': 'application/json; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.ico': 'image/x-icon',
  '.gguf': 'application/octet-stream',
};

/**
 * Get content type for a file extension
 */
function getContentType(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  return CONTENT_TYPES[ext] || 'application/octet-stream';
}

/**
 * Resolve file path from URL
 */
function resolveFilePath(url) {
  // Remove query string and hash
  const cleanUrl = url.split('?')[0].split('#')[0];

  // Default to test.html for root
  if (cleanUrl === '/') {
    return path.join(ROOT, 'tests', 'test.html');
  }

  // Resolve relative to ROOT
  return path.join(ROOT, cleanUrl);
}

/**
 * Handle HTTP requests
 */
function handleRequest(req, res) {
  const filePath = resolveFilePath(req.url);

  // Security: prevent directory traversal
  if (!filePath.startsWith(ROOT)) {
    res.writeHead(403);
    res.end('Forbidden');
    return;
  }

  // Set COOP/COEP headers for SharedArrayBuffer support
  res.setHeader('Cross-Origin-Opener-Policy', 'same-origin');
  res.setHeader('Cross-Origin-Embedder-Policy', 'require-corp');

  // Set CORS headers for cross-origin resource access
  res.setHeader('Cross-Origin-Resource-Policy', 'cross-origin');
  res.setHeader('Access-Control-Allow-Origin', '*');

  // Handle file stats first for large files (streaming)
  fs.stat(filePath, (err, stats) => {
    if (err) {
      if (err.code === 'ENOENT') {
        res.writeHead(404);
        res.end(`Not found: ${req.url}`);
      } else {
        res.writeHead(500);
        res.end(`Server error: ${err.message}`);
      }
      return;
    }

    if (stats.isDirectory()) {
      // Try index.html for directories
      const indexPath = path.join(filePath, 'index.html');
      if (fs.existsSync(indexPath)) {
        serveFile(indexPath, res);
      } else {
        res.writeHead(403);
        res.end('Directory listing not allowed');
      }
      return;
    }

    serveFile(filePath, res, stats.size);
  });
}

/**
 * Serve a file with proper headers
 */
function serveFile(filePath, res, fileSize = null) {
  const contentType = getContentType(filePath);

  // Set content type and length
  res.setHeader('Content-Type', contentType);
  if (fileSize !== null) {
    res.setHeader('Content-Length', fileSize);
  }

  // Enable caching for static assets
  if (filePath.endsWith('.wasm') || filePath.endsWith('.gguf')) {
    res.setHeader('Cache-Control', 'public, max-age=86400'); // 1 day
  }

  // Stream the file
  const stream = fs.createReadStream(filePath);
  stream.on('error', (err) => {
    res.writeHead(500);
    res.end(`Error reading file: ${err.message}`);
  });

  res.writeHead(200);
  stream.pipe(res);
}

// Create and start server
const server = http.createServer(handleRequest);

server.listen(PORT, () => {
  console.log(`
====================================
TEA WASM LLM Test Server
====================================

Server running at: http://localhost:${PORT}

COOP/COEP headers enabled for SharedArrayBuffer support.

Serving files from: ${ROOT}

Routes:
  /                  -> tests/test.html
  /tests/e2e/*.html  -> E2E test pages
  /pkg/*             -> WASM package files
  /models/*          -> Model files

Press Ctrl+C to stop.
====================================
`);
});

// Handle graceful shutdown
process.on('SIGINT', () => {
  console.log('\nShutting down server...');
  server.close(() => {
    console.log('Server stopped.');
    process.exit(0);
  });
});

export default server;
