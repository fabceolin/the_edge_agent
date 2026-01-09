/**
 * Cross-Origin Isolation Service Worker
 *
 * This service worker adds COOP/COEP headers for multi-threading support
 * on hosting platforms like GitHub Pages that don't allow custom headers.
 *
 * Source: https://github.com/nicepkg/vite-plugin-cross-origin-isolation
 * License: MIT
 */

/* eslint-disable no-restricted-globals */

const ENABLE_LOGGING = false;

function log(...args) {
  if (ENABLE_LOGGING) {
    console.log('[COI-SW]', ...args);
  }
}

// Check if already cross-origin isolated
if (typeof window !== 'undefined') {
  if (window.crossOriginIsolated) {
    log('Already cross-origin isolated, skipping service worker registration');
  } else if (!window.isSecureContext) {
    log('Not a secure context, cannot register service worker');
  } else {
    // Register service worker
    navigator.serviceWorker
      .register(window.document.currentScript.src)
      .then((registration) => {
        log('Service worker registered:', registration.scope);

        registration.addEventListener('updatefound', () => {
          log('Service worker update found');
          const newWorker = registration.installing;
          newWorker.addEventListener('statechange', () => {
            if (newWorker.state === 'activated') {
              log('New service worker activated, reloading...');
              window.location.reload();
            }
          });
        });

        // If this is the first time and service worker is active, reload
        if (registration.active && !navigator.serviceWorker.controller) {
          log('Service worker active but no controller, reloading...');
          window.location.reload();
        }
      })
      .catch((error) => {
        console.error('[COI-SW] Registration failed:', error);
      });
  }
}

// Service worker logic
self.addEventListener('install', () => {
  log('Service worker installing...');
  self.skipWaiting();
});

self.addEventListener('activate', (event) => {
  log('Service worker activating...');
  event.waitUntil(self.clients.claim());
});

self.addEventListener('fetch', (event) => {
  const request = event.request;

  // Only handle GET requests for same-origin resources
  if (request.method !== 'GET') {
    return;
  }

  // Check if this is a navigation request or resource request
  const url = new URL(request.url);
  const isSameOrigin = url.origin === self.location.origin;

  event.respondWith(
    (async () => {
      try {
        const response = await fetch(request);

        // Only modify same-origin responses
        if (!isSameOrigin) {
          return response;
        }

        // Clone the response to modify headers
        const newHeaders = new Headers(response.headers);

        // Add cross-origin isolation headers
        newHeaders.set('Cross-Origin-Opener-Policy', 'same-origin');
        newHeaders.set('Cross-Origin-Embedder-Policy', 'require-corp');

        // For cross-origin resources loaded by this page, we need credentialless
        // This allows loading resources like fonts and scripts from CDNs
        newHeaders.set('Cross-Origin-Resource-Policy', 'cross-origin');

        return new Response(response.body, {
          status: response.status,
          statusText: response.statusText,
          headers: newHeaders,
        });
      } catch (error) {
        log('Fetch error:', error);
        return fetch(request);
      }
    })()
  );
});

log('Service worker script loaded');
