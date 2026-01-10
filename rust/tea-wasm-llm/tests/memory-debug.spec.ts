import { test, expect } from '@playwright/test';

test.describe('WASM Memory Debug', () => {
  test.setTimeout(300000); // 5 min for model download

  test('track memory during model load', async ({ page }) => {
    // Capture console logs
    page.on('console', msg => {
      console.log(`[Browser] ${msg.text()}`);
    });

    // Capture page crashes
    page.on('crash', () => {
      console.error('PAGE CRASHED - SIGKILL/OOM');
    });

    // Navigate to demo
    await page.goto('https://fabceolin.github.io/the_edge_agent/wasm-demo/');

    // Inject memory monitoring
    await page.evaluate(() => {
      setInterval(() => {
        const mem = (performance as any).memory;
        if (mem) {
          const used = Math.round(mem.usedJSHeapSize / 1024 / 1024);
          const limit = Math.round(mem.jsHeapSizeLimit / 1024 / 1024);
          console.log('Memory: ' + used + 'MB / ' + limit + 'MB limit');
        }
      }, 2000);
    });

    // Wait for model to load or crash
    try {
      await page.waitForSelector('#status:has-text("Ready")', { timeout: 300000 });
      console.log('Model loaded successfully!');
    } catch (e) {
      console.error('Model failed to load');
      await page.screenshot({ path: 'memory-crash.png' });
    }
  });
});
