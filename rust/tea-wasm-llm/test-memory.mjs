// Memory test for wllama WASM loading
import { Wllama } from './node_modules/@wllama/wllama/esm/index.js';

const MODEL_URL = 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q2_K.gguf';

console.log('=== WASM Memory Test ===');
console.log('Model:', MODEL_URL.split('/').pop());
console.log('Initial heap:', Math.round(process.memoryUsage().heapUsed / 1024 / 1024), 'MB');

const wllama = new Wllama({
  'single-thread/wllama.wasm': './node_modules/@wllama/wllama/esm/single-thread/wllama.wasm',
  'multi-thread/wllama.wasm': './node_modules/@wllama/wllama/esm/multi-thread/wllama.wasm',
});

try {
  console.log('\nDownloading and loading model...');
  
  await wllama.loadModelFromUrl(MODEL_URL, {
    n_ctx: 2048,
    progressCallback: ({ loaded, total }) => {
      const pct = Math.round(loaded / total * 100);
      const heapMB = Math.round(process.memoryUsage().heapUsed / 1024 / 1024);
      const rssMB = Math.round(process.memoryUsage().rss / 1024 / 1024);
      if (pct % 10 === 0 || pct === 100) {
        console.log(`  ${pct}% | Heap: ${heapMB}MB | RSS: ${rssMB}MB`);
      }
    }
  });
  
  console.log('\n✓ Model loaded!');
  console.log('Final RSS:', Math.round(process.memoryUsage().rss / 1024 / 1024), 'MB');
  
  const result = await wllama.createCompletion('Hello', { nPredict: 5 });
  console.log('Test:', result);
  
  await wllama.exit();
} catch (err) {
  console.error('Error:', err.message);
}
