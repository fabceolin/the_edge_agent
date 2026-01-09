/**
 * TEA WASM LLM - Tiny Model Test
 *
 * This test loads a tiny LLM model (~500KB) using wllama and runs
 * a complete workflow to verify the full integration works.
 *
 * Model: ggml-org/models/tinyllamas/stories260K.gguf (~500KB)
 *
 * Requirements:
 * - Browser with WASM support
 * - Network access to HuggingFace CDN
 * - ~10-30 seconds for model download and inference
 *
 * Usage:
 * 1. Serve this directory with a web server (required for ES modules)
 * 2. Open tiny-model-test.html in a browser
 * 3. Click "Run Test" to execute
 *
 * For multi-threading support, serve with these headers:
 *   Cross-Origin-Opener-Policy: same-origin
 *   Cross-Origin-Embedder-Policy: require-corp
 */

// Import TEA WASM LLM
import init, {
    execute_yaml,
    set_llm_handler,
    has_llm_handler,
    has_shared_array_buffer,
    version,
} from '../pkg/tea_wasm_llm.js';

// Wllama configuration
const WLLAMA_CONFIG = {
    'single-thread/wllama.wasm': 'https://cdn.jsdelivr.net/npm/@anthropic-ai/wllama@2.3.6/esm/single-thread/wllama.wasm',
    'multi-thread/wllama.wasm': 'https://cdn.jsdelivr.net/npm/@anthropic-ai/wllama@2.3.6/esm/multi-thread/wllama.wasm',
};

// Tiny model for testing (~500KB)
const TINY_MODEL = {
    repo: 'ggml-org/models',
    file: 'tinyllamas/stories260K.gguf',
};

// Test state
let wllama = null;
let modelLoaded = false;

/**
 * Log message to console and DOM
 */
function log(message, type = 'info') {
    const timestamp = new Date().toISOString().slice(11, 23);
    console.log(`[${timestamp}] ${message}`);

    const output = document.getElementById('output');
    if (output) {
        const div = document.createElement('div');
        div.className = `log-${type}`;
        div.textContent = `[${timestamp}] ${message}`;
        output.appendChild(div);
        output.scrollTop = output.scrollHeight;
    }
}

/**
 * Initialize wllama and load tiny model
 */
async function initWllama() {
    log('Loading wllama library...');

    // Dynamic import of wllama from CDN
    const { Wllama } = await import('https://cdn.jsdelivr.net/npm/@anthropic-ai/wllama@2.3.6/esm/index.js');

    log('Creating wllama instance...');
    wllama = new Wllama(WLLAMA_CONFIG);

    log(`Loading tiny model: ${TINY_MODEL.repo}/${TINY_MODEL.file}`);
    log('This may take 10-30 seconds on first load...');

    await wllama.loadModelFromHF(TINY_MODEL.repo, TINY_MODEL.file, {
        progressCallback: ({ loaded, total }) => {
            if (total > 0) {
                const pct = Math.round((loaded / total) * 100);
                log(`Download progress: ${pct}%`, 'progress');
            }
        },
    });

    modelLoaded = true;
    log('✅ Model loaded successfully!', 'success');

    return wllama;
}

/**
 * Create LLM handler that uses wllama
 */
function createWllamaHandler(wllama) {
    return async (paramsJson) => {
        const params = JSON.parse(paramsJson);

        log(`LLM call: prompt="${params.prompt.slice(0, 50)}..." max_tokens=${params.max_tokens || 20}`);

        // Handle embedding requests
        if (params.action === 'embed') {
            log('Embedding not supported by this model', 'warn');
            return JSON.stringify([0.0]); // Placeholder
        }

        // Generate completion
        const result = await wllama.createCompletion(params.prompt, {
            nPredict: params.max_tokens || 20,
            sampling: {
                temp: params.temperature || 0.7,
                top_p: params.top_p || 0.9,
            },
        });

        log(`LLM response: "${result.slice(0, 50)}..."`, 'response');

        return JSON.stringify({
            content: result,
            model: 'tinyllamas/stories260K',
        });
    };
}

/**
 * Run the complete test
 */
async function runTest() {
    const startTime = Date.now();

    try {
        log('🚀 Starting TEA WASM LLM + wllama integration test');
        log('');

        // Step 1: Initialize TEA WASM
        log('Step 1: Initializing TEA WASM LLM...');
        await init();
        log(`Version: ${version()}`);
        log(`SharedArrayBuffer: ${has_shared_array_buffer()}`);
        log('');

        // Step 2: Load wllama and model
        log('Step 2: Loading wllama and tiny model...');
        const wllama = await initWllama();
        log('');

        // Step 3: Register handler
        log('Step 3: Registering wllama handler...');
        set_llm_handler(createWllamaHandler(wllama));
        log(`Handler registered: ${has_llm_handler()}`);
        log('');

        // Step 4: Execute YAML workflow
        log('Step 4: Executing YAML workflow...');

        const yaml = `
name: tiny-model-test
nodes:
  - name: generate
    action: llm.call
    with:
      prompt: "Once upon a time, there was a"
      max_tokens: 20
edges:
  - from: __start__
    to: generate
  - from: generate
    to: __end__
`;

        log('YAML workflow:');
        log(yaml.trim());
        log('');

        const result = await execute_yaml(yaml, '{}');
        const parsed = JSON.parse(result);

        log('');
        log('📋 Result:', 'success');
        log(JSON.stringify(parsed, null, 2));

        // Validate result
        if (!parsed.llm_response) {
            throw new Error('Missing llm_response in result');
        }
        if (!parsed.llm_response.content) {
            throw new Error('Missing content in llm_response');
        }
        if (parsed.llm_response.content.length === 0) {
            throw new Error('Empty content in llm_response');
        }

        const duration = ((Date.now() - startTime) / 1000).toFixed(2);
        log('');
        log(`✅ TEST PASSED in ${duration}s`, 'success');
        log('');
        log('Generated text: ' + parsed.llm_response.content);

        return { success: true, result: parsed, duration };

    } catch (error) {
        const duration = ((Date.now() - startTime) / 1000).toFixed(2);
        log('');
        log(`❌ TEST FAILED in ${duration}s: ${error.message}`, 'error');
        console.error(error);

        return { success: false, error: error.message, duration };
    }
}

// Export for use in HTML
window.runTest = runTest;
window.log = log;

// Auto-run if not in HTML context
if (typeof document === 'undefined') {
    runTest().then(result => {
        process.exit(result.success ? 0 : 1);
    });
}
