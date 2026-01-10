/**
 * TEA WASM LLM Demo Application
 *
 * Demonstrates YAML workflow execution with browser-based LLM.
 * Features syntax highlighting with CodeMirror.
 */

// CodeMirror imports
import { EditorView, basicSetup } from 'codemirror';
import { yaml } from '@codemirror/lang-yaml';
import { oneDark } from '@codemirror/theme-one-dark';
import { EditorState } from '@codemirror/state';

// YAML serialization
import jsYaml from 'js-yaml';

// Import from the bundled package
import {
  initLlm,
  initTeaLlm,
  chat,
  executeLlmYaml,
  getLlmCacheStats,
  clearLlmCache,
  hasCoopCoep,
  getVersion,
  // Lua callback bridge
  registerLuaCallback,
  clearLuaCallback,
  isLuaCallbackRegistered,
  // Prolog handler bridge
  registerPrologHandler,
  clearPrologHandler,
  isPrologHandlerRegistered,
} from './pkg/index.js';

// Lua engine instance
let luaEngine = null;

// Prolog engine instance
let prologEngine = null;

// Initialize Lua engine (wasmoon)
async function initLuaEngine() {
  if (luaEngine) return luaEngine;
  try {
    const { LuaFactory } = await import('wasmoon');
    const factory = new LuaFactory();
    luaEngine = await factory.createEngine();
    console.log('[TEA-DEMO] Lua engine loaded (wasmoon)');

    // Register Lua callback with TEA
    registerLuaCallback(async (code, stateJson) => {
      const state = JSON.parse(stateJson);
      // Set 'state' as a global object (allows state.think.content syntax)
      luaEngine.global.set('state', state);
      // Also set individual keys as globals for convenience (allows think.content syntax)
      for (const [key, value] of Object.entries(state)) {
        luaEngine.global.set(key, value);
      }
      // Execute Lua code
      const result = await luaEngine.doString(code);
      return JSON.stringify({ result });
    });

    console.log('[TEA-DEMO] Lua callback registered');
    return luaEngine;
  } catch (e) {
    console.warn('[TEA-DEMO] Could not load Lua engine:', e.message);
    return null;
  }
}

// Initialize Prolog engine (trealla)
async function initPrologEngine() {
  if (prologEngine) return prologEngine;
  try {
    const { Prolog } = await import('trealla');
    prologEngine = new Prolog();
    console.log('[TEA-DEMO] Prolog engine loaded (trealla)');

    // Register Prolog handler with TEA
    registerPrologHandler(async (queryJson) => {
      const { code, facts } = JSON.parse(queryJson);
      try {
        // Consult facts if provided
        if (facts) {
          await prologEngine.consultText(facts);
        }

        // Run query and collect results
        const query = prologEngine.query(code);
        const bindings = [];

        for await (const result of query) {
          if (result.status === 'success' && result.answer) {
            bindings.push(result.answer);
          }
        }

        return JSON.stringify({ bindings, success: true });
      } catch (e) {
        return JSON.stringify({ bindings: [], success: false, error: e.message });
      }
    });

    console.log('[TEA-DEMO] Prolog handler registered');
    return prologEngine;
  } catch (e) {
    console.warn('[TEA-DEMO] Could not load Prolog engine:', e.message);
    return null;
  }
}

// Configuration
const MODEL_URL = 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q2_K.gguf';

// Default YAML workflow (implicit edges - no edges section needed)
const DEFAULT_YAML = `name: multi-engine-workflow
nodes:
  # Step 1: Use Lua to analyze the question
  - name: lua_analyze
    action: lua.eval
    with:
      code: |
        local words = 0
        for _ in string.gmatch(question, "%S+") do
          words = words + 1
        end
        return { word_count = words, is_short = words < 10 }

  # Step 2: Use Prolog for logical classification
  - name: prolog_classify
    action: prolog.query
    with:
      code: "category(X), X \\\\= unknown"
      facts: |
        category(geography) :- sub_string("{{ state.question }}", _, _, _, "capital").
        category(geography) :- sub_string("{{ state.question }}", _, _, _, "country").
        category(math) :- sub_string("{{ state.question }}", _, _, _, "calculate").
        category(math) :- sub_string("{{ state.question }}", _, _, _, "sum").
        category(science) :- sub_string("{{ state.question }}", _, _, _, "why").
        category(unknown).

  # Step 3: LLM thinks about the question
  - name: think
    action: llm.call
    with:
      prompt: |
        Question: {{ state.question }}
        Word count: {{ state.lua_result.word_count }}
        Think step by step about this question.
      max_tokens: 150
      temperature: 0.3

  # Step 4: LLM provides final answer
  - name: answer
    action: llm.call
    with:
      prompt: |
        Based on: {{ state.think.content }}
        Provide a concise answer.
      max_tokens: 100
      temperature: 0.7`;

// Default state (YAML format)
const DEFAULT_STATE_YAML = `question: What is the capital of France?`;

// DOM Elements
const statusEl = document.getElementById('status');
const threadingStatusEl = document.getElementById('threading-status');
const cacheStatusEl = document.getElementById('cache-status');
const progressContainer = document.getElementById('progress-container');
const progressFill = document.getElementById('progress-fill');
const progressText = document.getElementById('progress-text');
const runYamlBtn = document.getElementById('run-yaml-btn');
const yamlOutput = document.getElementById('yaml-output');
const clearCacheBtn = document.getElementById('clear-cache-btn');
const reloadModelBtn = document.getElementById('reload-model-btn');
const versionEl = document.getElementById('version');

// CodeMirror editors
let yamlEditor;
let stateEditor;
let outputEditor;

// Initialize CodeMirror editors
function initEditors() {
  // YAML editor
  yamlEditor = new EditorView({
    state: EditorState.create({
      doc: DEFAULT_YAML,
      extensions: [
        basicSetup,
        yaml(),
        oneDark,
        EditorView.lineWrapping,
      ],
    }),
    parent: document.getElementById('yaml-editor'),
  });

  // YAML state editor
  stateEditor = new EditorView({
    state: EditorState.create({
      doc: DEFAULT_STATE_YAML,
      extensions: [
        basicSetup,
        yaml(),
        oneDark,
        EditorView.lineWrapping,
      ],
    }),
    parent: document.getElementById('state-editor'),
  });

  // Output editor (read-only, YAML)
  outputEditor = new EditorView({
    state: EditorState.create({
      doc: '',
      extensions: [
        basicSetup,
        yaml(),
        oneDark,
        EditorView.lineWrapping,
        EditorState.readOnly.of(true),
      ],
    }),
    parent: document.getElementById('output-editor'),
  });
}

// Get editor content
function getYamlContent() {
  return yamlEditor.state.doc.toString();
}

function getStateContent() {
  return stateEditor.state.doc.toString();
}

// Set output content
function setOutputContent(content) {
  outputEditor.dispatch({
    changes: {
      from: 0,
      to: outputEditor.state.doc.length,
      insert: content,
    },
  });
}

// Update status display
function updateStatus(text, type = 'loading') {
  statusEl.textContent = text;
  statusEl.className = `status-${type}`;
}

// Update progress bar
function updateProgress(loaded, total) {
  const percent = Math.round((loaded / total) * 100);
  progressFill.style.width = `${percent}%`;
  progressText.textContent = `${percent}%`;
}

// Show/hide progress bar
function showProgress(show) {
  if (show) {
    progressContainer.classList.remove('hidden');
  } else {
    progressContainer.classList.add('hidden');
  }
}

// Update cache status
async function updateCacheStatus() {
  try {
    const stats = await getLlmCacheStats();
    if (stats.totalSize > 0) {
      const sizeGB = (stats.totalSize / (1024 * 1024 * 1024)).toFixed(1);
      cacheStatusEl.textContent = `Cache: ${sizeGB}GB`;
    } else {
      cacheStatusEl.textContent = 'Cache: Empty';
    }
  } catch (e) {
    cacheStatusEl.textContent = 'Cache: N/A';
  }
}

// Update threading status
function updateThreadingStatus() {
  const multiThread = hasCoopCoep();
  threadingStatusEl.textContent = multiThread ? 'Multi-thread' : 'Single-thread';
}

// Initialize LLM
async function initializeLlm() {
  try {
    updateStatus('Loading model...', 'loading');
    showProgress(true);
    updateThreadingStatus();

    await initLlm({
      modelUrl: MODEL_URL,
      useCdn: true,
      verbose: true,
      onProgress: (loaded, total) => {
        updateProgress(loaded, total);
        const percent = Math.round((loaded / total) * 100);
        updateStatus(`Loading model: ${percent}%`, 'loading');
      },
      onReady: () => {
        updateStatus('Ready', 'ready');
        showProgress(false);
        runYamlBtn.disabled = false;
      },
    });

    // Initialize TEA YAML engine with LLM handler
    await initTeaLlm({ verbose: true }, async (paramsJson) => {
      const params = JSON.parse(paramsJson);
      const response = await chat(params.prompt, {
        maxTokens: params.max_tokens || 100,
        temperature: params.temperature || 0.7,
      });
      return JSON.stringify({ content: response.content });
    });

    // Initialize Lua and Prolog engines (non-blocking)
    Promise.all([initLuaEngine(), initPrologEngine()]).then(([lua, prolog]) => {
      const engines = [];
      if (lua) engines.push('Lua');
      if (prolog) engines.push('Prolog');
      if (engines.length > 0) {
        updateStatus(`Ready (with ${engines.join(' + ')})`, 'ready');
      }
    });

    await updateCacheStatus();

  } catch (error) {
    console.error('Initialization failed:', error);
    updateStatus(`Error: ${error.message}`, 'error');
    showProgress(false);
  }
}

// Handle YAML execution
runYamlBtn.addEventListener('click', async () => {
  const yamlContent = getYamlContent();
  let state;

  try {
    state = jsYaml.load(getStateContent());
  } catch (e) {
    yamlOutput.classList.remove('hidden');
    setOutputContent(`# Error parsing state YAML:\n# ${e.message}`);
    return;
  }

  try {
    runYamlBtn.disabled = true;
    runYamlBtn.textContent = 'Running...';

    const result = await executeLlmYaml(yamlContent, state);

    yamlOutput.classList.remove('hidden');
    // Filter out null/undefined values recursively
    const cleanResult = JSON.parse(JSON.stringify(result, (key, value) =>
      value === null || value === undefined ? undefined : value
    ));
    // Convert output to YAML with nice formatting
    setOutputContent(jsYaml.dump(cleanResult, {
      indent: 2,
      lineWidth: 80,
      noRefs: true,
      sortKeys: false,
    }));

  } catch (error) {
    console.error('YAML execution error:', error);
    yamlOutput.classList.remove('hidden');
    setOutputContent(`Error: ${error.message}`);
  } finally {
    runYamlBtn.disabled = false;
    runYamlBtn.textContent = 'Run YAML Workflow';
  }
});

// Handle clear cache
clearCacheBtn.addEventListener('click', async () => {
  try {
    await clearLlmCache();
    await updateCacheStatus();
    yamlOutput.classList.remove('hidden');
    setOutputContent('Cache cleared successfully.');
  } catch (error) {
    console.error('Clear cache error:', error);
    yamlOutput.classList.remove('hidden');
    setOutputContent(`Failed to clear cache: ${error.message}`);
  }
});

// Handle reload model
reloadModelBtn.addEventListener('click', async () => {
  try {
    await clearLlmCache();
    runYamlBtn.disabled = true;
    await initializeLlm();
  } catch (error) {
    console.error('Reload error:', error);
  }
});

// Set version
try {
  versionEl.textContent = `v${getVersion()}`;
} catch {
  versionEl.textContent = '';
}

// Initialize editors first, then LLM
initEditors();
initializeLlm();
