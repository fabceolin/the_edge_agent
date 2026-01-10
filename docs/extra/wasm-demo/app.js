/**
 * TEA WASM LLM Demo Application
 *
 * Demonstrates YAML workflow execution with browser-based LLM.
 * Features syntax highlighting with CodeMirror.
 */

// CodeMirror imports
import { EditorView, basicSetup } from 'codemirror';
import { json } from '@codemirror/lang-json';
import { yaml } from '@codemirror/lang-yaml';
import { oneDark } from '@codemirror/theme-one-dark';
import { EditorState } from '@codemirror/state';

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
} from './pkg/index.js';

// Configuration
const MODEL_URL = 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q2_K.gguf';

// Default YAML workflow (implicit edges - no edges section needed)
const DEFAULT_YAML = `name: qa-workflow
nodes:
  - name: think
    action: llm.call
    with:
      prompt: "Think step by step about: {{ state.question }}"
      max_tokens: 150
      temperature: 0.3

  - name: answer
    action: llm.call
    with:
      prompt: "Based on this analysis: {{ state.think.content }}\\nProvide a concise answer."
      max_tokens: 100
      temperature: 0.7

# No edges needed - implicit flow: think -> answer -> __end__`;

// Default state
const DEFAULT_STATE = {
  question: "What is the capital of France?"
};

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

  // JSON state editor
  stateEditor = new EditorView({
    state: EditorState.create({
      doc: JSON.stringify(DEFAULT_STATE, null, 2),
      extensions: [
        basicSetup,
        json(),
        oneDark,
        EditorView.lineWrapping,
      ],
    }),
    parent: document.getElementById('state-editor'),
  });

  // Output editor (read-only)
  outputEditor = new EditorView({
    state: EditorState.create({
      doc: '',
      extensions: [
        basicSetup,
        json(),
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
    state = JSON.parse(getStateContent());
  } catch (e) {
    yamlOutput.classList.remove('hidden');
    setOutputContent(`Error parsing state JSON: ${e.message}`);
    return;
  }

  try {
    runYamlBtn.disabled = true;
    runYamlBtn.textContent = 'Running...';

    const result = await executeLlmYaml(yamlContent, state);

    yamlOutput.classList.remove('hidden');
    setOutputContent(JSON.stringify(result, null, 2));

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
