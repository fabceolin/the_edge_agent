/**
 * TEA WASM LLM Demo Application
 *
 * Demonstrates the batteries-included browser LLM with:
 * - Simple chat interface
 * - Model loading progress
 * - Cache status display
 * - YAML workflow execution
 */

// Import from the bundled package in pkg/ directory
import {
  initLlm,
  chat,
  chatStream,
  executeLlmYaml,
  getLlmCacheStats,
  clearLlmCache,
  hasCoopCoep,
  isLlmReady,
  getVersion,
} from './pkg/index.js';

// Configuration - Using Gemma 3 1B Q4 for browser compatibility (~700MB vs ~1.3GB Q8)
const MODEL_URL = 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q4_K_M.gguf';

// DOM Elements
const statusEl = document.getElementById('status');
const threadingStatusEl = document.getElementById('threading-status');
const cacheStatusEl = document.getElementById('cache-status');
const progressContainer = document.getElementById('progress-container');
const progressFill = document.getElementById('progress-fill');
const progressText = document.getElementById('progress-text');
const messagesEl = document.getElementById('messages');
const chatForm = document.getElementById('chat-form');
const chatInput = document.getElementById('chat-input');
const sendBtn = document.getElementById('send-btn');
const yamlInput = document.getElementById('yaml-input');
const stateInput = document.getElementById('state-input');
const runYamlBtn = document.getElementById('run-yaml-btn');
const yamlOutput = document.getElementById('yaml-output');
const yamlResult = document.getElementById('yaml-result');
const clearCacheBtn = document.getElementById('clear-cache-btn');
const reloadModelBtn = document.getElementById('reload-model-btn');
const versionEl = document.getElementById('version');

// Tab switching
document.querySelectorAll('.tab').forEach(tab => {
  tab.addEventListener('click', () => {
    document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
    document.querySelectorAll('.tab-content').forEach(c => c.classList.add('hidden'));

    tab.classList.add('active');
    const tabId = tab.dataset.tab + '-tab';
    document.getElementById(tabId).classList.remove('hidden');
  });
});

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

// Add message to chat
function addMessage(role, content, isStreaming = false) {
  const messageDiv = document.createElement('div');
  messageDiv.className = `message ${role}${isStreaming ? ' streaming' : ''}`;
  messageDiv.textContent = content;
  messagesEl.appendChild(messageDiv);
  messagesEl.scrollTop = messagesEl.scrollHeight;
  return messageDiv;
}

// Update last message (for streaming)
function updateLastMessage(content) {
  const lastMessage = messagesEl.lastElementChild;
  if (lastMessage) {
    lastMessage.textContent = content;
    lastMessage.classList.remove('streaming');
    messagesEl.scrollTop = messagesEl.scrollHeight;
  }
}

// Enable/disable chat controls
function enableChat(enabled) {
  chatInput.disabled = !enabled;
  sendBtn.disabled = !enabled;
  runYamlBtn.disabled = !enabled;
}

// Initialize LLM
async function initializeLlm() {
  try {
    updateStatus('Loading model...', 'loading');
    showProgress(true);
    updateThreadingStatus();

    await initLlm({
      modelUrl: MODEL_URL,
      useCdn: true, // Use CDN for wllama assets
      verbose: true,
      onProgress: (loaded, total) => {
        updateProgress(loaded, total);
        const percent = Math.round((loaded / total) * 100);
        updateStatus(`Loading model: ${percent}%`, 'loading');
      },
      onReady: () => {
        updateStatus('Ready', 'ready');
        showProgress(false);
        enableChat(true);
      },
    });

    await updateCacheStatus();

  } catch (error) {
    console.error('Initialization failed:', error);
    updateStatus(`Error: ${error.message}`, 'error');
    showProgress(false);
    addMessage('error', `Failed to initialize: ${error.message}`);
  }
}

// Handle chat form submission
chatForm.addEventListener('submit', async (e) => {
  e.preventDefault();

  const prompt = chatInput.value.trim();
  if (!prompt || !isLlmReady()) return;

  chatInput.value = '';
  addMessage('user', prompt);

  try {
    enableChat(false);

    // Create streaming message
    const messageDiv = addMessage('assistant', '', true);
    let fullResponse = '';

    await chatStream(prompt, (token) => {
      fullResponse += token;
      messageDiv.textContent = fullResponse;
      messagesEl.scrollTop = messagesEl.scrollHeight;
    }, {
      maxTokens: 200,
      temperature: 0.7,
    });

    messageDiv.classList.remove('streaming');

  } catch (error) {
    console.error('Chat error:', error);
    addMessage('error', `Error: ${error.message}`);
  } finally {
    enableChat(true);
    chatInput.focus();
  }
});

// Handle YAML execution
runYamlBtn.addEventListener('click', async () => {
  const yaml = yamlInput.value.trim();
  let state;

  try {
    state = JSON.parse(stateInput.value.trim());
  } catch (e) {
    yamlOutput.classList.remove('hidden');
    yamlResult.textContent = `Error parsing state JSON: ${e.message}`;
    return;
  }

  try {
    runYamlBtn.disabled = true;
    runYamlBtn.textContent = 'Running...';

    const result = await executeLlmYaml(yaml, state);

    yamlOutput.classList.remove('hidden');
    yamlResult.textContent = JSON.stringify(result, null, 2);

  } catch (error) {
    console.error('YAML execution error:', error);
    yamlOutput.classList.remove('hidden');
    yamlResult.textContent = `Error: ${error.message}`;
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
    addMessage('assistant', 'Cache cleared successfully.');
  } catch (error) {
    console.error('Clear cache error:', error);
    addMessage('error', `Failed to clear cache: ${error.message}`);
  }
});

// Handle reload model
reloadModelBtn.addEventListener('click', async () => {
  try {
    await clearLlmCache();
    enableChat(false);
    await initializeLlm();
  } catch (error) {
    console.error('Reload error:', error);
    addMessage('error', `Failed to reload: ${error.message}`);
  }
});

// Set version
try {
  versionEl.textContent = `v${getVersion()}`;
} catch {
  versionEl.textContent = '';
}

// Initialize on load
initializeLlm();
