/**
 * TEA WASM LLM Demo Application
 *
 * Demonstrates YAML workflow execution with browser-based LLM.
 * Features syntax highlighting with CodeMirror.
 * Includes Mermaid graph visualization for agent workflows.
 */

// CodeMirror imports
import { EditorView, basicSetup } from 'codemirror';
import { yaml } from '@codemirror/lang-yaml';
import { oneDark } from '@codemirror/theme-one-dark';
import { EditorState } from '@codemirror/state';

// YAML serialization
import jsYaml from 'js-yaml';

// ============================================================================
// Mermaid Graph Visualization
// ============================================================================

// Mermaid instance cache
let mermaidInstance = null;

/**
 * Dynamically load Mermaid.js from CDN
 * @returns {Promise<object>} Mermaid instance
 */
async function loadMermaid() {
  if (mermaidInstance) return mermaidInstance;

  return new Promise((resolve, reject) => {
    const script = document.createElement('script');
    script.src = 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js';
    script.onload = () => {
      mermaid.initialize({
        startOnLoad: false,
        theme: 'dark',
        themeVariables: {
          primaryColor: '#61afef',
          primaryTextColor: '#abb2bf',
          primaryBorderColor: '#528bff',
          lineColor: '#5c6370',
          secondaryColor: '#2c313a',
          tertiaryColor: '#21252b',
          background: '#21252b',
          mainBkg: '#2c313a',
          nodeBorder: '#528bff',
          clusterBkg: '#21252b',
          clusterBorder: '#3a3f4b',
          titleColor: '#e6e6e6',
          edgeLabelBackground: '#21252b',
        },
        flowchart: {
          htmlLabels: true,
          curve: 'basis',
        },
      });
      mermaidInstance = mermaid;
      console.log('[TEA-DEMO] Mermaid.js loaded');
      resolve(mermaid);
    };
    script.onerror = (e) => {
      console.error('[TEA-DEMO] Failed to load Mermaid.js:', e);
      reject(new Error('Failed to load Mermaid.js'));
    };
    document.head.appendChild(script);
  });
}

/**
 * Escape node name for Mermaid syntax
 * @param {string} name - Node name
 * @returns {string} Escaped name
 */
function escapeNodeId(name) {
  return name.replace(/[^a-zA-Z0-9_]/g, '_');
}

/**
 * Escape label for Mermaid display
 * @param {string} label - Label text
 * @returns {string} Escaped label
 */
function escapeLabel(label) {
  return label
    .replace(/"/g, "'")
    .replace(/\|/g, '/')
    .replace(/\[/g, '(')
    .replace(/\]/g, ')');
}

/**
 * Generate Mermaid graph syntax from YAML workflow
 * @param {string} yamlContent - YAML workflow content
 * @returns {string|null} Mermaid syntax or null on error
 */
function generateMermaidFromYaml(yamlContent) {
  try {
    const config = jsYaml.load(yamlContent);
    if (!config || !config.nodes || config.nodes.length === 0) {
      return null;
    }

    const lines = ['graph TD'];
    const nodeNames = config.nodes.map(n => n.name);
    const edges = new Set();

    // Add start node
    lines.push('    __start__((Start))');

    // Add regular nodes
    for (const node of config.nodes) {
      const nodeId = escapeNodeId(node.name);
      const action = node.action || 'passthrough';
      const label = escapeLabel(`${node.name}\\n[${action}]`);
      lines.push(`    ${nodeId}["${label}"]`);
    }

    // Add end node
    lines.push('    __end__((End))');

    // Build edges from explicit edges or implicit sequential order
    if (config.edges && config.edges.length > 0) {
      // Use explicit edges
      for (const edge of config.edges) {
        const fromId = escapeNodeId(edge.from);
        const toId = escapeNodeId(edge.to);
        const edgeKey = `${fromId}->${toId}`;
        if (!edges.has(edgeKey)) {
          edges.add(edgeKey);
          if (edge.condition) {
            lines.push(`    ${fromId}-->|${escapeLabel(edge.condition)}|${toId}`);
          } else {
            lines.push(`    ${fromId}-->${toId}`);
          }
        }
      }
    } else {
      // Implicit sequential order: __start__ -> first node -> ... -> last node -> __end__
      if (nodeNames.length > 0) {
        // Start to first node
        const firstNodeId = escapeNodeId(nodeNames[0]);
        lines.push(`    __start__-->${firstNodeId}`);

        // Sequential edges between nodes
        for (let i = 0; i < nodeNames.length - 1; i++) {
          const fromId = escapeNodeId(nodeNames[i]);
          const toId = escapeNodeId(nodeNames[i + 1]);
          lines.push(`    ${fromId}-->${toId}`);
        }

        // Last node to end
        const lastNodeId = escapeNodeId(nodeNames[nodeNames.length - 1]);
        lines.push(`    ${lastNodeId}-->__end__`);
      }
    }

    return lines.join('\n');
  } catch (e) {
    console.warn('[TEA-DEMO] Could not generate Mermaid graph:', e.message);
    return null;
  }
}

/**
 * Render agent graph using Mermaid
 * @param {string|null} mermaidSyntax - Mermaid diagram syntax
 */
async function renderAgentGraph(mermaidSyntax) {
  const container = document.getElementById('mermaid-container');
  const errorDiv = document.getElementById('graph-error');
  const statusBadge = document.getElementById('graph-status');

  if (!container) return;

  if (!mermaidSyntax) {
    container.innerHTML = '';
    if (errorDiv) errorDiv.classList.remove('hidden');
    if (statusBadge) {
      statusBadge.textContent = '';
      statusBadge.className = 'status-badge';
    }
    return;
  }

  try {
    const mermaid = await loadMermaid();
    if (errorDiv) errorDiv.classList.add('hidden');

    // Generate unique ID for this render
    const id = `graph-${Date.now()}`;

    // Render mermaid to SVG
    const { svg } = await mermaid.render(id, mermaidSyntax);
    container.innerHTML = svg;

    if (statusBadge) {
      statusBadge.textContent = '✓';
      statusBadge.className = 'status-badge success';
    }

    console.log('[TEA-DEMO] Graph rendered successfully');
  } catch (e) {
    console.error('[TEA-DEMO] Graph render error:', e);
    container.innerHTML = '';
    if (errorDiv) errorDiv.classList.remove('hidden');
    if (statusBadge) {
      statusBadge.textContent = '✗';
      statusBadge.className = 'status-badge error';
    }
  }
}

/**
 * Toggle graph panel expand/collapse
 */
function toggleGraphPanel() {
  const panel = document.getElementById('graph-panel');
  if (panel) {
    panel.classList.toggle('expanded');
    // Remember state in localStorage
    localStorage.setItem('graphPanelExpanded', panel.classList.contains('expanded'));
  }
}

/**
 * Restore graph panel state from localStorage
 */
function restoreGraphPanelState() {
  const expanded = localStorage.getItem('graphPanelExpanded') === 'true';
  const panel = document.getElementById('graph-panel');
  if (panel && expanded) {
    panel.classList.add('expanded');
  }
}

// Make toggleGraphPanel available globally for onclick handler
window.toggleGraphPanel = toggleGraphPanel;

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
  // Opik tracing bridge
  initOpikTracing,
  disableOpikTracing,
  isOpikTracingEnabled,
  registerOpikCallback,
} from './pkg/index.js';

// Import JSON language support for trace detail editors
import { json } from '@codemirror/lang-json';

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

// ============================================================================
// Opik Tracing
// ============================================================================

// Collected traces for display
let opikTraces = [];
let opikConnected = false;

// Trace detail editors
let traceInputEditor = null;
let traceOutputEditor = null;

/**
 * Initialize Opik tracing with the provided API key
 */
async function connectOpik() {
  const apiKeyInput = document.getElementById('opik-api-key');
  const projectInput = document.getElementById('opik-project');
  const connectBtn = document.getElementById('opik-connect-btn');
  const statusIndicator = document.getElementById('opik-status-indicator');
  const statusText = document.getElementById('opik-status-text');

  const apiKey = apiKeyInput.value.trim();
  const projectName = projectInput.value.trim() || 'tea-wasm-demo';

  if (!apiKey) {
    statusText.textContent = 'API key required';
    statusIndicator.className = 'status-indicator disconnected';
    return;
  }

  // Update UI to connecting state
  statusIndicator.className = 'status-indicator connecting';
  statusText.textContent = 'Connecting...';
  connectBtn.disabled = true;
  connectBtn.textContent = 'Connecting...';

  try {
    // Initialize Opik tracing
    await initOpikTracing({
      apiKey: apiKey,
      projectName: projectName,
      verbose: true,
    });

    // Register custom callback to capture traces for display
    registerOpikCallback(async (traceJson) => {
      const trace = JSON.parse(traceJson);
      addTraceToList(trace, 'sending');

      // Send to Opik API
      try {
        const response = await fetch('https://www.comet.com/opik/api/v1/private/traces', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Bearer ${apiKey}`,
          },
          body: traceJson,
        });

        if (response.ok) {
          updateTraceStatus(trace.id, 'sent');
          console.log('[TEA-DEMO-OPIK] Trace sent successfully:', trace.name);
        } else {
          updateTraceStatus(trace.id, 'failed');
          console.warn('[TEA-DEMO-OPIK] Failed to send trace:', response.status);
        }
      } catch (error) {
        updateTraceStatus(trace.id, 'failed');
        console.error('[TEA-DEMO-OPIK] Error sending trace:', error);
      }
    });

    // Store API key in localStorage for convenience
    localStorage.setItem('OPIK_API_KEY', apiKey);
    localStorage.setItem('OPIK_PROJECT_NAME', projectName);

    // Update UI to connected state
    opikConnected = true;
    statusIndicator.className = 'status-indicator connected';
    statusText.textContent = `Connected to ${projectName}`;
    connectBtn.textContent = 'Disconnect';
    connectBtn.disabled = false;

    console.log('[TEA-DEMO-OPIK] Connected to Opik');
  } catch (error) {
    console.error('[TEA-DEMO-OPIK] Failed to connect:', error);
    statusIndicator.className = 'status-indicator disconnected';
    statusText.textContent = `Error: ${error.message}`;
    connectBtn.textContent = 'Connect';
    connectBtn.disabled = false;
  }
}

/**
 * Disconnect from Opik
 */
function disconnectOpik() {
  const statusIndicator = document.getElementById('opik-status-indicator');
  const statusText = document.getElementById('opik-status-text');
  const connectBtn = document.getElementById('opik-connect-btn');

  disableOpikTracing();
  opikConnected = false;

  statusIndicator.className = 'status-indicator disconnected';
  statusText.textContent = 'Disconnected';
  connectBtn.textContent = 'Connect';

  console.log('[TEA-DEMO-OPIK] Disconnected from Opik');
}

/**
 * Toggle Opik connection
 */
function toggleOpikConnection() {
  if (opikConnected) {
    disconnectOpik();
  } else {
    connectOpik();
  }
}

/**
 * Add a trace to the display list
 */
function addTraceToList(trace, status = 'sending') {
  // Store trace
  opikTraces.unshift({ ...trace, status });

  // Update UI
  renderTraceList();
}

/**
 * Update trace status in the list
 */
function updateTraceStatus(traceId, status) {
  const trace = opikTraces.find(t => t.id === traceId);
  if (trace) {
    trace.status = status;
    renderTraceList();
  }
}

/**
 * Render the trace list UI
 */
function renderTraceList() {
  const traceList = document.getElementById('trace-list');
  const traceCount = document.getElementById('trace-count');

  if (opikTraces.length === 0) {
    traceList.innerHTML = '<div class="trace-empty">No traces yet. Run a workflow to generate traces.</div>';
    traceCount.textContent = '0 traces';
    return;
  }

  traceCount.textContent = `${opikTraces.length} trace${opikTraces.length !== 1 ? 's' : ''}`;

  traceList.innerHTML = opikTraces.map(trace => {
    const duration = calculateDuration(trace.start_time, trace.end_time);
    const tokens = trace.usage?.total_tokens || 0;
    const time = formatTime(trace.start_time);

    const statusIcon = {
      sending: '⏳',
      sent: '✓',
      failed: '✗',
    }[trace.status] || '';

    return `
      <div class="trace-item ${trace.status}" data-trace-id="${trace.id}">
        <span class="trace-status-icon ${trace.status}">${statusIcon}</span>
        <span class="trace-name">${escapeHtml(trace.name)}</span>
        <span class="trace-duration">${duration}ms</span>
        ${tokens > 0 ? `<span class="trace-tokens">${tokens} tokens</span>` : ''}
        <span class="trace-time">${time}</span>
      </div>
    `;
  }).join('');

  // Add click handlers for trace detail view
  traceList.querySelectorAll('.trace-item').forEach(item => {
    item.addEventListener('click', () => {
      const traceId = item.dataset.traceId;
      showTraceDetail(traceId);
    });
  });
}

/**
 * Calculate duration between two ISO timestamps
 */
function calculateDuration(startTime, endTime) {
  try {
    const start = new Date(startTime).getTime();
    const end = new Date(endTime).getTime();
    return end - start;
  } catch {
    return 0;
  }
}

/**
 * Format timestamp for display
 */
function formatTime(isoTime) {
  try {
    const date = new Date(isoTime);
    return date.toLocaleTimeString();
  } catch {
    return '';
  }
}

/**
 * Escape HTML for safe display
 */
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

/**
 * Show trace detail panel
 */
function showTraceDetail(traceId) {
  const trace = opikTraces.find(t => t.id === traceId);
  if (!trace) return;

  const detailPanel = document.getElementById('trace-detail');
  const titleEl = document.getElementById('trace-detail-title');

  titleEl.textContent = `${trace.name} (${calculateDuration(trace.start_time, trace.end_time)}ms)`;

  // Initialize detail editors if needed
  initTraceDetailEditors();

  // Set content
  setTraceEditorContent(traceInputEditor, trace.input);
  setTraceEditorContent(traceOutputEditor, trace.output);

  // Show panel
  detailPanel.classList.remove('hidden');
  detailPanel.classList.add('visible');
}

/**
 * Hide trace detail panel
 */
function hideTraceDetail() {
  const detailPanel = document.getElementById('trace-detail');
  detailPanel.classList.remove('visible');
  setTimeout(() => {
    detailPanel.classList.add('hidden');
  }, 300);
}

/**
 * Initialize trace detail editors
 */
function initTraceDetailEditors() {
  if (traceInputEditor) return;

  const inputContainer = document.getElementById('trace-input-editor');
  const outputContainer = document.getElementById('trace-output-editor');

  if (!inputContainer || !outputContainer) return;

  traceInputEditor = new EditorView({
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
    parent: inputContainer,
  });

  traceOutputEditor = new EditorView({
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
    parent: outputContainer,
  });
}

/**
 * Set content in a trace editor
 */
function setTraceEditorContent(editor, content) {
  if (!editor) return;
  const text = typeof content === 'string' ? content : JSON.stringify(content, null, 2);
  editor.dispatch({
    changes: {
      from: 0,
      to: editor.state.doc.length,
      insert: text,
    },
  });
}

/**
 * Clear all traces
 */
function clearTraces() {
  opikTraces = [];
  renderTraceList();
}

/**
 * Restore Opik configuration from localStorage
 */
function restoreOpikConfig() {
  const apiKey = localStorage.getItem('OPIK_API_KEY');
  const projectName = localStorage.getItem('OPIK_PROJECT_NAME');

  if (apiKey) {
    document.getElementById('opik-api-key').value = apiKey;
  }
  if (projectName) {
    document.getElementById('opik-project').value = projectName;
  }
}

// ============================================================================
// Tab Navigation
// ============================================================================

/**
 * Initialize tab navigation
 */
function initTabNavigation() {
  const tabBtns = document.querySelectorAll('.tab-btn');
  const tabContents = document.querySelectorAll('.tab-content');

  tabBtns.forEach(btn => {
    btn.addEventListener('click', () => {
      const targetTab = btn.dataset.tab;

      // Update button states
      tabBtns.forEach(b => b.classList.remove('active'));
      btn.classList.add('active');

      // Update content visibility
      tabContents.forEach(content => {
        if (content.id === `tab-${targetTab}`) {
          content.classList.add('active');
        } else {
          content.classList.remove('active');
        }
      });

      // Store active tab
      localStorage.setItem('activeTab', targetTab);
    });
  });

  // Restore active tab from localStorage
  const savedTab = localStorage.getItem('activeTab');
  if (savedTab) {
    const btn = document.querySelector(`.tab-btn[data-tab="${savedTab}"]`);
    if (btn) {
      btn.click();
    }
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

    // Generate and render agent graph before execution
    const mermaidSyntax = generateMermaidFromYaml(yamlContent);
    await renderAgentGraph(mermaidSyntax);

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
    // Still try to render graph even on execution error
    try {
      const mermaidSyntax = generateMermaidFromYaml(yamlContent);
      await renderAgentGraph(mermaidSyntax);
    } catch (graphError) {
      console.warn('[TEA-DEMO] Could not render graph on error:', graphError);
    }
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

// Restore graph panel state from localStorage
restoreGraphPanelState();

// Initialize tab navigation
initTabNavigation();

// Restore Opik configuration from localStorage
restoreOpikConfig();

// Set up Opik event handlers
document.getElementById('opik-connect-btn').addEventListener('click', toggleOpikConnection);
document.getElementById('clear-traces-btn').addEventListener('click', clearTraces);
document.getElementById('close-trace-detail').addEventListener('click', hideTraceDetail);

// Allow Enter key to connect in API key field
document.getElementById('opik-api-key').addEventListener('keypress', (e) => {
  if (e.key === 'Enter' && !opikConnected) {
    connectOpik();
  }
});
