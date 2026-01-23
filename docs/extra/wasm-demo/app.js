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

// ============================================================================
// Tab Navigation
// ============================================================================

/**
 * Initialize tab navigation
 */
function initTabNavigation() {
  const tabs = document.querySelectorAll('.tab');
  const tabContents = document.querySelectorAll('.tab-content');

  tabs.forEach(tab => {
    tab.addEventListener('click', () => {
      const targetTab = tab.dataset.tab;

      // Update active tab
      tabs.forEach(t => t.classList.remove('active'));
      tab.classList.add('active');

      // Show target content, hide others
      tabContents.forEach(content => {
        if (content.id === `${targetTab}-tab`) {
          content.classList.remove('hidden');
        } else {
          content.classList.add('hidden');
        }
      });

      // Notify game module when game tab is selected
      if (targetTab === 'game' && window.gameUI) {
        window.gameUI.onTabActivated();
      }

      console.log(`[TEA-DEMO] Switched to ${targetTab} tab`);
    });
  });
}

// Make tab navigation available globally
window.initTabNavigation = initTabNavigation;

// Import from the bundled package
import {
  initLlm,
  initTeaLlm,
  initYamlEngine,
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
      console.log('[TEA-DEMO] Prolog query:', code);
      console.log('[TEA-DEMO] Prolog facts length:', facts ? facts.length : 0);
      try {
        // Consult facts if provided
        if (facts) {
          await prologEngine.consultText(facts);
        }

        // Run query and collect results
        const query = prologEngine.query(code);
        const bindings = [];

        for await (const result of query) {
          console.log('[TEA-DEMO] Prolog result:', JSON.stringify(result));
          // Trealla returns { status: 'success', answer: {VarName: value} }
          // We need to collect the answer object with variable bindings
          if (result.status === 'success') {
            if (result.answer && typeof result.answer === 'object') {
              // Push the binding object (e.g., {H: "paris"})
              bindings.push(result.answer);
            } else if (result.answer) {
              // Handle non-object answers (shouldn't happen but be defensive)
              bindings.push({ value: result.answer });
            }
          }
        }

        console.log('[TEA-DEMO] Prolog bindings collected:', bindings.length);
        return JSON.stringify({ bindings, success: true });
      } catch (e) {
        console.error('[TEA-DEMO] Prolog error:', e);
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

// ============================================================================
// Example YAML Workflows
// ============================================================================

const EXAMPLES = {
  // ---------------------------------------------------------------------------
  // Example 1: Tera Template Filters (No LLM Required)
  // Demonstrates: {{ state.var }}, filters (upper, lower, tojson, length, default)
  // ---------------------------------------------------------------------------
  'tera-templates': {
    name: 'Tera Templates & Filters',
    description: 'Demonstrates Jinja2-like template syntax with filters',
    requiresLlm: false,
    yaml: `name: template-filters-demo
# Demonstrates Tera template engine (Jinja2-compatible)
# Features:
#   - Variable interpolation: {{ state.key }}
#   - Filters: upper, lower, tojson, length, default
#   - Nested access: {{ state.user.name }}
#   - No LLM required - runs instantly!

nodes:
  # Step 1: Transform text with filters
  - name: text_transforms
    action: return
    with:
      value:
        uppercase: "{{ state.name | upper }}"
        lowercase: "{{ state.name | lower }}"
        with_default: "{{ state.missing | default(value='N/A') }}"
    output: transforms

  # Step 2: Work with data structures
  - name: data_ops
    action: return
    with:
      value:
        user_json: "{{ state.user | tojson }}"
        item_count: "{{ state.items | length }}"
        greeting: "Hello, {{ state.user.name }}!"
    output: data

  # Step 3: Combine results
  - name: summary
    action: return
    with:
      value:
        message: "Processed {{ state.items | length }} items for {{ state.user.name | upper }}"
        all_transforms: "{{ state.transforms | tojson }}"
    output: summary`,
    state: `name: Alice
user:
  name: Bob
  email: bob@example.com
items:
  - apple
  - banana
  - cherry`,
  },

  // ---------------------------------------------------------------------------
  // Example 2: Conditional Routing with goto/when (No LLM Required)
  // Demonstrates: goto with when conditions, score-based routing
  // ---------------------------------------------------------------------------
  'conditional-routing': {
    name: 'Conditional Routing',
    description: 'Demonstrates goto with when conditions for branching workflows',
    requiresLlm: false,
    yaml: `name: conditional-routing-demo
# Demonstrates conditional edge routing with goto/when
# Features:
#   - goto: simple unconditional jumps
#   - goto with when: conditional branching
#   - Multiple conditions evaluated in order
#   - Score-based routing logic
#   - No LLM required - runs instantly!

variables:
  threshold_high: 80
  threshold_low: 40

nodes:
  # Entry point: evaluate the score
  - name: evaluate
    action: return
    with:
      value:
        score: "{{ state.score }}"
        evaluated: true
    output: evaluation
    # Conditional routing based on score (using if/to syntax like Rust)
    goto:
      - if: state.score >= 80
        to: high_tier
      - if: state.score >= 40
        to: mid_tier
      - to: low_tier  # Default fallback

  # High tier path (score >= 80)
  - name: high_tier
    action: return
    with:
      value:
        tier: "premium"
        discount: 20
        message: "Premium tier! Score: {{ state.score }}"
    output: result
    goto: __end__

  # Mid tier path (40 <= score < 80)
  - name: mid_tier
    action: return
    with:
      value:
        tier: "standard"
        discount: 10
        message: "Standard tier. Score: {{ state.score }}"
    output: result
    goto: __end__

  # Low tier path (score < 40)
  - name: low_tier
    action: return
    with:
      value:
        tier: "basic"
        discount: 0
        message: "Basic tier. Score: {{ state.score }}. Keep trying!"
    output: result
    goto: __end__`,
    state: `score: 75`,
  },

  // ---------------------------------------------------------------------------
  // Example 3: Variables & Complex Expressions (No LLM Required)
  // Demonstrates: settings.variables, nested templates, loops
  // ---------------------------------------------------------------------------
  'variables-expressions': {
    name: 'Variables & Loops',
    description: 'Demonstrates variables, nested templates, and loop iteration',
    requiresLlm: false,
    yaml: `name: variables-loops-demo
# Demonstrates advanced template features
# Features:
#   - Top-level variables for reusable values
#   - Loop iteration with {% for %}
#   - Conditional blocks with {% if %}
#   - Nested template expressions
#   - No LLM required - runs instantly!

variables:
  app_name: "TEA Demo"
  version: "1.0.0"
  max_items: 5

nodes:
  # Generate a formatted list using loop
  - name: format_list
    action: return
    with:
      value:
        header: "{{ variables.app_name }} v{{ variables.version }}"
        formatted: |
          {% for item in state.products %}
          - {{ item.name }}: {{ item.price }} USD
          {% endfor %}
    output: list

  # Calculate totals and summary
  - name: calculate_summary
    action: return
    with:
      value:
        item_count: "{{ state.products | length }}"
        status: |
          {% if state.products | length > 3 %}
          Large order ({{ state.products | length }} items)
          {% else %}
          Small order
          {% endif %}
    output: summary

  # Final output with all data
  - name: finalize
    action: return
    with:
      value:
        app: "{{ variables.app_name }}"
        version: "{{ variables.version }}"
        items: "{{ state.list.formatted }}"
        order_status: "{{ state.summary.status }}"
    output: final`,
    state: `products:
  - name: Widget
    price: 29.99
  - name: Gadget
    price: 49.99
  - name: Gizmo
    price: 19.99
  - name: Doohickey
    price: 39.99`,
  },

  // ---------------------------------------------------------------------------
  // Example 4: Simulated Parallel Execution (No LLM Required)
  // Demonstrates: Fan-out pattern with multiple paths
  // ---------------------------------------------------------------------------
  'parallel-fanout': {
    name: 'Parallel Fan-Out',
    description: 'Demonstrates simulated parallel execution with fan-out pattern',
    requiresLlm: false,
    yaml: `name: parallel-fanout-demo
# Demonstrates simulated parallel execution
# Features:
#   - Fan-out: single node spawns multiple parallel paths
#   - Fan-in: collect results from parallel paths
#   - Merge strategies: isolated, merge_deep, last_write_wins
#   - No LLM required - runs instantly!
#
# Note: In WASM, parallel execution is simulated sequentially
# but maintains correct fan-in semantics.

settings:
  parallel:
    strategy: merge_deep  # Merge parallel results

nodes:
  # Entry point - will fan out to analyzers
  - name: start
    action: return
    with:
      value:
        input: "{{ state.text }}"
        started_at: "now"
    output: metadata

  # Parallel path 1: Word analysis
  - name: word_analyzer
    action: return
    with:
      value:
        text_sample: "{{ state.text | upper }}"
        type: "word_analysis"
    output: analysis.words

  # Parallel path 2: Character analysis
  - name: char_analyzer
    action: return
    with:
      value:
        char_count: "{{ state.text | length }}"
        type: "char_analysis"
    output: analysis.chars

  # Parallel path 3: Pattern detection
  - name: pattern_detector
    action: return
    with:
      value:
        has_numbers: "{{ state.text | lower }}"
        type: "pattern_analysis"
    output: analysis.patterns

  # Fan-in: Collect all parallel results
  - name: aggregate
    action: return
    with:
      value:
        summary: "Analysis complete"
        word_result: "{{ state.analysis.words | tojson }}"
        char_result: "{{ state.analysis.chars | tojson }}"
    output: final

edges:
  - from: __start__
    to: start
  # Fan-out edges (start spawns parallel analyzers)
  - from: start
    to: word_analyzer
  - from: start
    to: char_analyzer
  - from: start
    to: pattern_detector
  # Fan-in edges (all analyzers feed into aggregate)
  - from: word_analyzer
    to: aggregate
  - from: char_analyzer
    to: aggregate
  - from: pattern_detector
    to: aggregate
  - from: aggregate
    to: __end__`,
    state: `text: "Hello World from TEA WASM 2024!"`,
  },

  // ---------------------------------------------------------------------------
  // Example 5: Neurosymbolic QA (Requires LLM + Lua + Prolog)
  // The original robust workflow with knowledge base validation
  // ---------------------------------------------------------------------------
  'neurosymbolic-qa': {
    name: 'Neurosymbolic Q&A (LLM)',
    description: 'Advanced: LLM + Lua + Prolog for verified knowledge retrieval',
    requiresLlm: true,
    yaml: `name: robust-qa-workflow
# Demonstrates: Robust answer extraction from small quantized models
# The workflow uses Lua normalization + Prolog validation to ensure
# consistent results even when LLM output format varies.
#
# Features:
# - Complete world capitals database (195 countries)
# - Case-insensitive question matching
# - Longest match priority (avoids "car" matching in "madagascar")
# - Multi-pattern answer extraction from unreliable LLM outputs
# - Confidence scoring: verified, corrected, fallback

nodes:
  # ===========================================================================
  # Step 1: Analyze question structure with Lua
  # ===========================================================================
  - name: analyze_question
    action: lua.eval
    with:
      code: |
        local q = question or ""
        local words = 0
        for _ in string.gmatch(q, "%S+") do
          words = words + 1
        end

        -- Detect question type via patterns
        local qtype = "general"
        local patterns = {
          { pattern = "capital", type = "geography" },
          { pattern = "country", type = "geography" },
          { pattern = "city", type = "geography" },
          { pattern = "where", type = "geography" },
          { pattern = "calculate", type = "math" },
          { pattern = "sum", type = "math" },
          { pattern = "how many", type = "math" },
          { pattern = "why", type = "science" },
          { pattern = "how does", type = "science" },
          { pattern = "what is", type = "definition" },
          { pattern = "who is", type = "biography" },
        }

        local lower_q = q:lower()
        for _, p in ipairs(patterns) do
          if lower_q:find(p.pattern) then
            qtype = p.type
            break
          end
        end

        return {
          word_count = words,
          question_type = qtype,
          is_short = words < 10
        }

  # ===========================================================================
  # Step 2: Use Prolog for knowledge-based hints (195 countries)
  # ===========================================================================
  - name: prolog_hints
    action: prolog.query
    with:
      code: "hint(H)"
      facts: |
        % =========================================================
        % COMPLETE WORLD CAPITALS DATABASE (195 countries)
        % =========================================================

        % EUROPE (44 countries)
        capital(albania, tirana).
        capital(andorra, andorra_la_vella).
        capital(austria, vienna).
        capital(belarus, minsk).
        capital(belgium, brussels).
        capital(bosnia, sarajevo).
        capital(bulgaria, sofia).
        capital(croatia, zagreb).
        capital(cyprus, nicosia).
        capital(czech, prague).
        capital(czechia, prague).
        capital(denmark, copenhagen).
        capital(estonia, tallinn).
        capital(finland, helsinki).
        capital(france, paris).
        capital(germany, berlin).
        capital(greece, athens).
        capital(hungary, budapest).
        capital(iceland, reykjavik).
        capital(ireland, dublin).
        capital(italy, rome).
        capital(kosovo, pristina).
        capital(latvia, riga).
        capital(liechtenstein, vaduz).
        capital(lithuania, vilnius).
        capital(luxembourg, luxembourg).
        capital(malta, valletta).
        capital(moldova, chisinau).
        capital(monaco, monaco).
        capital(montenegro, podgorica).
        capital(netherlands, amsterdam).
        capital(holland, amsterdam).
        capital(macedonia, skopje).
        capital(norway, oslo).
        capital(poland, warsaw).
        capital(portugal, lisbon).
        capital(romania, bucharest).
        capital(russia, moscow).
        capital(sanmarino, san_marino).
        capital(serbia, belgrade).
        capital(slovakia, bratislava).
        capital(slovenia, ljubljana).
        capital(spain, madrid).
        capital(sweden, stockholm).
        capital(switzerland, bern).
        capital(ukraine, kyiv).
        capital(uk, london).
        capital(britain, london).
        capital(england, london).
        capital(vatican, vatican_city).

        % ASIA (48 countries)
        capital(afghanistan, kabul).
        capital(armenia, yerevan).
        capital(azerbaijan, baku).
        capital(bahrain, manama).
        capital(bangladesh, dhaka).
        capital(bhutan, thimphu).
        capital(brunei, bandar_seri_begawan).
        capital(cambodia, phnom_penh).
        capital(china, beijing).
        capital(georgia, tbilisi).
        capital(india, new_delhi).
        capital(indonesia, jakarta).
        capital(iran, tehran).
        capital(iraq, baghdad).
        capital(israel, jerusalem).
        capital(japan, tokyo).
        capital(jordan, amman).
        capital(kazakhstan, astana).
        capital(kuwait, kuwait_city).
        capital(kyrgyzstan, bishkek).
        capital(laos, vientiane).
        capital(lebanon, beirut).
        capital(malaysia, kuala_lumpur).
        capital(maldives, male).
        capital(mongolia, ulaanbaatar).
        capital(myanmar, naypyidaw).
        capital(burma, naypyidaw).
        capital(nepal, kathmandu).
        capital(northkorea, pyongyang).
        capital(oman, muscat).
        capital(pakistan, islamabad).
        capital(palestine, ramallah).
        capital(philippines, manila).
        capital(qatar, doha).
        capital(saudiarabia, riyadh).
        capital(saudi, riyadh).
        capital(singapore, singapore).
        capital(southkorea, seoul).
        capital(korea, seoul).
        capital(srilanka, colombo).
        capital(syria, damascus).
        capital(taiwan, taipei).
        capital(tajikistan, dushanbe).
        capital(thailand, bangkok).
        capital(timorleste, dili).
        capital(turkey, ankara).
        capital(turkmenistan, ashgabat).
        capital(uae, abu_dhabi).
        capital(emirates, abu_dhabi).
        capital(uzbekistan, tashkent).
        capital(vietnam, hanoi).
        capital(yemen, sanaa).

        % AFRICA (54 countries)
        capital(algeria, algiers).
        capital(angola, luanda).
        capital(benin, porto_novo).
        capital(botswana, gaborone).
        capital(burkinafaso, ouagadougou).
        capital(burundi, gitega).
        capital(cameroon, yaounde).
        capital(capeverde, praia).
        capital(car, bangui).
        capital(chad, ndjamena).
        capital(comoros, moroni).
        capital(congo, brazzaville).
        capital(drc, kinshasa).
        capital(djibouti, djibouti).
        capital(egypt, cairo).
        capital(equatorialguinea, malabo).
        capital(eritrea, asmara).
        capital(eswatini, mbabane).
        capital(swaziland, mbabane).
        capital(ethiopia, addis_ababa).
        capital(gabon, libreville).
        capital(gambia, banjul).
        capital(ghana, accra).
        capital(guinea, conakry).
        capital(guineabissau, bissau).
        capital(ivorycoast, yamoussoukro).
        capital(kenya, nairobi).
        capital(lesotho, maseru).
        capital(liberia, monrovia).
        capital(libya, tripoli).
        capital(madagascar, antananarivo).
        capital(malawi, lilongwe).
        capital(mali, bamako).
        capital(mauritania, nouakchott).
        capital(mauritius, port_louis).
        capital(morocco, rabat).
        capital(mozambique, maputo).
        capital(namibia, windhoek).
        capital(niger, niamey).
        capital(nigeria, abuja).
        capital(rwanda, kigali).
        capital(saotome, sao_tome).
        capital(senegal, dakar).
        capital(seychelles, victoria).
        capital(sierraleone, freetown).
        capital(somalia, mogadishu).
        capital(southafrica, pretoria).
        capital(southsudan, juba).
        capital(sudan, khartoum).
        capital(tanzania, dodoma).
        capital(togo, lome).
        capital(tunisia, tunis).
        capital(uganda, kampala).
        capital(zambia, lusaka).
        capital(zimbabwe, harare).

        % NORTH AMERICA (23 countries)
        capital(antiguabarbuda, saint_johns).
        capital(bahamas, nassau).
        capital(barbados, bridgetown).
        capital(belize, belmopan).
        capital(canada, ottawa).
        capital(costarica, san_jose).
        capital(cuba, havana).
        capital(dominica, roseau).
        capital(dominicanrepublic, santo_domingo).
        capital(elsalvador, san_salvador).
        capital(grenada, saint_georges).
        capital(guatemala, guatemala_city).
        capital(haiti, port_au_prince).
        capital(honduras, tegucigalpa).
        capital(jamaica, kingston).
        capital(mexico, mexico_city).
        capital(nicaragua, managua).
        capital(panama, panama_city).
        capital(stkitts, basseterre).
        capital(stlucia, castries).
        capital(stvincent, kingstown).
        capital(trinidadtobago, port_of_spain).
        capital(trinidad, port_of_spain).
        capital(usa, washington_dc).
        capital(america, washington_dc).
        capital(unitedstates, washington_dc).

        % SOUTH AMERICA (12 countries)
        capital(argentina, buenos_aires).
        capital(bolivia, sucre).
        capital(brazil, brasilia).
        capital(brasil, brasilia).
        capital(chile, santiago).
        capital(colombia, bogota).
        capital(ecuador, quito).
        capital(guyana, georgetown).
        capital(paraguay, asuncion).
        capital(peru, lima).
        capital(suriname, paramaribo).
        capital(uruguay, montevideo).
        capital(venezuela, caracas).

        % OCEANIA (14 countries)
        capital(australia, canberra).
        capital(fiji, suva).
        capital(kiribati, tarawa).
        capital(marshallislands, majuro).
        capital(micronesia, palikir).
        capital(nauru, yaren).
        capital(newzealand, wellington).
        capital(palau, ngerulmud).
        capital(papuanewguinea, port_moresby).
        capital(samoa, apia).
        capital(solomonislands, honiara).
        capital(tonga, nukualofa).
        capital(tuvalu, funafuti).
        capital(vanuatu, port_vila).

        % =========================================================
        % HINT PREDICATE WITH LONGEST MATCH PRIORITY
        % Avoids false positives like "car" matching in "madagascar"
        % =========================================================

        % Helper: get atom length
        atom_len(Atom, Len) :-
          atom_codes(Atom, Codes),
          length(Codes, Len).

        % Helper: lowercase an atom (Trealla-compatible, no maplist)
        lower_atom(Atom, Lower) :-
          atom_codes(Atom, Codes),
          lower_codes(Codes, LowerCodes),
          atom_codes(Lower, LowerCodes).

        % Recursive lowercase for code list (replaces maplist)
        lower_codes([], []).
        lower_codes([C|Cs], [L|Ls]) :-
          to_lower_code(C, L),
          lower_codes(Cs, Ls).

        to_lower_code(C, L) :-
          C >= 65, C =< 90, !, L is C + 32.
        to_lower_code(C, C).

        % Find matching country and return its capital
        % Uses simple first-match (countries are ordered by name length implicitly)
        hint(Answer) :-
          lower_atom('{{ state.question }}', LowerQ),
          capital(Country, Answer),
          lower_atom(Country, LowerCountry),
          sub_atom(LowerQ, _, _, _, LowerCountry),
          !.  % Cut to return first match

        % Fallback if no match found
        hint(unknown) :-
          lower_atom('{{ state.question }}', LowerQ),
          \+ (
            capital(Country, _),
            lower_atom(Country, LowerCountry),
            sub_atom(LowerQ, _, _, _, LowerCountry)
          ).

  # ===========================================================================
  # Step 3: LLM generates response with structured format
  # ===========================================================================
  - name: llm_think
    action: llm.call
    with:
      prompt: |
        Answer this question directly and concisely.

        Question: {{ state.question }}

        INSTRUCTIONS:
        - Give ONLY the direct answer
        - Do NOT include explanations unless asked
        - For geography questions, just state the answer (e.g., "Paris")
        - Keep your response under 50 words

        Answer:
      max_tokens: 80
      temperature: 0.1

  # ===========================================================================
  # Step 4: Lua extracts and normalizes the answer
  # ===========================================================================
  - name: extract_answer
    action: lua.eval
    with:
      code: |
        local response = state.llm_think or {}
        local content = response.content or ""
        local prolog_result = state.prolog_result or {}
        local bindings = prolog_result.bindings or {}
        local qtype = state.lua_result and state.lua_result.question_type or "general"

        -- EXTRACTION PIPELINE
        local extracted = content

        -- 1. Remove common LLM prefixes
        local prefixes = {
          "^The answer is:?%s*",
          "^Answer:?%s*",
          "^The capital of %w+ is%s*",
          "^The capital is%s*",
          "^It is%s*",
          "^It's%s*",
          "^That would be%s*",
          "^Based on.-,%s*",
        }
        for _, prefix in ipairs(prefixes) do
          extracted = extracted:gsub(prefix, "")
        end

        -- 2. Remove trailing punctuation and whitespace
        extracted = extracted:gsub("[%.!?]+%s*$", "")
        extracted = extracted:gsub("^%s+", ""):gsub("%s+$", "")

        -- 3. Extract first sentence if multiple sentences
        local first_sentence = extracted:match("^([^%.!?]+)")
        if first_sentence and #first_sentence > 0 then
          extracted = first_sentence:gsub("^%s+", ""):gsub("%s+$", "")
        end

        -- 4. For geography, try to extract just the city name
        if qtype == "geography" then
          -- Look for capitalized words (proper nouns)
          local proper_noun = extracted:match("([A-Z][a-z]+)")
          if proper_noun then
            extracted = proper_noun
          end
        end

        -- 5. VALIDATION: Check against Prolog knowledge base
        local validated = extracted
        local confidence = "llm"

        -- Check if Prolog found an answer
        for _, binding in ipairs(bindings) do
          local hint = binding.H or binding
          -- Handle Trealla atom format: {functor: "paris", args: []}
          -- In wasmoon, JS objects may not be type "table", so check functor directly
          if hint and hint.functor then
            hint = hint.functor
          end
          if hint and tostring(hint) ~= "unknown" then
            -- Prolog has authoritative answer
            -- Convert underscore to space and capitalize
            local prolog_answer = tostring(hint)
            prolog_answer = prolog_answer:gsub("_", " ")
            -- Capitalize each word
            prolog_answer = prolog_answer:gsub("(%a)([%w]*)", function(first, rest)
              return first:upper() .. rest:lower()
            end)

            -- Use Prolog answer if LLM answer seems different
            local llm_lower = extracted:lower():gsub("%s+", "")
            local prolog_lower = prolog_answer:lower():gsub("%s+", "")

            if prolog_lower:find(llm_lower) or llm_lower:find(prolog_lower) then
              -- LLM and Prolog agree
              validated = prolog_answer
              confidence = "verified"
            else
              -- Prefer Prolog for known facts
              validated = prolog_answer
              confidence = "corrected"
            end
            break
          end
        end

        -- 6. Fallback if extraction failed
        if #validated == 0 then
          -- Return first 100 chars of original
          validated = content:sub(1, 100)
          confidence = "fallback"
        end

        return {
          final_answer = validated,
          confidence = confidence,
          raw_llm = content,
          question_type = qtype
        }

  # ===========================================================================
  # Step 5: Format final response with metadata
  # ===========================================================================
  - name: format_output
    action: lua.eval
    with:
      code: |
        local answer = state.lua_result and state.lua_result.final_answer or "Unknown"
        local confidence = state.lua_result and state.lua_result.confidence or "unknown"
        local qtype = state.lua_result and state.lua_result.question_type or "general"

        -- Build confidence indicator
        local indicator = ""
        if confidence == "verified" then
          indicator = " [verified]"
        elseif confidence == "corrected" then
          indicator = " [from knowledge base]"
        elseif confidence == "fallback" then
          indicator = " [uncertain]"
        end

        return {
          answer = answer .. indicator,
          metadata = {
            confidence = confidence,
            question_type = qtype,
            source = confidence == "corrected" and "prolog" or "llm"
          }
        }`,
    state: `question: What is the capital of France?`,
  },
};

// Default example to show on load
const DEFAULT_EXAMPLE = 'neurosymbolic-qa';

// Get default YAML and state from examples
const DEFAULT_YAML = EXAMPLES[DEFAULT_EXAMPLE].yaml;
const DEFAULT_STATE_YAML = EXAMPLES[DEFAULT_EXAMPLE].state;

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

// Set YAML editor content
function setYamlContent(content) {
  yamlEditor.dispatch({
    changes: {
      from: 0,
      to: yamlEditor.state.doc.length,
      insert: content,
    },
  });
}

// Set state editor content
function setStateContent(content) {
  stateEditor.dispatch({
    changes: {
      from: 0,
      to: stateEditor.state.doc.length,
      insert: content,
    },
  });
}

// Load example by key
function loadExample(exampleKey) {
  const example = EXAMPLES[exampleKey];
  if (!example) {
    console.warn(`[TEA-DEMO] Example not found: ${exampleKey}`);
    return;
  }

  setYamlContent(example.yaml);
  setStateContent(example.state);

  // Update description display
  const descEl = document.getElementById('example-description');
  if (descEl) {
    descEl.textContent = example.description;
    if (example.requiresLlm) {
      descEl.innerHTML += ' <span class="llm-badge">LLM will run in your browser via CPU</span>';
    }
  }

  // Clear previous output
  setOutputContent('');
  const outputContainer = document.getElementById('yaml-output');
  if (outputContainer) {
    outputContainer.classList.add('hidden');
  }

  // Regenerate graph preview
  const mermaidSyntax = generateMermaidFromYaml(example.yaml);
  renderAgentGraph(mermaidSyntax);

  console.log(`[TEA-DEMO] Loaded example: ${example.name}`);
}

// Populate example selector dropdown
function populateExampleSelector() {
  const selector = document.getElementById('example-selector');
  if (!selector) return;

  // Clear existing options
  selector.innerHTML = '';

  // Add options for each example
  for (const [key, example] of Object.entries(EXAMPLES)) {
    const option = document.createElement('option');
    option.value = key;
    option.textContent = example.name;
    if (key === DEFAULT_EXAMPLE) {
      option.selected = true;
    }
    selector.appendChild(option);
  }

  // Add change handler
  selector.addEventListener('change', (e) => {
    loadExample(e.target.value);
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
  let yamlEngineReady = false;

  // Step 1: Initialize YAML engine first (independent of LLM)
  try {
    updateStatus('Initializing YAML engine...', 'loading');
    showProgress(true);
    updateThreadingStatus();

    await initYamlEngine({ verbose: true });
    yamlEngineReady = true;
    runYamlBtn.disabled = false;
    console.log('[TEA-DEMO] YAML engine initialized, non-LLM workflows ready');
    updateStatus('YAML Ready (loading LLM...)', 'ready');
  } catch (error) {
    console.error('YAML engine initialization failed:', error);
    updateStatus(`Error: ${error.message}`, 'error');
    showProgress(false);
    return; // Can't continue without YAML engine
  }

  // Step 2: Initialize additional engines (non-blocking)
  Promise.all([initLuaEngine(), initPrologEngine()]).then(([lua, prolog]) => {
    const engines = [];
    if (lua) engines.push('Lua');
    if (prolog) engines.push('Prolog');
    if (engines.length > 0) {
      console.log(`[TEA-DEMO] Additional engines: ${engines.join(', ')}`);
    }
  });

  // Step 3: Try to load LLM (may fail, but YAML workflows will still work)
  try {
    updateStatus('Loading LLM model...', 'loading');

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
        updateStatus('Ready (with LLM)', 'ready');
        showProgress(false);
      },
    });

    // Register LLM handler for llm.call actions
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
    console.error('LLM loading failed (YAML workflows still available):', error);
    updateStatus(`YAML Ready (LLM unavailable: ${error.message})`, 'ready');
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

// Initialize editors first, then example selector, tabs, then LLM
initEditors();
populateExampleSelector();
initTabNavigation();
initializeLlm();

// Restore graph panel state from localStorage
restoreGraphPanelState();
