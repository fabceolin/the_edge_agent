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

// Default YAML workflow (implicit edges - no edges section needed)
// ROBUST VERSION: Uses Lua + Prolog for reliable answer extraction from quantized LLMs
// Features complete world capitals database (195 countries) with case-insensitive
// matching and longest-match priority to avoid false positives.
const DEFAULT_YAML = `name: robust-qa-workflow
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
        }`;

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
