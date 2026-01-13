/**
 * TEA Report UI - TEA-REPORT-001c
 *
 * Handles UI rendering and user interactions for the TEA Bug Report viewer.
 */

// ============================================================================
// State Management
// ============================================================================

let currentReport = null;
let currentReportUrl = null;

// ============================================================================
// UI State Functions
// ============================================================================

/**
 * Show the loading state.
 */
function showLoading() {
    const loading = document.getElementById('loading');
    const error = document.getElementById('error');
    const report = document.getElementById('report');

    if (loading) loading.hidden = false;
    if (error) error.hidden = true;
    if (report) report.hidden = true;
}

/**
 * Show the error state.
 *
 * @param {string} message - Error message to display
 */
function showError(message) {
    const loading = document.getElementById('loading');
    const error = document.getElementById('error');
    const report = document.getElementById('report');

    if (loading) loading.hidden = true;
    if (error) {
        error.hidden = false;
        error.innerHTML = `
            <h2>Unable to Decode Report</h2>
            <p>${escapeHtml(message)}</p>
            <p class="help-text">
                This may happen if the URL is corrupted or incomplete.
                Please try copying the full URL from the error message.
            </p>
        `;
    }
    if (report) report.hidden = true;
}

/**
 * Show the report.
 *
 * @param {Object} reportData - Decoded report data
 */
function showReport(reportData) {
    const loading = document.getElementById('loading');
    const error = document.getElementById('error');
    const report = document.getElementById('report');

    if (loading) loading.hidden = true;
    if (error) error.hidden = true;
    if (report) report.hidden = false;

    currentReport = reportData;

    renderBadges(reportData);
    renderErrorMessage(reportData);
    renderStackTrace(reportData);
    renderExtendedContext(reportData);
    searchAndShowSimilarIssues(reportData);
}

// ============================================================================
// Rendering Functions
// ============================================================================

/**
 * Render version, platform, runtime, and error type badges.
 *
 * @param {Object} report - Decoded report
 */
function renderBadges(report) {
    const badges = document.getElementById('badges');
    if (!badges) return;

    const errorTypeLabel = window.TEADecoder
        ? window.TEADecoder.getErrorTypeLabel(report.error_type)
        : report.error_type;

    badges.innerHTML = `
        <span class="badge badge-version" title="TEA Version">v${escapeHtml(report.version)}</span>
        <span class="badge badge-platform" title="Platform">${escapeHtml(report.platform)}</span>
        <span class="badge badge-runtime" title="Runtime">${escapeHtml(report.runtime)}</span>
        <span class="badge badge-error-type" title="Error Type">${escapeHtml(errorTypeLabel)}</span>
    `;
}

/**
 * Render the error message display.
 *
 * @param {Object} report - Decoded report
 */
function renderErrorMessage(report) {
    const message = document.getElementById('message');
    if (!message) return;

    message.textContent = report.message || 'No error message';
}

/**
 * Render the stack trace display.
 *
 * @param {Object} report - Decoded report
 */
function renderStackTrace(report) {
    const stack = document.getElementById('stack');
    if (!stack) return;

    if (!report.stack || report.stack.length === 0) {
        stack.innerHTML = '<li class="stack-frame empty">No stack trace available</li>';
        return;
    }

    stack.innerHTML = report.stack.map((frame, index) => {
        const symbol = escapeHtml(frame.symbol || '<unknown>');
        const addr = frame.addr !== undefined ? `0x${frame.addr.toString(16)}` : '';

        let location = '';
        let locationLink = null;

        if (frame.file) {
            const file = escapeHtml(frame.file);
            const line = frame.line || '?';
            location = `${file}:${line}`;

            // Create GitHub source link if file path looks like project file
            if (!frame.file.startsWith('~') && !frame.file.startsWith('/')) {
                const githubOrg = window.TEAGitHub ? window.TEAGitHub.GITHUB_ORG : 'fabceolin';
                const githubRepo = window.TEAGitHub ? window.TEAGitHub.GITHUB_REPO : 'the_edge_agent';
                const lineAnchor = frame.line ? `#L${frame.line}` : '';
                locationLink = `https://github.com/${githubOrg}/${githubRepo}/blob/main/${frame.file}${lineAnchor}`;
            }
        } else if (addr) {
            location = addr;
        }

        const frameClass = index < 3 ? 'stack-frame relevant' : 'stack-frame';
        const locationHtml = locationLink
            ? `<a href="${locationLink}" target="_blank" rel="noopener" class="source-link">${location}</a>`
            : `<span class="location">${location}</span>`;

        return `
            <li class="${frameClass}">
                <span class="frame-index">${index}</span>
                <span class="symbol">${symbol}</span>
                ${location ? `<span class="at">at</span> ${locationHtml}` : ''}
            </li>
        `;
    }).join('');
}

/**
 * Render extended context section (if present).
 *
 * @param {Object} report - Decoded report
 */
function renderExtendedContext(report) {
    const extendedSection = document.getElementById('extended');
    if (!extendedSection) return;

    if (!report.extended) {
        extendedSection.hidden = true;
        return;
    }

    const ext = report.extended;
    let content = '';

    if (ext.workflow_name) {
        content += `<div class="context-item"><strong>Workflow:</strong> ${escapeHtml(ext.workflow_name)}</div>`;
    }

    if (ext.active_node) {
        content += `<div class="context-item"><strong>Active Node:</strong> ${escapeHtml(ext.active_node)}</div>`;
    }

    if (ext.active_action) {
        content += `<div class="context-item"><strong>Active Action:</strong> ${escapeHtml(ext.active_action)}</div>`;
    }

    if (ext.nodes && ext.nodes.length > 0) {
        content += `
            <div class="context-item">
                <strong>Workflow Nodes:</strong>
                <ul class="node-list">
                    ${ext.nodes.map(n => `
                        <li>
                            <span class="node-name">${escapeHtml(n.name)}</span>
                            ${n.action_type ? `<span class="node-action">(${escapeHtml(n.action_type)})</span>` : ''}
                        </li>
                    `).join('')}
                </ul>
            </div>
        `;
    }

    if (ext.schema_fields && ext.schema_fields.length > 0) {
        content += `
            <div class="context-item">
                <strong>Schema Fields:</strong>
                <span class="schema-fields">${ext.schema_fields.map(f => escapeHtml(f)).join(', ')}</span>
            </div>
        `;
    }

    if (content) {
        extendedSection.innerHTML = `<h2>Workflow Context</h2>${content}`;
        extendedSection.hidden = false;
    } else {
        extendedSection.hidden = true;
    }
}

// ============================================================================
// GitHub Integration UI
// ============================================================================

/**
 * Search for and display similar issues.
 *
 * @param {Object} report - Decoded report
 */
async function searchAndShowSimilarIssues(report) {
    const similarSection = document.getElementById('similar-issues');
    const issuesList = document.getElementById('issues-list');
    const fileIssueBtn = document.getElementById('file-issue');

    if (!similarSection || !issuesList) return;

    // Show loading state
    similarSection.hidden = false;
    issuesList.innerHTML = '<li class="loading-issues">Searching for similar issues...</li>';

    try {
        const issues = window.TEAGitHub
            ? await window.TEAGitHub.findSimilarIssues(report)
            : [];

        if (issues.length > 0) {
            issuesList.innerHTML = issues.map(issue => `
                <li class="issue-item">
                    <a href="${issue.html_url}" target="_blank" rel="noopener">
                        <span class="issue-number">#${issue.number}</span>
                        <span class="issue-title">${escapeHtml(issue.title)}</span>
                        <span class="issue-state ${issue.state}">${issue.state}</span>
                    </a>
                </li>
            `).join('');

            // Update file issue button text
            if (fileIssueBtn) {
                fileIssueBtn.textContent = 'File New Issue';
            }
        } else {
            similarSection.hidden = true;
            if (fileIssueBtn) {
                fileIssueBtn.textContent = 'File Issue on GitHub';
            }
        }
    } catch (error) {
        console.error('Error searching for similar issues:', error);
        similarSection.hidden = true;
    }
}

/**
 * Handle "File Issue" button click.
 */
function handleFileIssue() {
    if (!currentReport) return;

    if (window.TEAGitHub) {
        window.TEAGitHub.openGitHubIssue(currentReport, currentReportUrl);
    } else {
        console.error('GitHub module not loaded');
    }
}

/**
 * Handle "Copy Report" button click.
 */
async function handleCopyReport() {
    if (!currentReport) return;

    const text = generateReportText(currentReport);

    try {
        await navigator.clipboard.writeText(text);
        showCopySuccess();
    } catch (error) {
        // Fallback for older browsers
        const textarea = document.createElement('textarea');
        textarea.value = text;
        textarea.style.position = 'fixed';
        textarea.style.opacity = '0';
        document.body.appendChild(textarea);
        textarea.select();

        try {
            document.execCommand('copy');
            showCopySuccess();
        } catch (e) {
            alert('Unable to copy to clipboard. Please select and copy manually.');
        }

        document.body.removeChild(textarea);
    }
}

/**
 * Show copy success feedback.
 */
function showCopySuccess() {
    const copyBtn = document.getElementById('copy-report');
    if (!copyBtn) return;

    const originalText = copyBtn.textContent;
    copyBtn.textContent = 'Copied!';
    copyBtn.classList.add('success');

    setTimeout(() => {
        copyBtn.textContent = originalText;
        copyBtn.classList.remove('success');
    }, 2000);
}

/**
 * Generate plain text report for copying.
 *
 * @param {Object} report - Decoded report
 * @returns {string} Plain text report
 */
function generateReportText(report) {
    const lines = [
        '=== TEA Bug Report ===',
        '',
        `Version: ${report.version}`,
        `Platform: ${report.platform}`,
        `Runtime: ${report.runtime}`,
        `Error Type: ${report.error_type}`,
        '',
        '--- Error ---',
        report.message,
        '',
        '--- Stack Trace ---'
    ];

    if (report.stack && report.stack.length > 0) {
        report.stack.forEach((frame, i) => {
            const symbol = frame.symbol || '<unknown>';
            let location = '';
            if (frame.file) {
                location = ` at ${frame.file}:${frame.line || '?'}`;
            } else if (frame.addr !== undefined) {
                location = ` at 0x${frame.addr.toString(16)}`;
            }
            lines.push(`${i}: ${symbol}${location}`);
        });
    } else {
        lines.push('No stack trace available');
    }

    if (report.context) {
        lines.push('', '--- Context ---');
        if (report.context.node_name) lines.push(`Node: ${report.context.node_name}`);
        if (report.context.action_type) lines.push(`Action: ${report.context.action_type}`);
    }

    if (report.extended) {
        lines.push('', '--- Workflow Context ---');
        if (report.extended.workflow_name) lines.push(`Workflow: ${report.extended.workflow_name}`);
        if (report.extended.active_node) lines.push(`Active Node: ${report.extended.active_node}`);
    }

    return lines.join('\n');
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Escape HTML special characters.
 *
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeHtml(str) {
    if (!str) return '';
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
}

// ============================================================================
// Initialization
// ============================================================================

/**
 * Initialize the report viewer.
 *
 * @param {string} urlOrPath - URL or path containing encoded report
 */
function initReportViewer(urlOrPath) {
    showLoading();

    // Store the full URL for issue body generation
    currentReportUrl = window.location.href;

    // Small delay to allow DOM to update with loading state
    setTimeout(() => {
        try {
            if (!window.TEADecoder) {
                throw new Error('Decoder module not loaded');
            }

            const report = window.TEADecoder.decodeReport(urlOrPath);
            showReport(report);
        } catch (error) {
            console.error('Failed to decode report:', error);
            showError(error.message);
        }
    }, 50);
}

/**
 * Set up event listeners when DOM is ready.
 */
function setupEventListeners() {
    const fileIssueBtn = document.getElementById('file-issue');
    const copyReportBtn = document.getElementById('copy-report');

    if (fileIssueBtn) {
        fileIssueBtn.addEventListener('click', handleFileIssue);
    }

    if (copyReportBtn) {
        copyReportBtn.addEventListener('click', handleCopyReport);
    }
}

// Set up listeners when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', setupEventListeners);
} else {
    setupEventListeners();
}

// ============================================================================
// Exports
// ============================================================================

if (typeof window !== 'undefined') {
    window.TEAUI = {
        showLoading,
        showError,
        showReport,
        initReportViewer,
        escapeHtml,
        generateReportText,
        handleFileIssue,
        handleCopyReport
    };
}
