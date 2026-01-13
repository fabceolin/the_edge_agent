/**
 * TEA Report GitHub Integration - TEA-REPORT-001c
 *
 * Provides GitHub issue search and filing functionality for the
 * TEA Bug Report viewer.
 */

// ============================================================================
// Configuration
// ============================================================================

const GITHUB_ORG = 'fabceolin';
const GITHUB_REPO = 'the_edge_agent';
const GITHUB_API_BASE = 'https://api.github.com';

// ============================================================================
// Issue Search
// ============================================================================

/**
 * Search for similar issues on GitHub.
 *
 * Uses the GitHub Search API (unauthenticated, subject to rate limits).
 * Searches by error message keywords and error type.
 *
 * @param {Object} report - Decoded error report
 * @returns {Promise<Array>} Array of similar issues (may be empty)
 */
async function searchSimilarIssues(report) {
    // Extract key terms from error message (skip short words)
    const keywords = report.message
        .split(/[\s:]+/)
        .filter(w => w.length > 3 && !w.match(/^[\d.]+$/))
        .slice(0, 5)
        .join(' ');

    if (!keywords.trim()) {
        return [];
    }

    // Build search query
    const errorType = report.error_type || 'Error';
    const queryTerms = [
        `repo:${GITHUB_ORG}/${GITHUB_REPO}`,
        'is:issue',
        keywords,
        // Also search for the error type label
        `"[${errorType}]"`
    ].join(' ');

    const query = encodeURIComponent(queryTerms);

    try {
        const response = await fetch(
            `${GITHUB_API_BASE}/search/issues?q=${query}&per_page=5`,
            {
                headers: {
                    'Accept': 'application/vnd.github.v3+json'
                }
            }
        );

        if (!response.ok) {
            // Handle rate limiting gracefully
            if (response.status === 403) {
                console.warn('GitHub API rate limit exceeded');
                return [];
            }
            throw new Error(`GitHub API error: ${response.status}`);
        }

        const data = await response.json();
        return data.items || [];
    } catch (error) {
        console.error('Failed to search GitHub issues:', error);
        return [];
    }
}

/**
 * Search for issues by stack trace signature.
 *
 * Creates a signature from the top stack frames and searches for it.
 *
 * @param {Object} report - Decoded error report
 * @returns {Promise<Array>} Array of similar issues
 */
async function searchByStackSignature(report) {
    if (!report.stack || report.stack.length === 0) {
        return [];
    }

    // Create signature from top 3 stack frames
    const signature = report.stack
        .slice(0, 3)
        .map(frame => frame.symbol || `0x${(frame.addr || 0).toString(16)}`)
        .join(' ');

    if (!signature.trim()) {
        return [];
    }

    const query = encodeURIComponent(
        `repo:${GITHUB_ORG}/${GITHUB_REPO} is:issue "${signature}"`
    );

    try {
        const response = await fetch(
            `${GITHUB_API_BASE}/search/issues?q=${query}&per_page=3`,
            {
                headers: {
                    'Accept': 'application/vnd.github.v3+json'
                }
            }
        );

        if (!response.ok) {
            return [];
        }

        const data = await response.json();
        return data.items || [];
    } catch (error) {
        console.error('Failed to search by stack signature:', error);
        return [];
    }
}

/**
 * Combined search for similar issues.
 *
 * Searches both by error message and stack signature,
 * de-duplicating results.
 *
 * @param {Object} report - Decoded error report
 * @returns {Promise<Array>} Array of unique similar issues
 */
async function findSimilarIssues(report) {
    const [messageResults, stackResults] = await Promise.all([
        searchSimilarIssues(report),
        searchByStackSignature(report)
    ]);

    // De-duplicate by issue number
    const seen = new Set();
    const combined = [];

    for (const issue of [...messageResults, ...stackResults]) {
        if (!seen.has(issue.number)) {
            seen.add(issue.number);
            combined.push(issue);
        }
    }

    return combined.slice(0, 5); // Limit to 5 results
}

// ============================================================================
// Issue Title & Body Generation
// ============================================================================

/**
 * Generate issue title from report.
 *
 * Format: [{ErrorType}] {message (truncated to 60 chars)}
 *
 * @param {Object} report - Decoded error report
 * @returns {string} Issue title
 */
function generateIssueTitle(report) {
    const type = report.error_type || 'Error';
    let msg = report.message || 'Unknown error';

    // Truncate message to 60 chars
    if (msg.length > 60) {
        msg = msg.slice(0, 57) + '...';
    }

    return `[${type}] ${msg}`;
}

/**
 * Format stack trace for issue body.
 *
 * @param {Array} stack - Stack frames from report
 * @returns {string} Formatted stack trace
 */
function formatStackTrace(stack) {
    if (!stack || stack.length === 0) {
        return 'No stack trace available';
    }

    return stack.map((frame, i) => {
        const symbol = frame.symbol || '<unknown>';
        let location;
        if (frame.file) {
            location = `${frame.file}:${frame.line || '?'}`;
        } else if (frame.addr !== undefined) {
            location = `0x${frame.addr.toString(16)}`;
        } else {
            location = '<unknown>';
        }
        return `${i}: ${symbol} at ${location}`;
    }).join('\n');
}

/**
 * Generate issue body from report.
 *
 * Includes:
 * - Environment section (version, platform, runtime)
 * - Error section (message, stack trace)
 * - Context section (if present)
 * - Extended context (if present)
 *
 * @param {Object} report - Decoded error report
 * @param {string} [reportUrl] - Optional URL to the report viewer
 * @returns {string} Issue body in Markdown format
 */
function generateIssueBody(report, reportUrl) {
    const sections = [];

    // Environment section
    sections.push(`## Environment

- **TEA Version**: ${report.version || 'unknown'}
- **Platform**: ${report.platform || 'unknown'}
- **Runtime**: ${report.runtime || 'unknown'}`);

    // Error section
    sections.push(`## Error

\`\`\`
${report.message || 'No message'}
\`\`\``);

    // Stack trace section
    sections.push(`## Stack Trace

\`\`\`
${formatStackTrace(report.stack)}
\`\`\``);

    // Context section (if present)
    if (report.context) {
        const ctx = report.context;
        const contextParts = [];
        if (ctx.node_name) contextParts.push(`- **Node**: ${ctx.node_name}`);
        if (ctx.action_type) contextParts.push(`- **Action**: ${ctx.action_type}`);
        if (ctx.checkpoint_id) contextParts.push(`- **Checkpoint**: ${ctx.checkpoint_id}`);

        if (contextParts.length > 0) {
            sections.push(`## Context

${contextParts.join('\n')}`);
        }
    }

    // Extended context section (if present)
    if (report.extended) {
        const ext = report.extended;
        const extParts = [];

        if (ext.workflow_name) extParts.push(`- **Workflow**: ${ext.workflow_name}`);
        if (ext.nodes && ext.nodes.length > 0) {
            extParts.push(`- **Nodes**: ${ext.nodes.map(n => n.name).join(', ')}`);
        }
        if (ext.active_node) extParts.push(`- **Active Node**: ${ext.active_node}`);
        if (ext.active_action) extParts.push(`- **Active Action**: ${ext.active_action}`);
        if (ext.schema_fields && ext.schema_fields.length > 0) {
            extParts.push(`- **Schema Fields**: ${ext.schema_fields.join(', ')}`);
        }

        if (extParts.length > 0) {
            sections.push(`## Workflow Structure

${extParts.join('\n')}`);
        }
    }

    // Footer
    let footer = `---
*Report generated by [TEA Bug Reporter](https://${GITHUB_ORG}.github.io/the_edge_agent/report/)*`;

    if (reportUrl) {
        footer += `

[View Full Report](${reportUrl})`;
    }

    sections.push(footer);

    return sections.join('\n\n');
}

// ============================================================================
// Issue URL Generation
// ============================================================================

/**
 * Generate GitHub new issue URL with pre-filled data.
 *
 * @param {Object} report - Decoded error report
 * @param {string} [reportUrl] - Optional URL to the report viewer
 * @returns {string} GitHub new issue URL
 */
function generateIssueUrl(report, reportUrl) {
    const title = encodeURIComponent(generateIssueTitle(report));
    const body = encodeURIComponent(generateIssueBody(report, reportUrl));

    // Check if URL would be too long (GitHub has limits)
    const baseUrl = `https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/issues/new`;
    const fullUrl = `${baseUrl}?title=${title}&body=${body}`;

    // GitHub has a ~8000 character limit for issue URLs
    if (fullUrl.length > 8000) {
        // Truncate body to fit
        const availableLength = 8000 - baseUrl.length - title.length - 20;
        const truncatedBody = encodeURIComponent(
            generateIssueBody(report, reportUrl).slice(0, availableLength) +
            '\n\n*[Body truncated due to URL length limits]*'
        );
        return `${baseUrl}?title=${title}&body=${truncatedBody}`;
    }

    return fullUrl;
}

/**
 * Open GitHub new issue page in a new tab.
 *
 * @param {Object} report - Decoded error report
 * @param {string} [reportUrl] - Optional URL to the report viewer
 */
function openGitHubIssue(report, reportUrl) {
    const url = generateIssueUrl(report, reportUrl);
    window.open(url, '_blank', 'noopener,noreferrer');
}

// ============================================================================
// Exports
// ============================================================================

if (typeof window !== 'undefined') {
    window.TEAGitHub = {
        GITHUB_ORG,
        GITHUB_REPO,
        searchSimilarIssues,
        searchByStackSignature,
        findSimilarIssues,
        generateIssueTitle,
        generateIssueBody,
        generateIssueUrl,
        formatStackTrace,
        openGitHubIssue
    };
}

if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        GITHUB_ORG,
        GITHUB_REPO,
        searchSimilarIssues,
        searchByStackSignature,
        findSimilarIssues,
        generateIssueTitle,
        generateIssueBody,
        generateIssueUrl,
        formatStackTrace,
        openGitHubIssue
    };
}
