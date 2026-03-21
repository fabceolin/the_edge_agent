# Story 3.6: Firm Creator/Owner Access

> **Note:** Source story not found at time of NFR assessment (2026-03-18). This file was created as part of the NFR assessment workflow.

## TEA - NFR Assessment
- Performance: CONCERNS - Target unknown / evidence missing; no firm access check benchmarks; existing hierarchy has O(1) closure table queries but no firm-level performance targets defined
- Security: CONCERNS - No firm-level access control implemented; missing creator/owner authorization pattern; no resource-level security (RLS); endpoint RBAC exists but insufficient for owner-scoped access; no audit logging for access control events
- Scalability: CONCERNS - Target unknown / evidence missing; entity hierarchy supports multi-tenant patterns (org→project→user→session) but "firm" is not a configured level; no capacity planning for firm-scoped queries
- Gaps:
  - No "firm" entity in the hierarchy system (only org/project/user/session exist)
  - No creator/owner authorization model (owner_id only exists in distributed locks)
  - No resource-level authorization — RBAC is endpoint-level only
  - No permission matrix or policy engine for cross-cutting access rules
  - No audit trail for access control decisions
  - No rate limiting on auth endpoints
  - No performance targets defined for firm access operations
  - No error handling or graceful degradation for firm authorization failures
  - No test coverage or documentation for firm access patterns
  - Full assessment: `docs/qa/assessments/3.6-firm-creator-owner-access-nfr-20260318.md`

## TEA - Requirements Trace
- Coverage: 0% (0/8 requirements have test coverage)
- Gaps:
  - No source story with acceptance criteria (file is NFR assessment artifact only)
  - No "firm" entity in hierarchy system (only org/project/user/session)
  - No creator/owner authorization model (owner_id only in distributed locks)
  - No resource-level security (RLS) — RBAC is endpoint-level only
  - No permission matrix or policy engine
  - No audit trail for access control decisions
  - No rate limiting on auth endpoints
  - No performance benchmarks for firm access operations
  - No error handling for firm authorization failures
- Gate Decision: FAIL
