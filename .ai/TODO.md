# Task Register

**Purpose:** Track open, active, blocked, completed, and deferred tasks for the user-level `~/config/.ai/` bootstrap and template configuration.

## Outstanding Snapshot

- Open: 0
- In Progress: 0
- Blocked: 0
- Deferred: 2
- Last Review: 2026-04-24
- Top Priorities: T-2026-04-001 (gated by truncation/length triggers)

---

## Open

<!-- none -->

## In Progress

<!-- none -->

## Blocked

<!-- none -->

## Deferred

### T-2026-04-001 — Extract templates from BOOTSTRAP.md into separate files

- Status: Deferred
- Origin: Chat 2026-04-24 (BOOTSTRAP.md assessment / improvements)
- Last Updated: 2026-04-24
- Trigger conditions (any one is sufficient):
  - A real truncation occurs in a session that attaches BOOTSTRAP.md.
  - `BOOTSTRAP.md` exceeds ~1500 lines.
  - A new AI tool adopted in the workflow requires modular templates.
- Notes:
  - Move the four templates (`README.md`, `CONTEXT.md`, `INSTRUCTIONS.md`, `TODO.md`) into `~/config/.ai/templates/` and reference them from `BOOTSTRAP.md` by path.
  - Tradeoff: monolithic BOOTSTRAP.md is portable (drag/drop into any chat, web or local). Multi-file is cleaner but breaks sandboxed/web agents that can't fetch siblings.
  - Defer chosen because: portability beats modularity for current workflow; no measured friction yet; first-time bootstrap is rare relative to harvest.

### T-2026-04-002 — Reassess modular-templates question after several harvest cycles

- Status: Deferred
- Origin: Chat 2026-04-24 (BOOTSTRAP.md assessment / improvements)
- Last Updated: 2026-04-24
- Trigger conditions:
  - After running harvest mode on multiple chats across multiple repos (e.g., ≥5 harvests), evaluate whether agents missed instructions or produced truncated output.
  - If yes → promote T-2026-04-001 from Deferred to Open.
  - If no → mark this task Done with the verdict "monolithic confirmed adequate."
- Notes:
  - Companion to T-2026-04-001. This is the verification step that decides whether the deferral should end.

## Done

<!-- none -->

## Deprecated

<!-- none -->
