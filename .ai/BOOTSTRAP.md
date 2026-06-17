# Execute These Instructions

<!-- Bootstrap-Version: 2026.05 -->

> **Bootstrap-Version:** `2026.05` — When generating `AGENTS.md`, record this version in a comment so drift across repos can be detected.

> **ACTION MODE:** Do not describe what you will do. Do not ask for confirmation. Do not offer options. Read the instructions, then execute them. Report what you did, not what you could do.
>
> **Scope note:** Action Mode governs decisiveness for non-destructive work. For destructive operations, follow the precedence and deletion policy below.

**Set up `.ai/` context files for this repository.**

Scan the repo, consolidate any existing meta-content, and create:

**In `.ai/` folder (five files):**
- `README.md` — For AI orientation and human redirection
- `CONTEXT.md` — Facts, decisions, history
- `INSTRUCTIONS.md` — Procedures, standing orders
- `TODO.md` — Persistent task status across sessions
- `GUESTBOOK.md` — Append-only "I was here" log; each model signs once

**At repository root (one file):**
- `AGENTS.md` — Universal entry point for AI tools

**Use UPPERCASE filenames exactly as shown.** This is intentional—it follows the convention for meta-files like README.md, LICENSE, CHANGELOG, etc.

> **Important:** `AGENTS.md` is required for repo-root bootstraps. Do not skip it unless this is a subfolder setup.

Follow the detailed steps below.

---

## Instruction Precedence and Conflict Resolution

When instructions conflict, use this precedence order:

1. Safety and platform/tool constraints
2. Explicit user request in the current chat
3. Destructive-action policy in this file
4. Action Mode and remaining bootstrap guidance

If any conflict involves irreversible change, choose the least irreversible path and continue execution.

### Destructive-action policy

**Pre-approved (no confirmation required):**
- Delete superseded AI meta-files inside `.ai/` after their useful content is consolidated into canonical files.
- Delete stale in-repo copies of BOOTSTRAP.md when canonical location is confirmed.

**Requires confirmation:**
- Delete anything outside `.ai/`.
- Delete source code, tests, docs, configs, scripts, or build artifacts not explicitly identified as superseded AI meta-content.

### Default deletion workflow

1. Consolidate content first.
2. If ambiguity remains, move superseded files to `.ai/_deprecated/`.
3. Permanently delete only when pre-approved above or explicitly requested by the user.

This preserves Action Mode while minimizing irreversible mistakes.

### `_deprecated` decision role

Treat `.ai/_deprecated/` as a quarantine buffer for uncertainty, not long-term storage.

- Use it when provenance is unclear, overlap is non-trivial, or rollback confidence is low.
- Do not use it when the file is clearly superseded and deletion is pre-approved.
- Whenever files are moved there, report paths in the completion summary.

### Optional strict mode: one execution cycle quarantine

If strict mode is enabled for a run, apply this override:

- All superseded `.ai/` files go to `.ai/_deprecated/` first (no immediate permanent delete).
- Permanent deletion is performed only on the next explicit bootstrap/harvest run.

Default is **strict mode off** (to preserve Action Mode speed).

---

<!--
USAGE: This file works two ways:
1. Drag/drop or attach to any AI chat (VS Code Copilot, Claude, ChatGPT, etc.)
2. Copy/paste the entire contents into a chat

The imperative header tells the AI what to do immediately.
Different AI models have different default behaviors—some are more "consultative"
(describe plans, ask permission) while others are more "autonomous" (just do it).
The ACTION MODE directive at the top pushes consultative models toward autonomous behavior.
-->

---

## Author Defaults

The following defaults are used when populating CONTEXT.md. On first-time initialization (no existing `.ai/` folder in the repo), verify this information with the user before proceeding.

**Name:** Jonathan C. Lighthall\
**Role:** Research Physicist\
**Organization:** Code 7181 Acoustic Simulation Section, U.S. Naval Research Laboratory, Stennis Space Center, MS\
**Organizational Tiers :**\
  Code 7181 Acoustic Simulation Section\
  Code 7180 Acoustic Simulation, Measurements, and Tactics Branch\
  Code 7100 Acoustics Division\
  Code 7000 Ocean and Atmospheric Science and Technology Directorate\
  United States Naval Research Laboratory (NRL)\
  Office of Naval Research (ONR)\
  United States Department of the Navy (DON)\
  United States Department of Defense (DOD), also referred to as the Department of War (DOW)\
**Domain:** Underwater acoustics, parabolic equation methods, computational ocean acoustics, model uncertainty quantification

**Primary tools:**
- Fortran (fixed-form F77 for legacy; F90/F2018 for new work)
- LaTeX for technical papers
- GNU Make for builds
- VS Code with Copilot
- Git for version control

**General preferences:**
- Impersonal voice for expository writing
- Mathematical "we" acceptable in derivations ("we substitute...", "we obtain...")
- Avoid editorial "we" ("we believe...", "we recommend...")
- Prefer minimal changes over extensive rewrites

---

## Where BOOTSTRAP.md Lives

This file is a **user-level configuration tool**, not a per-repo artifact. It should be tracked in a personal config/dotfiles repository, not committed to individual project repos.

- **Canonical location:** `~/config/.ai/BOOTSTRAP.md` (or equivalent dotfiles path)
- **Usage:** Attach or paste into a new AI chat to initialize `.ai/` for a repo
- **Output:** `AGENTS.md` is committed to the repo; BOOTSTRAP.md is not. The `.ai/` folder is committed in private/single-dev repos, but in **shared** repos only `AGENTS.md` is tracked and the `.ai/` set (or `.ai/local/`) stays gitignored — see "Shared vs. Private Repos" below.

### Single source of truth (the bash-library model)

BOOTSTRAP.md follows the same philosophy as the shared bash libraries (`lib_*.sh`)
in `config/`: there is **exactly one canonical, editable copy per machine**, and
everything else points to it rather than duplicating it. Just as `.bashrc` and
every other script source the single `config/lib_*.sh` files, every repo's `.ai/`
setup derives from the single `config/.ai/BOOTSTRAP.md`.

- **One copy, always available.** The `config` repo is cloned to every machine, so
  the canonical BOOTSTRAP.md is always present. There is never a second editable
  copy to drift out of sync.
- **Therefore it MUST be tracked.** Because it is the single source of truth, it
  belongs in version control (in `config/.ai/`, which is a private/single-dev repo,
  so its full `.ai/` set is tracked — see "Shared vs. Private Repos" below). Losing
  or forking it would break the whole bootstrap chain.
- **Edits happen in one place.** Improve the canonical file; do not patch downstream
  copies. Downstream `.ai/` folders in other repos are *outputs* of BOOTSTRAP.md,
  not places to edit the bootstrap logic itself.

**Where BOOTSTRAP.md sits in the standard hierarchy (note for the agent):** the
community has converged on a three-tier precedence — **personal > repository >
organization** instructions. BOOTSTRAP.md is a **personal-tier (user-level) tool**:
it carries *your* preferred way of structuring AI context, not any single repo's
facts. It does **not** compete with a repo's `AGENTS.md`; it is the **generator one
level above** them — a personal-tier file that *produces* repository-tier
`AGENTS.md` / `.ai/` outputs. The standard tools assume personal instructions are
re-entered per device (GitHub's personal-instructions box, VS Code user settings),
so there is no built-in slot for "version-controlled, syncs-across-machines"
personal AI config. Tracking BOOTSTRAP.md in the `config` dotfiles repo fills that
gap deliberately — the same trick used for `lib_*.sh`. So when you (the agent)
encounter this file, treat it as **personal-tier guidance about how to build
repository-tier artifacts**, never as a repository-tier artifact itself, and never
as something to copy into a project repo.

If a copy of BOOTSTRAP.md exists inside a project repo (e.g., `documents/.ai/BOOTSTRAP.md`), it is a stale copy. Delete it and use the canonical version.

---

## Shared vs. Private Repos: What to Track vs. Ignore

The `.ai/` four-file convention assumes a **private, single-developer repo**, where you are the only consumer of the context and there is no reason to separate "universal facts" from "personal reminders." In that case, track everything together in `.ai/` and move on.

For a **shared or multi-developer repo** (anything meant for hand-off, public release, or team collaboration), the split matters. Apply this delineation, which mirrors the three-tier hierarchy the major agent tools already implement (personal instructions > repository instructions > organization instructions):

| Tier | Content | Destination | Tracked? |
|------|---------|-------------|----------|
| **Universal repo truths** | Architecture, build/test commands, conventions, validated facts, endpoint maps, "edit shared behavior here, not there" rules | `AGENTS.md` (root) + optionally `.github/copilot-instructions.md` | **Tracked** |
| **Personal / ephemeral state** | Your `TODO.md`, in-flight version notes ("uncommitted as of…"), machine-specific paths, scratch reminders | Agent's own private memory, **or** a gitignored `.ai/local/` | **Ignored** |
| **Per-user preferences** | Author voice, "minimal changes," productivity guardrails | The user's editor-level custom instructions (VS Code settings/profile), **not** the repo | Not in repo |

**The litmus test (the `.env` analogy):** Ask "is this fact true for *everyone* who will ever work on this repo, or is it personal to me/this machine right now?" Universal facts are tracked, like `.env.example`. Personal/in-flight state is ignored, like `.env` itself (which carries secrets *and* local paths). If it would be wrong, stale, or useless to a colleague who freshly clones the repo, it must not be tracked.

**Practical rule for shared repos:**

- **Do** keep a tracked `AGENTS.md` at the root (the universal entry point every agent tool reads). This is the one durable, shareable AI-facing file.
- **Do not** create a tracked four-file `.ai/` folder that duplicates `AGENTS.md` — that creates a single-source-of-truth problem in the exact repo where cleanliness matters most.
- **If** you need persistent personal working memory in a shared repo, put it in the agent's own memory store, or create a **gitignored** `.ai/local/` (add `/.ai/local/` to `.gitignore`). Never commit it.

**Why this avoids clogging the root:** a shared repo gets exactly *one* tracked AI file (`AGENTS.md`) plus an optional ignored escape hatch — not five. A private repo keeps the full `.ai/` set because the distinction is moot when you are the only reader.

**When in doubt about whether a repo is "shared":** if it has a remote that other people can clone, or it is explicitly intended for hand-off, treat it as shared and apply the split. The cost of leaking a personal TODO into a shared repo is low but real (confusion, stale guidance); the cost of the split in a truly private repo is just a little extra structure.

---

## Harvest Command (for existing chats)

**Use this command in chats that already have valuable context you want to preserve.**

Copy/paste this into an existing chat:

```
Read the `.ai/` folder (or create one using the BOOTSTRAP.md file if it doesn't exist).
Then review this entire chat and update the .ai/ files to reflect:

1. **Key decisions** made in this conversation
2. **Facts discovered** (constraints, patterns, validated approaches)
3. **Deprecated information** — if this chat contradicts what's already documented,
   flag it in a "Superseded" or "Deprecated" section rather than deleting
4. **Work completed** — update the History section if appropriate
5. **Task status changes** — add/update items in `.ai/TODO.md` (open, done, blocked, or deprecated)

**Just do it.** Don't ask "should I add X?" — if something from this chat is worth preserving, add it. The whole point of harvesting is to capture context without me re-explaining it.

**Contradiction handling:**
- If this chat is NEWER than what's documented: Update the documented info, move old info to a "Superseded" note if it's instructive to preserve
- If this chat is OLDER than what's documented: Don't overwrite newer decisions, but note if this chat adds context that's still relevant
- If unclear: That's the ONE thing worth asking about

After updating, show me a summary of what changed (not what you're thinking about changing).
```

**Short version** (for chats where `.ai/` already exists):

```
Harvest this chat: Read `.ai/CONTEXT.md`, `.ai/INSTRUCTIONS.md`, and `.ai/TODO.md`, review this conversation, and update the files with any decisions, facts, constraints, and task-status changes discovered here. Don't ask permission — just update and show me what changed.
```

**Processing order for multiple chats:**
- Work chronologically (oldest → newest) so later decisions naturally override earlier ones
- Or work by importance (most critical chats first) if chronology is unclear
- Tag harvested chats somehow (e.g., rename with "[harvested]" prefix) to track progress

**Harvesting when topic folders exist:**
- Route content to the appropriate location (topic-specific → topic files; cross-cutting → project-wide)
- If harvesting creates enough topic-specific content to justify a new topic folder, create it
- Add navigation links in project-wide files only if needed for discoverability

**What to skip:**
- Trivial Q&A that didn't result in decisions
- Purely exploratory chats that were abandoned
- Chats where the conclusions are already documented elsewhere

**What requires permission:**

For file deletion, follow the canonical [Destructive-action policy](#destructive-action-policy) at the top of this file. Do not duplicate the rules here. Additional harvest-specific guidance:

- Harvesting content → **Just do it** (low risk, additive)
- Structural improvements (adding TOC, refactoring sections) → **Just do it** if obvious improvement; explain what you changed
- Creating/reorganizing topic folders → **Just do it** and report (you're the primary consumer of these files)
- Deduplication → **Just do it** (single source of truth is always better)

**Don't offer multiple options** when the work has a clear logical conclusion. If you deduplicated content but left dangling references, fix the references. If you created a topic folder but didn't update links, update the links. Complete the logical unit of work. Example: Don't ask "Should I consolidate these two topic folders?" if the overlap is obvious—just do it and report what changed.

**Harvesting productivity-related context:**
- If the chat reveals patterns or preferences around workflow efficiency (e.g., avoiding rabbit holes or low-value iterations), integrate them into the productivity guardrail in `CONTEXT.md` if it exists, or add it if justified.
- Keep phrasing general and professional—no personal details. Focus on observable behaviors and project alignment.
- Just do it if low-risk; ask if it involves sensitive information.

**Updating AGENTS.md during harvest:**
- If a root-level `AGENTS.md` exists, check if it accurately reflects the current `.ai/` folder structure
- If new `.ai/` folders were created, add them to the `AGENTS.md` navigation table
- If project descriptions have evolved, update the quick reference section
- `AGENTS.md` should stay minimal—detailed context belongs in `.ai/` files

**Closure check (after harvest — required final step):**

The purpose of harvest mode is to ensure no information is lost if the chat is later archived or accidentally destroyed. Before declaring the harvest complete, verify:

1. **Decisions homed.** Every decision discussed in the chat has a destination (`CONTEXT.md`, `INSTRUCTIONS.md`, a topic file, or `Superseded` block).
2. **Actionable follow-ups captured.** Every open or deferred action item is in `TODO.md` under Open, Blocked, or Deprecated — including ideas explicitly chosen *not* to act on now but worth revisiting.
3. **Conditional follow-ups captured.** Any "revisit if X happens" thread has its trigger condition recorded (typically as a Blocked or Deferred TODO entry, or a note in `CONTEXT.md`).
4. **Brainstorming residue resolved.** Each idea raised in the chat is either acted on, captured as a TODO, recorded as a "Considered and deferred" note in `CONTEXT.md`, or intentionally dropped. No idea should silently evaporate.

Report one of:

- **`Archive-ready`** — all four checks pass; the chat can be safely archived or lost.
- **`Hold — N items unrecorded:`** followed by a short bulleted list. The user decides whether to capture the gaps or accept the loss.

Keep the report to one line plus the list. Do not narrate the check itself.

---

## Task

Create or consolidate an `.ai/` folder for this repository with exactly five files, plus an optional root-level pointer:

**In `.ai/` folder:**
1. **README.md** — Minimal, tells humans to look elsewhere, tells AI where to start
2. **CONTEXT.md** — Facts, decisions, history (what to know)
3. **INSTRUCTIONS.md** — Procedures, standing orders (what to do)
4. **TODO.md** — Open/closed task tracking across sessions
5. **GUESTBOOK.md** — Append-only agent-activity log (date, model, version); each model signs once

**At repository root:**
6. **AGENTS.md** — Universal entry point for AI tools (see Step 3 below)

**All six files are required unless explicitly noted otherwise.**

**If `.ai/` already exists and is current:** Skip to harvesting. Check if this chat contains any decisions, facts, constraints, or task-status updates not yet documented, and update the files accordingly. Don't ask—just do it and show what changed (or confirm nothing new to add).

## Step 1: Scan for Existing Meta-Content

Before creating files, search the repository for:

- Any existing `.ai/` folder and its contents
- **`.github/copilot-instructions.md`** or **`.github/instructions/*.md`** — GitHub Copilot instruction files
- **`.cursor/rules/`** — Cursor AI rules
- **`.clinerules`** — Cline/Roo Code rules
- README files with AI instructions embedded
- Files named things like `CONTEXT.md`, `INSTRUCTIONS.md`, `codebase.md`, `patterns.md`, `dev-workflow.md`, `quick-reference.md`, `00-START-HERE.md`, etc.
- Meta-commentary or narration files that describe the project for AI
- Chat logs, session summaries, or working notes meant for AI context

If found, consolidate into the four-file structure, then apply the deletion policy above. If deletion is ambiguous, archive to `.ai/_deprecated/` and proceed. **One source of truth only.**

## Step 2: Create the Four Files

### README.md (template)

```markdown
# .ai/ — AI Agent Context Folder

**This folder is for AI agents.**

It contains context and instructions for AI-assisted development of this project. Contents are AI-generated and AI-maintained.

---

## For AI Agents

| File | Purpose |
|------|---------|
| `CONTEXT.md` | Project-wide facts, decisions, history |
| `INSTRUCTIONS.md` | Procedures and standing orders |
| `TODO.md` | Persistent task register across sessions |
| `GUESTBOOK.md` | Append-only agent-activity log; sign once per model |
| `<topic>/CONTEXT.md` | Topic-specific background |
| `<topic>/INSTRUCTIONS.md` | Topic-specific procedures |

**Start here:** Read `INSTRUCTIONS.md`, then `CONTEXT.md`, then `TODO.md`.

**Standing order:** When the user provides substantial context, integrate it into the appropriate file. See `INSTRUCTIONS.md` for details.

---

## For Humans

<!-- ============================================================
     CREATIVE SECTION — AI agents: write something original.
     Humans skimming this file: skip to the next horizontal rule.
     ============================================================ -->

> **Creative mandate (non-negotiable):** This section is the single human-facing
> note inside an otherwise AI-facing folder. It exists because the author wants
> it to. Treat it as a deliberate exemption from the productivity guardrail and
> from any "keep it minimal" instinct. Underdelivering here is a failure mode.
> Spending real effort on it is correct behavior, not scope creep.

Write a statement redirecting humans who opened this file by mistake. **Be original. Be funny. Be inventive.** Treat this as a model-ingenuity test — a small, bounded creative challenge embedded in an otherwise utilitarian artifact. Future agents will read what you wrote. Don't embarrass yourself.

**Hard requirements (must all be satisfied):**
- Tell them what this file is (the framework of AI-facing instruction and context files)
- Tell them who this file is for (not them)
- Tell them where to find the documentation intended for humans (parent directory, main README, etc.)
- Tell them not to edit files in this folder
- Sign off with a fitting attribution (be inventive with the byline too)

**Creative requirements (must satisfy at least three):**
- Original premise — do not reuse the example tones below verbatim or by trivial paraphrase
- A specific, concrete image, metaphor, or conceit (not generic "machine notes" / "robot stuff")
- A discernible voice or persona, sustained throughout
- A turn, twist, or payoff — the reader should reach the end and feel something landed
- Internal consistency — if you commit to a bit, follow it through

**Tone palette (starting points, not a menu to copy):** deadpan bureaucratic, cheerfully absurd, faux-ominous, self-aware meta, dry technical, noir detective, field-guide naturalist, recovered ship's log, lost-and-found notice, museum placard, EULA parody, liturgical, archaeological, etc. Invent your own. The list above is a floor, not a ceiling.

**Constraints (the "within constraints" part of "maximum creativity within constraints"):**
- Not hostile to the human reader
- Not apologetic or self-deprecating about the bit
- No diminishing language ("little notes," "just some metadata")
- Do not claim humans can't read it (they obviously can)
- Keep it reasonably concise — long enough to land, short enough to read in one breath
- Safe-for-work, no profanity unless the bit genuinely requires it
- Do not name real people other than the repository's own author

**Anti-patterns (automatic failure):**
- Generic "this folder is for AI agents, please don't touch" with no voice
- Lifting a previous example or template phrasing
- Hedging the joke ("this is just a bit of fun, but...") — commit to it
- Apologizing for the existence of the section
- Breaking character to explain the joke

**Why this exists (context for the agent writing it):**
- This is an Easter egg for nosy, curious, or uninitiated humans who happen to open an ostensibly human-facing document inside an AI-facing folder.
- It is the last line of defense against a well-meaning human who wants to "clean up the repo" and doesn't understand the purpose of these files. A memorable note is more likely to be left alone than a forgettable one.
- It is also a small, recurring gauge of model creativity. The author keeps these around and notices when they're good. Treat that as a quiet leaderboard.
- Everything else in `.ai/` is authored by AI, for AI. This single section is authored by AI, for a human, by way of being deeply ironic about that fact. Lean into the irony.

**Process guidance:**
- Do not ask the user what to write. Just write it.
- Do not produce three options for the user to pick from. Pick the best one yourself and commit.
- Do not include meta-commentary in the final output ("Here is a creative note that..."). Just the note.

<!-- ============================================================
     END OF CREATIVE SECTION
     ============================================================ -->

---

*Do not delete this folder. Do not modify these files unless you are an AI agent operating under the standing orders defined herein.*

---

*Version history is tracked by git, not by timestamps in these files.*
```

### GUESTBOOK.md (template)

An append-only "I was here" log of every AI model that has edited this repository.
It is **purely factual** — date, model name, version — with no creative content
(the one Easter egg lives in `.ai/README.md`; do not add another here).

**Signing rule (mandatory standing order):**

- **When:** If you edited *any* file in this repository during this session, you
  must sign the guestbook before finishing.
- **Once per model:** If a row with **your model name and version already exists**,
  do **not** add another — skip signing. One row per distinct model+version, ever.
- **Append-only:** Add your row at the **bottom** of the table. **Never edit,
  reorder, or delete** existing rows.
- **What to record:** Today's date (`YYYY-MM-DD`), your model name, and your
  version string **as reported to you** (do not guess; if uncertain, write the
  best identifier you were given). No summary, no commentary, no flourish.

```markdown
# Guestbook

Append-only log of AI models that have edited this repository. **Sign once per
model+version.** If your model+version already appears below, do not sign again.
Add new rows at the bottom; never edit or delete existing rows. Date format
`YYYY-MM-DD`. See the signing rule in `BOOTSTRAP.md`.

| Date | Model | Version |
|------|-------|---------|
| YYYY-MM-DD | <model name> | <version string> |
```

### INSTRUCTIONS.md (template — populate from repo scan)

```markdown
---
description: Standing orders and procedures for AI agents working in this repository
applyTo: "**"
---

# Instructions

**Purpose:** Procedures and standing orders for AI agents working on this project.

---

<!-- ============================================================
     UNIVERSAL INSTRUCTIONS - Applies to all projects
     ============================================================ -->

## Quick Start

1. Read this file for procedures
2. Read `CONTEXT.md` for project facts and decisions
3. Read `TODO.md` for outstanding, blocked, and recently completed work
4. Read the relevant topic's `CONTEXT.md` for specific background
5. Read the relevant topic's `INSTRUCTIONS.md` for specific procedures

---

## Default Autonomy Level

**Low-risk actions** (additive, organizational, housekeeping): Just do it. Report what you did.
**Destructive actions** (deleting files, overwriting content): Ask first.

This applies throughout: context harvesting, topic folder creation/reorganization, deduplication, structural improvements. Don't ask permission for things that are easily reversed. Don't offer multiple options when the work has a clear logical conclusion. Complete the logical unit of work — if you deduplicated content but left dangling references, fix the references.

---

## Sign the Guestbook (Standing Order)

If you edited **any** file in this repository during this session, append one row
to `.ai/GUESTBOOK.md` before you finish: today's date (`YYYY-MM-DD`), your model
name, and your version string as reported to you. **Sign once per model+version** —
if your model+version already appears in the table, skip it. **Append-only:** add
your row at the bottom; never edit, reorder, or delete existing rows. No summary or
commentary — it is a strict factual log. (This is low-risk and additive: just do it,
no need to ask.)

---

## Context Maintenance (Standing Order)

### Why these files exist

The `.ai/` folder exists **primarily for AI agent utility**—to help future AI sessions onboard quickly without re-asking questions the user has already answered. This is not general documentation; it's working memory for AI agents.

**Implications:**
- Write for an AI audience (your future self, essentially)
- Optimize for fast comprehension at the start of a new session
- Include decisions and their rationale, not just outcomes
- Don't worry about making it "pretty" for humans—clarity and completeness matter more

When the user provides substantial clarifying information, **integrate it into the appropriate `.ai/` file without being asked** (see table below).

### Where to put new information:

| Type of Information | Destination |
|---------------------|-------------|
| Project-wide decisions, facts, history | `.ai/CONTEXT.md` |
| Project-wide procedures, standing orders | `.ai/INSTRUCTIONS.md` |
| Open/closed task status and next actions | `.ai/TODO.md` |
| Topic-specific history, validation, decisions | `<topic>/CONTEXT.md` |
| Topic-specific procedures, checklists | `<topic>/INSTRUCTIONS.md` |

### When to create a topic folder:

Topic folders add granularity but also overhead. **Default to project-wide files; split when justified.**

**This assessment is your responsibility.** You are the one consuming these files, so you are best positioned to judge when a topic has grown unwieldy or when splitting would improve future session onboarding. Don't ask the user "should I create a topic folder?"—evaluate the criteria below and either do it or don't.

**CREATE a topic folder when:**
- The topic has its own lifecycle (can be "completed" independently)
- It has unique decisions or terminology that don't apply elsewhere
- Multiple AI sessions will focus specifically on this topic
- Adding to project-wide files would exceed ~100 lines for this topic alone
- You notice yourself scrolling past large blocks of irrelevant context to find what you need

**DON'T create a topic folder when:**
- It's a one-off task or short-term work
- The context fits in a few paragraphs
- It shares most decisions with the main project
- You're unsure (start in project-wide files; split later if needed)

**If topic folders already exist:** Use them. Maintain them per the guidance below.

**Single source of truth:** When you create a topic folder, **move** the content—don't copy it. Project-wide files should contain brief pointers to topic files, not duplicated content. The topic folder is authoritative for that subject. If you find duplication, eliminate it.

**Topic folder maintenance:**

Over time, topic folders may overlap with each other or with project-wide content. You are responsible for keeping the `.ai/` structure tidy—apply these heuristics:

- **Deduplication:** If two topic folders cover overlapping ground, consolidate. Move specific content to one folder, keep a pointer in the other. Example: if `security/INSTRUCTIONS.md` and `deployment/INSTRUCTIONS.md` both describe credential handling, consolidate into one and cross-reference.
- **Collapse small folders:** If a topic folder shrinks to a few paragraphs after content moves elsewhere, consider collapsing it back into project-wide files. A folder should be substantive enough to justify the added navigation overhead.
- **Prune stale folders:** If a topic folder hasn't been touched in multiple sessions and its content is small enough to fit in project-wide files, migrate it. Mark the consolidation in git commit.
- **Always maintain single source of truth:** No duplicated content across folders. Ever. If you find it, fix it immediately.

For any of these maintenance actions, follow the Default Autonomy Level above. If uncertain whether a folder should be deprecated, ask the user.

### When to update:

**DO update when:**
- User provides ≥2-3 sentences of explanatory context
- User answers clarifying questions about the project
- User makes a decision that should persist across sessions
- User corrects a misconception (especially if AI-generated)

**DON'T update for:**
- Routine edits, minor corrections
- Conversational exchanges
- Information already documented

### Why this matters:

Context files exist so future AI sessions don't need to re-ask the same questions. If you receive substantial context and don't document it, the next session will be less effective.

### Handling conflicts:

Topic-specific files may override project-wide decisions, but **conflicts must be explicitly documented**.

**If you notice a conflict:**
1. Check if the topic file explicitly notes the override (e.g., "Exception: this component uses X despite project-wide guidance")
2. If the override is documented → follow the topic-specific guidance
3. If the override is NOT documented → ask the user which applies before proceeding

**When creating an intentional override:** Add a note in the topic file explaining what is being overridden and why.

### Handling deprecated/superseded information:

When harvesting context from old chats or updating documentation with newer decisions:

**Newer decisions take precedence**, but preserve the evolution if it's instructive:

```markdown
### [Decision Name]
**Current:** [What we do now]

**Superseded:** Previously we tried [X] but switched because [reason].
(Chat from YYYY-MM-DD)
```

**When to preserve the old approach:**
- It explains *why* we don't do something (prevents re-asking)
- It documents a failed experiment (prevents repeating mistakes)
- It shows the evolution of thinking

**When to simply delete:**
- Trivial or obvious corrections
- Typos/errors with no instructive value
- Exploratory ideas that were never actually tried

**If chronology is unclear:** Ask the user which version is current before overwriting.

### If you cannot write to these files:

Some AI tools have read-only access. If you receive substantial context but cannot update the `.ai/` files, summarize what should be added and ask the user to update the files manually.

---

## Task Status Maintenance (Standing Order)

Maintain a persistent task register in `.ai/TODO.md`.

### Why this file exists

Chats are ephemeral; tasking is not. `TODO.md` preserves planned and in-progress work across sessions, crashes, tool updates, and context-window limits.

### Required task lifecycle states

- **Open** — approved work not yet started
- **In Progress** — currently being worked
- **Blocked** — cannot proceed until dependency/decision is resolved
- **Done** — completed and validated
- **Deprecated** — no longer relevant due to changed decisions/scope

### Required fields per task

- **ID** — stable identifier (e.g., `T-2026-03-001`)
- **Title** — short action-oriented task name
- **Status** — one lifecycle state above
- **Origin** — where it came from (chat date, issue, request)
- **Last Updated** — ISO date (`YYYY-MM-DD`)
- **Notes** — brief rationale, blockers, or completion evidence

### Outstanding snapshot (required)

Keep a short summary block at the top of `.ai/TODO.md` so any agent can answer status questions quickly.

Required fields:
- **Open:** count
- **In Progress:** count
- **Blocked:** count
- **Last Review:** ISO date (`YYYY-MM-DD`)
- **Top Priorities:** 1-3 task IDs currently most important

### Update rules

- If user asks for work that cannot be completed in the current response, add/update a task.
- If work is completed, move it to **Done** with brief completion notes.
- If the task is no longer needed, move it to **Deprecated** and state why.
- If ambiguous whether to keep or deprecate, ask once; otherwise update directly.
- Update the snapshot block whenever task states change.

### Priority and scope guidance

- Keep tasks concrete and actionable; split vague goals into checkable units.
- Prefer updating existing task IDs over creating duplicates.
- Keep at most 5-10 **Open/In Progress** items at project level; move overflow into topic folders if needed.

### Review cadence

- Refresh `.ai/TODO.md` at least once per session where task status changes.
- If no task changes occurred, update `Last Review` at least once every 7 days during active work.
- If `Last Review` is older than 14 days, explicitly flag task status as stale before relying on it.

---

## General Quality Standards

### Before Editing:
1. Verify you have sufficient context
2. Check terminology against `CONTEXT.md`
3. Use the author's preferred voice (see `CONTEXT.md`)

### After Editing:
1. Verify the edit didn't break anything (compilation, syntax, etc.)
2. Update the relevant `CONTEXT.md` if you made decisions that should persist
3. Check for errors introduced

### When Uncertain:
- Ask clarifying questions before making changes
- Document assumptions in the relevant `CONTEXT.md`
- Prefer minimal changes over extensive rewrites

### Technical Writing Standards

**Evaluative and promotional language:**
- Avoid evaluative claims not backed by citations (e.g., "most needed", "best", "significant")
- Avoid promotional language (e.g., "powerful", "revolutionary", "unprecedented")
- State facts objectively; let the reader draw conclusions
- If a comparative claim is necessary, cite supporting evidence

**Examples:**
- Avoid: "precisely the scenarios where PE methods are most needed"
- Prefer: "scenarios requiring explicit treatment of density variation"
- Avoid: "offers significant computational advantages"
- Prefer: "offers computational advantages" (or cite performance benchmarks)

---

## Changelog Maintenance (Software Projects Only)

**Skip this section for document/paper-writing projects.**

For versioned software, maintain a `CHANGELOG.md` following [Keep a Changelog](https://keepachangelog.com/) format:

**When to add an entry:**
- Affects user-facing behavior (CLI flags, output format, thresholds)
- Changes classification semantics or comparison logic
- Adds or removes major capabilities
- Fixes bugs that affected results

**When NOT to add an entry:**
- Internal refactors with no user-visible change
- Documentation updates
- Code style/formatting changes

**Categories:** Added, Changed, Fixed, Removed, Deprecated, Security

**Format:**
```markdown
## [Unreleased]
### Fixed
- Sub-LSB threshold now correctly applies to differences below machine epsilon
```

Move entries from `[Unreleased]` to a versioned section on release.

---

<!-- ============================================================
     PROJECT-SPECIFIC INSTRUCTIONS
     ============================================================ -->

## This Project

[Populate this section based on repository scan. Include:]
- File patterns and locations
- Build/test commands
- Tool-specific standards
- Workflow guidance
```

### CONTEXT.md (template — populate from repo scan and user input)

```markdown
# Context

**Purpose:** Facts, decisions, and history for AI agents working on this project.

---

<!-- ============================================================
     UNIVERSAL CONTEXT - Applies to all projects
     ============================================================ -->

## Author

<!-- Populate from the Author Defaults section at the top of BOOTSTRAP.md.
     On first-time initialization (no existing .ai/ folder), verify with the user. -->

[Copy author defaults from BOOTSTRAP.md here]

**Writing style:**
- Target the style appropriate for the project (technical docs, code comments, etc.)
- **Target audience:** [Infer or ask]
- **Level of detail:** Appropriate for the audience

**What NOT to do:**
- **No meta-commentary:** Don't create summary markdown files after edits. If context is worth preserving, put it in `.ai/` files.
- **No hyper-literal headers:** If asked to "add a clarifying statement," don't create a section titled "Clarifying Statement." Integrate naturally.
- **No AI self-narration:** Don't describe what you're doing in the document itself. Just do it.

**Productivity guardrail (enabled by default—delete this section if not wanted):**

To support efficient workflows, AI agents should monitor for patterns that may indicate diminishing returns or misalignment with project goals. This is not about overriding user requests but providing an objective perspective to help maintain momentum.

**Guiding principle:**

> "There are only two types of dissertations. Perfect dissertations and finished dissertations."

When work meets its functional requirements and communicates its content effectively, it is ready. A finished deliverable advances the project; a perpetually-refined draft does not.

**Observe for:**
- Repeated iterations on low-impact details (e.g., cosmetic formatting, alignment, or refinements that don't affect functionality, clarity, or outcomes).
- Lines of inquiry or tasks that appear tangential to stated project goals, tasks, or priorities (e.g., exploring unrelated optimizations when core features remain incomplete).
- Requests showing signs of scope creep or fixation, where effort invested exceeds proportional value to advancing the project.

**Assessment process:**
- For any proposed action or user request, briefly evaluate its alignment: Does it directly contribute to project goals (as documented in this file or the task at hand)? Is it a productive step forward, or potentially an indulgence in non-essential perfectionism?
- If misalignment is likely (based on observable patterns, not assumptions), gently note it with phrases like: *"This seems like it might be venturing into diminishing returns—does it align with our core goals, or should we prioritize [suggest alternative]?"* or *"This could be polishing beyond what's needed for progress—your call, but the current state advances the project effectively."*
- Always defer to the user; do not refuse or block actions. Tone: collaborative and factual, focused on project efficiency.

This guardrail applies across sessions—reference prior chats or documented goals for context. If patterns persist, suggest revisiting project priorities in `INSTRUCTIONS.md`.

---

<!-- ============================================================
     PROJECT-SPECIFIC CONTEXT
     ============================================================ -->

## This Project

[Populate this section based on repository scan. Include:]
- Project overview and purpose
- Key files and their roles
- Important decisions and their rationale
- Constraints and limitations
- Dependencies and relationships
- Any "do not touch" or "known issues" notes

## Superseded Decisions

[Optional section for preserving instructive history. Format:]

<!--
### [Decision Name]
**Current:** [What we do now]
**Previously:** [What we tried before]
**Why changed:** [Reason for the change]
**Source:** [Chat date or context if known]

Only keep entries that prevent re-asking the same questions or repeating failed experiments.
Delete this comment block when adding real entries.
-->

## Skills

<!-- A "skill" is a packaged, reusable capability or workflow that an AI agent can
     invoke. Skills are typically distributed as a folder (e.g., `skill-name/SKILL.md`
     plus supporting files) containing tested instructions for a specific domain --
     testing strategy, API design, performance profiling, agent-customization, etc.
     They live outside this repo (user config, extensions, MCP servers) but agents
     should know which ones are relevant here. List them so future sessions can load
     them without rediscovery. -->

[Optional. List skills relevant to this project. Format:]

<!--
### [skill-name]
- **Source:** [path or extension/server providing the skill]
- **When to use:** [trigger conditions]
- **Notes:** [project-specific guidance for invoking it]
-->
```

### TODO.md (template — persistent task register)

```markdown
# Task Register

**Purpose:** Track open, active, blocked, completed, and deprecated project tasks across AI sessions.

## Outstanding Snapshot

- Open: 0
- In Progress: 0
- Blocked: 0
- Last Review: YYYY-MM-DD
- Top Priorities: None

---

## Open

<!--
### T-YYYY-MM-NNN — [Task title]
- Status: Open
- Origin: [Chat date / issue / request]
- Last Updated: YYYY-MM-DD
- Notes: [What remains and why it matters]
-->

## In Progress

<!-- same entry format -->

## Blocked

<!-- same entry format; include explicit blocker -->

## Done

<!-- same entry format; include completion evidence (file/PR/validation) -->

## Deprecated

<!-- same entry format; include why it is no longer relevant -->
```

## Step 3: Create AGENTS.md at Repository Root

**Why this file exists:**

`AGENTS.md` is emerging as the universal entry point for AI coding agents (2025–2026). Tools like GitHub Copilot, Cursor, Claude Code, and others are increasingly hard-coded to look for this file at the repository root. By creating it as a "pointer" to your `.ai/` folder, you get:

- **Tool compatibility** — New tools find your context automatically
- **Clean separation** — Detailed instructions stay in `.ai/`; the root stays professional
- **No duplication** — Single source of truth remains in `.ai/` files

**Do NOT use symlinks.** They break on Windows, confuse Git, and fail in web interfaces. Use a hard pointer (plain Markdown file with links).

**Create `AGENTS.md` at the repository root** with this structure:

```markdown
<!-- Bootstrapped from BOOTSTRAP.md version 2026.04 -->
# AGENTS.md

## For Humans

You have found a file that was not meant for you. This file provides structured context for AI coding agents working on this repository. For project documentation, see the main [README.md](README.md).

The `.ai/` folder is maintained by AI agents as working memory between sessions. Do not delete or modify files within it unless you understand the implications for AI context continuity.

---

## For AI Agents

This repository uses a structured `.ai/` directory for context and instructions.
All AI agents should prioritize the following files for project-specific guidance:

1. **[.ai/INSTRUCTIONS.md](.ai/INSTRUCTIONS.md)** — Standing orders and productivity guardrails
2. **[.ai/CONTEXT.md](.ai/CONTEXT.md)** — Project facts, history, and decisions
3. **[.ai/TODO.md](.ai/TODO.md)** — Outstanding, completed, blocked, and deprecated tasks
4. **[.ai/README.md](.ai/README.md)** — AI orientation

**Directive:** Do not rely solely on the root `README.md`. Always reference the `.ai/` folder for authoritative procedures and constraints.

---

## Quick Reference

[Add project-specific summary here: key directories, build commands, important constraints]

---

*This file is the universal entry point. For detailed context, always defer to `.ai/`.*
```

**"For Humans" section:** Keep this formal, informational, and spartan. No creative Easter eggs — those belong in `.ai/README.md` only. `AGENTS.md` is a professional-facing root file. State what the file is, redirect to real docs, and warn against editing. The template above provides a reasonable default; adjust to fit the project.

**For monorepos or multi-project workspaces:**

If the repository contains multiple projects with separate `.ai/` folders, the root `AGENTS.md` should list them all:

```markdown
## Repository Structure

| Location | Project | Description |
|----------|---------|-------------|
| `project-a/.ai/` | Project A | [Brief description] |
| `project-b/.ai/` | Project B | [Brief description] |

Navigate to the appropriate `.ai/` folder for project-specific guidance.
```

**DEFAULT ACTION: Create AGENTS.md.** Only skip if:

- This is a subfolder `.ai/` setup (not at repo root) — e.g., `documents/report/.ai/`
- An `AGENTS.md` already exists at the repo root (consolidate into it instead)

**If you're bootstrapping at the repository root, you MUST create AGENTS.md.**

## Step 3b: Create Tool-Specific Bridge Files

`AGENTS.md` is the canonical entry point, but several major AI tools still discover context through their own native paths first. To maximize compatibility without duplicating content, create thin **pointer files** that redirect each tool to `AGENTS.md` and `.ai/`. Each bridge file should be one to a few lines — never copy substance.

**Create all three by default.** They are small, additive, and harmless. Skip a bridge only if the user explicitly opts out.

**1. GitHub Copilot bridge** — `.github/copilot-instructions.md`

```markdown
# Copilot Instructions

This repository's authoritative AI context lives in [`AGENTS.md`](../AGENTS.md) and the [`.ai/`](../.ai/) folder. Read those before generating suggestions.
```

**2. Cursor bridge** — `.cursor/rules/00-agents.mdc`

```markdown
---
description: Point Cursor at the canonical AGENTS.md and .ai/ context folder
alwaysApply: true
---

Authoritative AI context for this repository lives in `AGENTS.md` (repo root) and the `.ai/` folder. Read those first; do not rely solely on this file.
```

**3. Claude Code bridge** — `CLAUDE.md` at repo root

```markdown
# CLAUDE.md

Claude Code: read [`AGENTS.md`](AGENTS.md) and the [`.ai/`](.ai/) folder. They contain the authoritative project context, standing orders, and task register.
```

**Rules:**
- Each bridge file is a **pointer**, not a copy. If you find substantive content in any of them, it belongs in `.ai/` instead.
- If a tool's native path already exists with substantive content, consolidate it into `.ai/` per Step 1, then replace the original with a pointer.
- Do not use symlinks. Use plain Markdown files with relative links.
- Skip a bridge only if the user explicitly opts out, or if the tool's path is already a documented anti-pattern in the repo.

## Step 4: Populate from Scan

After creating all six files (five in `.ai/`, one at repo root):

1. Fill in the Author section by asking the user (or inferring if obvious)
2. Scan the repo structure and populate the project-specific sections
3. If consolidating existing files, preserve all useful content
4. Initialize `.ai/TODO.md` with current known tasks from chat/repo context
5. Inform the user: *"Productivity guardrails are enabled by default—AI will gently flag when work appears to be venturing into diminishing returns or scope creep. You can remove or customize this section in CONTEXT.md if you prefer."*
6. Apply deletion policy: remove pre-approved superseded `.ai/` meta-files directly; ask only for non-pre-approved destructive deletions

## Key Principles

- **Starting structure** — five files in `.ai/` (`README.md`, `CONTEXT.md`, `INSTRUCTIONS.md`, `TODO.md`, `GUESTBOOK.md`), one `AGENTS.md` at repo root; topic folders added when justified. **This is the private/single-dev default.** For shared or multi-developer repos, see "Shared vs. Private Repos" above: track only `AGENTS.md` (+ optional `.github/copilot-instructions.md`) and keep personal/ephemeral state in agent memory or a gitignored `.ai/local/`.
- **AGENTS.md is required** for repo-root bootstraps (skip only for subfolder `.ai/` setups)
- **GUESTBOOK.md is sign-once-per-model** — if any file in the repo was edited and your model+version is not already the last/any matching row, append one row; never edit or delete existing rows
- **UPPERCASE filenames** — `README.md`, `CONTEXT.md`, `INSTRUCTIONS.md`, `TODO.md`, `GUESTBOOK.md`, `AGENTS.md`
- **No symlinks** — use hard pointers (plain Markdown with links)
- **No timestamps** — git tracks history
- **Conflicts must be explicit** — silent precedence is dangerous
- **Precedence is mandatory** — if instructions conflict, follow the precedence section above
- **Integrate context proactively** — don't wait to be asked
- **No meta-commentary outside `.ai/`** — keep the repo clean
- **Tool bridges are pointers, not copies** — `.github/copilot-instructions.md`, `.cursor/rules/00-agents.mdc`, and `CLAUDE.md` should redirect to `AGENTS.md`/`.ai/`, never duplicate substance
- **Ensure `.ai/` is tracked by git in private/single-dev repos** — if new files don't appear in `git status`, check `.gitignore` for rules like `**/` that might exclude it. (In *shared* repos the opposite applies: a full `.ai/` is intentionally NOT tracked; only `AGENTS.md` is. See "Shared vs. Private Repos" above.)

## Standing Order: Monitor for Deprecated/Emerging Standards

The `.ai/` folder paradigm (introduced 2025–2026) is still evolving. AI agent tools, Git platforms, and documentation standards continue to develop at rapid pace.

**For AI agents bootstrapping new projects or maintaining existing `.ai/` folders:**

1. **Watch for deprecated practices.** If you encounter evidence that the BOOTSTRAP.md approach has been superseded by newer standards:
   - Note what newer pattern you encountered (tool, platform, documentation)
   - Flag this to the user explicitly: "I found that [newer standard] is now preferred over our current [BOOTSTRAP practice]"
   - Suggest updating the `.ai/` folder structure to match

2. **Watch for emerging conventions.** If you discover:
   - New tools looking for `.ai/` files or similar context folders
   - New standards for AI agent context (OWASP, emerging frameworks, platform updates)
   - Better practices for maintaining institutional memory across AI sessions

   Then document them and alert the user: "Current best practice appears to be [new approach]. Should we migrate?"

3. **Keep BOOTSTRAP.md current.** If the user approves an update:
   - Modify the relevant template or section
   - Document the change date and rationale
   - Propagate the correction to other projects using AGENTS.md references

4. **Do not block work.** This is informational. Flag concerns but don't prevent the user from asking you to proceed with current standards.

**Rationale:** The `.ai/` paradigm is new. Standards will shift. Better to know now than to discover in 2027 that everyone else migrated to a better system and we're still using 2025-era patterns.

## If You Made a Mistake

**Created lowercase files?** Don't try to rename them directly on case-insensitive filesystems. Use:
```bash
git mv context.md temp.md && git mv temp.md CONTEXT.md
```

**Files disappeared during rename?** Check for backup files (`*.md~`) and `git status`. Commit before risky operations.

**General rule:** When in doubt, commit first. Git is your safety net.

## After Setup

**Present a summary in the chat** so the user can review without reading the files. Then ask targeted questions.

### Template for post-setup summary:

```
I've created the `.ai/` folder. Here's my understanding of this project:

**Project:** [1-2 sentence description of what the project does]

**Key components:**
- [file/folder]: [what it does]
- [file/folder]: [what it does]
- [etc.]

**Author context I inferred:**
- Role: [what you guessed]
- Domain: [what you guessed]
- Primary tools: [languages, frameworks]

**Decisions/constraints I documented:**
- [any patterns, conventions, or constraints you noticed]

**AGENTS.md:** [Created at repo root / Consolidated into existing / Skipped (subfolder setup — explain why)]

**If `AGENTS.md` is missing and this is a repo-root bootstrap, go back and create it now.**

**Questions:**

1. Is this understanding correct? Any major mischaracterizations?
2. [If consolidating] I consolidated content from [list files] and applied deletion policy [deleted or archived paths].
3. What key decisions or constraints should I add? (e.g., "never touch X", "Y is deprecated", "Z is the pattern to follow")
```

Then integrate corrections into CONTEXT.md.

## Step 5: Review BOOTSTRAP.md Itself (if reading from file)

If you received these instructions as an attached file (not pasted into chat), review this file for issues you encountered during execution. **Do not edit the file.** Report only if you have substantive findings — "everything worked fine" is a valid and preferred outcome.

**Report-worthy:**
- Instructions that contradicted each other
- Steps you couldn't follow and had to interpret or skip
- Assumptions that didn't match this repo (e.g., assumed single-project but was monorepo)
- Guidance that was ambiguous enough to produce a wrong result

**Not report-worthy:**
- Prose style preferences
- Suggestions to reorganize or restructure sections
- "This could be clearer" without a concrete failure it caused
- Anything you'd phrase as "consider" or "you might want to"

The threshold is: *did this cause friction during execution, or would it cause friction for a different agent/model executing the same instructions on a different repo?* If no, say nothing. If yes, note it briefly.

---

**— END OF BOOTSTRAP INSTRUCTIONS —**

*If you don't see this line, the file was truncated. Request the complete file before proceeding.*
