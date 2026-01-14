# Instructions

**Purpose:** Procedures and standing orders for AI agents working on this project.

---

## Quick Start

1. Read this file for procedures
2. Read `CONTEXT.md` for project facts and decisions
3. Ask clarifying questions if the project state is unclear

---

## Context Maintenance (Standing Order)

When the user provides substantial clarifying information, **integrate it into the appropriate `.ai/` file without being asked**.

### Where to put new information:

| Type of Information | Destination |
|---------------------|-------------|
| Project-wide decisions, facts, history | `.ai/CONTEXT.md` |
| Project-wide procedures, standing orders | `.ai/INSTRUCTIONS.md` |

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

### Handling deprecated/superseded information:

When new decisions override old ones:

```markdown
### [Decision Name]
**Current:** [What we do now]

**Superseded:** Previously we tried [X] but switched because [reason].
```

Preserve the old approach if it explains *why* we don't do something (prevents re-asking).

---

## General Quality Standards

### Before Editing:
1. Verify cross-platform compatibility — this repo spans Windows, macOS, Linux, WSL, Cygwin, MinGW, and Git Bash
2. Check the environment-specific folder if the change is location-specific
3. Understand symlink strategy before modifying linking logic
4. For Emacs Lisp: verify version compatibility (test against documented minimum version)

### After Editing:
1. Verify the edit works as intended (run scripts if applicable)
2. Check that you didn't break cross-platform compatibility
3. Update `CONTEXT.md` if you made decisions that should persist
4. Verify symlinks and paths still resolve correctly on the target platform

### When Uncertain:
- Ask clarifying questions about which environment(s) the change affects
- Document assumptions in `CONTEXT.md`
- Prefer minimal, targeted changes over extensive rewrites
- Test on multiple platforms if possible (at minimum, document which platform(s) were tested)

---

## This Project: Specific Procedures

### Emacs Configuration

**File:** `emacs_all.el` (universal, loaded on all platforms)

**Principles:**
- Code must be compatible with Emacs 26.1 (minimum tested version)
- Features introduced in newer versions (27.1+, 28.0+) must be guarded with version checks
- Use `(when (>= emacs-major-version X) ...)` for version-specific features
- Keep Lisp style consistent with existing code (defun, hooks, custom-set-variables)

**Version check template:**
```elisp
;; display-fill-column-indicator-mode is only available in Emacs 27.1+
(when (>= emacs-major-version 27)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (set-face-foreground 'fill-column-indicator "color-253"))
```

### Shell Scripts

**Files:** `make_links.sh`, `get_repos.sh`, `install_repos.sh`, environment-specific `make_links.sh` in each folder

**Principles:**
- Must work in Bash (don't assume Zsh or other shells)
- Environment-specific scripts should be in dedicated folders (e.g., `wsl/make_links.sh`, `windows/make_links.sh`)
- Symlinks must be created with proper path handling (different on Windows vs. Unix)
- Library files (`lib_*.sh`) provide common functionality — use them

**Cross-platform path handling:**
- WSL: Use `$(wslpath -w ...)` for Windows path conversion
- Cygwin: Similar path conversion needed
- Native Windows: Use native path separators and command tools

### Bash Configuration

**Files:** `.bashrc*`, `.bash_aliases*`, and variants for specific environments

**Principles:**
- Global settings go in `.bashrc_common`
- Environment-specific settings go in `.bashrc_[environment]` or `.bash_aliases_[environment]`
- Aliases for remote/VPN connections keep the `_remote` or `_vpn` suffix (e.g., `.bash_aliases_anl_remote`)
- Use utility libraries from `lib_*.sh`

### Linking Strategy

**Principle:** Configuration files are stored in this repository and symlinked to the user's home directory.

**Process:**
1. Run `make_links.sh` in the appropriate environment folder
2. Script creates symlinks from `~/.bashrc` → `~/config/.bashrc_[environment]`, etc.
3. Private configs symlinked from separate `config_private` repository

**When modifying linking logic:**
- Test that symlinks resolve correctly on target platform
- Verify that both absolute and relative path resolution work
- Document any platform-specific path handling

### Repository Integration

This repository depends on other git repositories. See `get_repos.sh` for the cloning procedure.

**Process:**
1. User clones `config` repository
2. User runs `./get_repos.sh` to clone dependent repos
3. `install_repos.sh` orchestrates setup

**Don't assume** other repositories are already present. Check for their existence before using them.

---

## Error Handling & Diagnostics

### For Shell Scripts:

Use the `lib_*.sh` utilities (already available in this repo):
- `lib_dbg.sh` — Debug output and error reporting
- `lib_cond_echo.sh` — Conditional output based on verbosity
- `lib_traps.sh` — Trap handling for error management

### For Emacs Lisp:

- Add comments explaining non-obvious version checks or compatibility decisions
- Use `eval-and-compile` if version-specific code needs to run at compile time
- Test on minimum supported version if possible

---

## When Making Recommendations

If the user asks for suggestions or best practices:

1. **Respect existing patterns.** This repo has established conventions across multiple platforms and decades of configuration management. Don't suggest architectural overhauls.

2. **Stay within scope.** This is a configuration repository, not a general shell scripting framework. Improvements should be pragmatic and localized.

3. **Document trade-offs.** If suggesting a change that affects multiple platforms, explain what works on each platform and what doesn't.

4. **Prioritize compatibility.** New features are fine; breaking existing functionality across any supported environment is not.

---

## Changelog Maintenance

**Skip for this project** — the repository is configuration, not versioned software. Use git commit messages to document changes.

---

*Last updated: 2026-01-13*
