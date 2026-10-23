# Context

**Purpose:** Facts, decisions, and history for AI agents working on this project.

---

## Author

**Name:** Jon Lighthall
**Role:** Systems engineer / configuration manager
**Organization:** Independent / multi-platform
**Domain:** Linux/Unix system administration, shell scripting, configuration management

**Primary tools:**
- Bash shell scripting
- Emacs (Lisp configuration)
- Git
- Cross-platform configuration (Windows, macOS, Linux, WSL)

**General preferences:**
- Impersonal, technical voice
- Avoid editorializing ("we believe")
- Minimal changes over extensive rewrites
- Preserve existing structure and patterns

**What NOT to do:**
- No meta-commentary outside `.ai/` files
- Don't suggest refactoring without understanding the cross-platform dependencies
- Don't modify `.bashrc*` files without checking compatibility across environments

**Productivity guardrail:**
- Watch for low-impact iteration (cosmetic tweaks) or tangents away from core configuration goals. If effort looks disproportionate, call it out briefly and suggest a more aligned path, deferring to the user.

---

## This Project: `config`

**Overview:**
Multi-platform user configuration repository containing shell profiles, Emacs settings, and environment-specific configurations for Windows, macOS, Linux, WSL, Cygwin, MinGW, and Git Bash.

**Purpose:**
- Centralized, version-controlled dotfiles and configurations
- Easy setup and linking across heterogeneous environments
- Separation of environment-specific settings by folder
- Integration with a separate private repository for sensitive data

**Repository structure:**

| Folder | Purpose |
|--------|---------|
| `linux/` | Linux-specific settings |
| `apple/` | macOS configuration |
| `windows/` | Windows native settings |
| `wsl/` | Windows Subsystem for Linux |
| `cygwin/` | Cygwin environment |
| `git_bash/` | Git Bash (MinGW) |
| `mingw/` | MinGW packages and setup |
| `nrl/` | Site-specific configuration (NRL) |
| `oracle/` | Site-specific configuration (Oracle Linux) |
| `private/` | Sensitive data (linked from `config_private` repo) |

**Key files:**

| File | Purpose |
|------|---------|
| `emacs_all.el` | Universal Emacs settings (loaded on all platforms) |
| `readme.md` | User documentation and setup instructions |
| `.bashrc*` | Bash profiles and aliases (environment-specific variants) |
| `lib_*.sh` | Bash utility libraries (colors, debugging, formatting, etc.) |
| `make_links.sh` | Symlink installation script (root level) |
| `get_repos.sh` | Clone and setup related repositories |
| `.bash_aliases*` | Bash aliases for various environments |

**Key scripts and their roles:**

- `make_links.sh` — Creates symlinks from home directory to config files in this repo. Each environment folder has its own `make_links.sh` for environment-specific setup.
- `get_repos.sh` — Clones dependent repositories (see referenced git_author.sh, lib_*.sh)
- `git_author.sh` — Configures git author info (stored in `nrl/` for site-specific overrides)
- `install_repos.sh` — Orchestrates repository cloning and setup

**Dependencies:**
- Git (required for cloning and version control)
- Bash (for all shell scripts)
- Emacs (optional, for Lisp configuration)
- SSH keys (required for SSH-based repository cloning)
- WSL VPN kit (for WSL connectivity if behind corporate VPN)

**Cross-platform constraints:**
- Paths differ significantly (Windows `\`, WSL `/`, macOS case-insensitive filesystem)
- Shell availability varies (Git Bash ≠ native Windows shell)
- Emacs version compatibility matters (26.1+ vs. 27.1+ for new features like `display-fill-column-indicator-mode`)
- SSH and credential handling differs per environment

**Known issues / patterns:**
- Emacs 26.1 doesn't support `display-fill-column-indicator-mode` (introduced in 27.1) — version checks required in `emacs_all.el`
- Private configurations are in a separate repository (`config_private`) and symlinked from `private/`
- Site-specific settings (NRL, Oracle) stored in dedicated folders
- Aliases for remote VPN connections stored in separate files (`bash_aliases_*`)
- Site-specific working context (including any API-integration or testing notes
  for the `nrl/` folder) is kept inside that folder's own scope (`nrl/.ai/*`),
  not in this parent repo. The `nrl/` folder is an independent, intentionally
  non-submodule repository and is not present on personal/home machines, so this
  repo's context must never depend on it.

**Recent changes:**
- Fixed Emacs version compatibility in `emacs_all.el` (version check for `display-fill-column-indicator-mode`)
- Added productivity guardrail to `.ai/CONTEXT.md` to flag low-impact or tangential work
- BOOTSTRAP.md bumped to 2026.10 (2026-10-07): added "Non-git synced folders" guidance, an optional `SOURCES.md` template for document-centric folders, an AGENTS.md version-stamp placeholder (was hard-coded 2026.04), five-file wording fixes, user-requested-edit carve-out in Step 5, and the preferred NRL org code in Author Defaults. Rationale is recorded in the file's own "Revision Notes" section. Triggered by bootstrapping the non-git NSPE/uBand 6.4 admin folder on Teams.
- BOOTSTRAP.md bumped to 2026.10.1 (2026-10-22): Author Defaults no longer conflict with Action Mode (populate, then confirm in the post-setup summary); GUESTBOOK template points to `.ai/INSTRUCTIONS.md` instead of BOOTSTRAP.md; new "Nested git repos" guidance for synced folders; `.N` same-month version suffix; minor fixes. Triggered by bootstrapping the 2026-11 travel folder and the LURCRSS project folder (OneDrive).

---

## Linux bash startup chain (verified 2026-10-23, Rocky 8.10, bash 4.4, VS Code Remote-SSH)

Goal: identical shell behavior whether a remote Linux host is reached by `ssh -X` from
WSL (login shell) or through a VS Code Remote-SSH terminal (non-login by default).

- `ssh` login: `/etc/profile` (-> `/etc/profile.d/*`, `/etc/bashrc`) -> `~/.bash_profile`
  -> `linux/.bash_profile` sources `linux/.bashrc` directly -> greeting at end of profile.
- VS Code terminal: `bash --init-file .../shellIntegration-bash.sh`. Bash itself reads no
  rc file; the script sources `~/.bashrc`. If the terminal profile has `-l`, VS Code sets
  `VSCODE_SHELL_LOGIN=1` and the script sources `/etc/profile` + `~/.bash_profile`
  instead, but `shopt -q login_shell` is still FALSE (do not use it to detect this case).
- Hidden run: on server start the VS Code server resolves the extension-host env with
  `bash -i -l -c` (no tty, `VSCODE_RESOLVING_ENVIRONMENT=1`). This runs `~/.bash_profile`
  invisibly (LOGIN stamps appear in `.bash_history` with no matching `last` entry), and its
  exported vars (PATH, VB, DEBUG, TAB, fTAB, LC_COLLATE) are inherited by every VS Code
  terminal. That is why VS Code terminals report `~/bin already in PATH`.
- VS Code terminals have no DISPLAY; `linux/.bashrc_X11` borrows an ssh-forwarded display
  from a concurrent `ssh -X` session. The resolv.conf nameserver probe in that file is a
  WSL2 idiom (nameserver = Windows host) and is meaningless on Linux servers.
- RHEL/Rocky 8 default `~/.bashrc` only sources `/etc/bashrc` and prepends
  `~/.local/bin:~/bin`; `linux/.bashrc` already does both (but appends `~/.local/bin`), so
  symlinking `~/.bashrc` -> `linux/.bashrc` loses nothing; `linux/make_links.sh` links it.
- **Decision (2026-10-23):** the difference comes from VS Code, so it is fixed with VS Code
  configuration. `linux/vscode_machine_settings.jsonc` (linked by `linux/make_links.sh` to
  `~/.vscode-server/data/Machine/settings.json`) makes the default Linux terminal profile
  `bash -l`, disables automatic shell integration (`--init-file` made the shell non-login and
  added a level to BASH_SOURCE, which shifted all depth-based TAB math), and nulls `SHLVL`
  (server chain passed 4; `set_tab_shell`/`print_` indent by SHLVL) plus the captured
  session vars `DEBUG VB TAB fTAB FPRETTY_LOADED` (inherited `DEBUG=0` defeated the
  `${DEBUG=1}` default). Result: VS Code terminals match ssh logins exactly (user-confirmed).
  `linux/.bash_profile` and `linux/.bashrc` (non-login only) load shell integration manually,
  last, via `code --locate-shell-integration-path bash` (needs `VSCODE_IPC_HOOK_CLI`; prints
  an error to stdout with exit 0 elsewhere, hence the `-r` check; do not source it inside a
  function, its `declare -A` would become local). `linux/.bash_profile` / `linux/.bashrc_X11`
  short-circuit when `VSCODE_RESOLVING_ENVIRONMENT` is set; the profile guard un-exports the
  session vars. `VSCODE_SHELL_LOGIN` checks remain as a fallback for hosts where injection is
  still on. `~/.bashrc` is a link, not a hand-edited stub.
- **Gotcha (2026-10-23):** `linux/make_links.sh` also links `~/.claude` -> `linux/.claude`.
  On a host where `~/.claude` is a real directory it is renamed with a date suffix, which
  strands Claude Code's credentials, transcripts and memory (and a running session starts
  writing runtime state into the repo dir, gitignored). On caustic this was reverted and
  merged back; migrate deliberately (Claude Code closed) or link only the tracked items.
- `make_links.sh` fails under a non-tty (e.g. an agent's Bash tool) at `tput cols`
  (`lib_links.sh:337`); run with `TERM=xterm-256color COLUMNS=120`.

---

## Superseded Decisions

None documented yet.
