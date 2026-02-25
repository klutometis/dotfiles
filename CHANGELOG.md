# Changelog

## 2026-02-25

### Added `--remote` flag to `terminal`

**Changes**:
- Added `--remote HOST` flag to `~/bin/terminal` for SSH-backed tmux sessions
- Window title reflects remote context: `terminal (SESSION @ HOST)`
- Local sessions unchanged: `terminal (SESSION)`
- SSH config (`~/.ssh/config`) used for host aliases, ports, keys, etc.

**Rationale**:
Eliminates tmux-within-tmux when working on remote machines. The remote tmux session feels local — no nested prefix (C-z C-z, etc.) since only one tmux is involved. The `@ HOST` in the title is the only indicator that you're remote.

**Files affected**:
- `~/bin/terminal`

## 2026-02-19

### Switched eat terminal backend; C-g → ESC in eat for OpenCode

**Changes**:
- Switched `ai-code` terminal backend from `vterm` to `eat` (`ai-code-backends-infra-terminal-backend 'eat`)
- Added `eat-send-escape` helper and bound `C-g` → ESC in both `eat-semi-char-mode-map` and `eat-char-mode-map`

**Rationale**:
`eat` handles mouse passthrough and ESC sequences more cleanly than `vterm`. In `eat`, `esc esc` → single ESC and `C-q esc` → ESC, so OpenCode's double-ESC interrupt requires `C-g C-g` (two keystrokes) rather than the four-ESC sequence needed without this binding. The `vterm` equivalent (`C-g` → `vterm-send-escape`) is preserved for fallback use.

**Files affected**:
- `~/etc/dotfiles/dot-emacs.d/init.el`

## 2026-01-28

### Refactored `opencode` wrapper script

**Changes**:
- Replaced `opencode-personal` and `opencode-work` scripts with a single `opencode` script.
- The new script uses `shflags` for command-line flag parsing.
- The script is structured with a `main()` function for better readability and maintainability.
- Added support for `--profile`, `--model`, and `--config` flags.
- The `work` profile defaults to the work-specific config and model, which can be overridden by flags.
- Updated `~/.emacs.d/init.el` to use the new `opencode` script.

**Rationale**:
A single script with flags is more flexible and scalable than multiple scripts. The use of `shflags` makes the script more robust and easier to maintain. The new structure with a `main()` function is a good practice for shell scripting.

**Files affected**:
- `~/bin/opencode` - new script
- `~/bin/opencode-personal` - removed
- `~/bin/opencode-work` - removed
- `~/.emacs.d/init.el` - updated to use the new script.

## 2026-01-25

### Migrated from straight.el to Elpaca Package Manager

**Changes**:
- Replaced straight.el with Elpaca for package management
- Added `use-package-always-ensure t` to auto-install all packages (equivalent to `straight-use-package-by-default`)
- Added `:ensure nil` to all built-in packages (ansi-color, auth-source-pass, compile, combobulate, dired, eglot, emacs, find-dired, flymake, frame, grep, midnight, pulse, savehist, sort, windmove, winner)
- Added custom recipe for combobulate (`:ensure (:host github :repo "mickeynp/combobulate")`)
- Configured auto-update to run daily at midnight via `midnight-hook`
- Removed unused packages: keyfreq, yasnippet, yasnippet-snippets, exec-path-from-shell, xclip, claude-code, monet, mcp, eat (kept vterm)
- Fixed consult error by removing obsolete `consult--source-*` variables from `consult-customize`

**Rationale**:
- **Elpaca**: Faster parallel package installation vs straight.el's serial approach
- **Package cleanup**: Removed ~2400 yasnippet files and other unused bloat for faster startup
- **Auto-updates**: Daily midnight updates keep packages current without manual intervention
- **Built-in annotations**: Explicit `:ensure nil` makes it clear which packages are built-in vs external

**Files affected**:
- `~/.emacs.d/init.el` - Complete package manager migration
- `~/.emacs.d/early-init.el` - Added lexical-binding cookie
- Created backup: `~/.emacs.d/init.el.straight-backup-20260125-032933`

### Enhanced Dired, Savehist, and File Management

**Changes**:
- Enhanced `dired` configuration: dwim-target (two-pane operations), group-directories-first, auto-refresh
- Added `dired-narrow` package with `/` binding for fuzzy filtering in dired buffers
- Added `dired-rainbow` package for color-coded file types (source/media/archives/etc.)
- Enhanced `savehist` with 1000-item history and consult-specific histories (grep/find/line/buffer)
- Verified and enhanced `embark-consult` integration (removed duplicate embark declaration)
- Enabled `global-auto-revert-mode` for automatic buffer refresh when files change
- Removed `revert-buffer` keybinding (obsoleted by global-auto-revert-mode)
- Replaced `better-defaults` with explicit configuration for transparency
- Fixed clipetty on local machine (OSC 52 passthrough through tmux to Alacritty)

**Rationale**:
- **dired enhancements**: dwim-target makes two-pane copy/move operations intuitive, dired-narrow enables quick filtering, dired-rainbow improves visual organization
- **savehist**: Increased history enables better recall of searches and commands across sessions
- **global-auto-revert**: Eliminates manual revert-buffer calls when files change externally (git, builds, etc.)
- **better-defaults removal**: All settings now explicit and visible in config (no black boxes)
- **clipetty fix**: tmux wasn't passing OSC 52 sequences; `allow-passthrough on` + `clipetty-assume-nested-mux t` fixes local clipboard integration

**What better-defaults provided (now explicit)**:
- UI cleanup (no toolbars/scrollbars/menu-bar)
- show-paren-mode, save-place-mode, uniquify
- Better default settings (case-insensitive completion, backup-by-copying, indent with spaces, etc.)

**Files affected**:
- `~/etc/dotfiles/dot-emacs.d/init.el`
- `~/.tmux.conf`

### Removed Unused AI Packages and Reclaimed Keybinding

**Changes**:
- Removed `aidermacs` package (86 lines including custom functions)
- Removed `gptel` package (86 lines including backend configuration and helper functions)
- Added `C-c A` keybinding to `agent-shell` (reclaimed from aidermacs)

**Rationale**: 
Both `aidermacs` and `gptel` have been supplanted by `agent-shell` in the workflow. Removing them simplifies configuration and reclaims valuable keybindings. `C-c A` is now available for quick access to agent-shell, which provides a unified interface for Claude Code, Gemini CLI, and Codex via ACP.

**Files affected**:
- `~/etc/dotfiles/dot-emacs.d/init.el`

### Removed Legacy Packages and Added Modern Tooling

**Changes**:
- Removed `auto-package-update` package (incompatible with straight.el workflow)
- Replaced `flycheck` with `flymake` for syntax checking
- Replaced `desktop-save-mode` with `activities.el` for unified session/workspace management
- Added `wgrep` package for editing grep/ripgrep results in-place
- Added keybindings for `helpful` package (enhanced help system)
- Rebound `C-h` from `kill-whole-line` to restore standard help prefix
- Moved `kill-whole-line` to `C-c k` (consistent with `C-c u` for kill-line-backward)

**Rationale**: 
- `auto-package-update` only works with package.el; straight.el already handles updates via `straight-pull-all`
- `flymake` is built into Emacs 29+ and integrates natively with Eglot (unlike flycheck)
- `activities.el` supersedes both desktop+ and burly.el, providing unified activity management (suspend/resume workspaces) with automatic state saving, tab-bar integration, and bookmark-based buffer restoration for both file and special buffers
- `helpful` provides superior help buffers with source code, references, and better formatting
- `wgrep` enables direct editing of search results, essential for modern refactoring workflows
- Restoring `C-h` prefix enables all standard help commands and helpful's enhanced interface

**New keybindings**:
- `C-h f/v/k/x` - Enhanced help via helpful (function/variable/key/command)
- `C-c k` - kill-whole-line (moved from C-h)
- `C-c ! n/p/l` - Flymake navigation (next/previous error, list diagnostics)
- `C-c C-p` (in grep buffers) - Edit results with wgrep
- `C-c C-c` (in wgrep) - Apply changes to files
- `C-x C-a C-d/C-a/C-s/C-k/g/l/b` - Activities commands (define/resume/suspend/kill/revert/list/switch-buffer)

**Files affected**:
- `~/etc/dotfiles/dot-emacs.d/init.el`

**Documentation updated**:
- Moved `var/doc/emacs.md` → `notes/emacs.md` (flat structure per CLAUDE.md)
- Updated CLAUDE.md to use flat structure for notes/todo/changelog archives (date patterns self-document)

## 2026-01-24

### Consolidated Emacs Configuration into Single File

**Changes**:
- Merged `~/etc/dotfiles/dot-emacs.d/init-settings.el` into `init.el`
- All global settings, helper functions, and keybindings now in `(use-package emacs)` stanza
- Removed obsolete `(load "~/.emacs.d/init-settings.el")` statement
- Added missing function definitions: `copy-line` and `kill-line-backward`
- Removed duplicate `C-c C-h` keybinding (kept only `C-c h` for help)
- Removed unused `org-store-link` binding (not using org mode)
- Fixed `consult-apropos` autoload error by changing to built-in `apropos`

**Rationale**: Single-file configuration improves discoverability and eliminates the need to know about separate settings files. All global configuration is now in one logical place within the `emacs` package stanza.

**Files affected**:
- `~/etc/dotfiles/dot-emacs.d/init.el` - merged and cleaned up
- `~/etc/dotfiles/dot-emacs.d/init-settings.el` - no longer loaded (kept as backup)


### Modernized Auto-Completion Stack

**Changes**:
- Replaced `company-mode` with `corfu` (fast, capf-native completion UI)
- Replaced commented-out `lsp-mode` with `eglot` (built-in LSP client)
- Added `cape` for completion source stacking (LSP + files + spell + dabbrev)
- Configured eglot for C++, Python, TypeScript, JavaScript, and Shell modes
- Auto-completion now triggers after 2 characters with 0.1s delay

**Rationale**: Corfu + eglot provides a modern, lightweight completion stack that integrates seamlessly with Vertico/Consult. Eglot is built into Emacs 29+ and requires minimal configuration. Cape allows stacking multiple completion sources (LSP, files, spell checking) in a single unified interface.

**What you get**:
- Popup completions with TAB/S-TAB navigation (vs hippie-expand's cycling)
- LSP-aware completions for supported languages
- File path completions
- Spell-check suggestions integrated into completions
- Dynamic abbreviations from open buffers

**Language servers required**:
- C/C++: `clangd`
- Python: `python-lsp-server` or `pyright`
- TypeScript: `typescript-language-server`
- Bash: `bash-language-server`


### Added Hermetic Language Server Installation

**Changes**:
- Updated `~/bin/configure-system.sh` to install language servers hermetically
- Replaced `nvm` with `mise` (Rust-based, 3-40x faster, polyglot version manager)
- Added `uv` for Python tool installation (10-100x faster than pip, proper isolation)
- Automated installation of: clangd, python-lsp-server, typescript-language-server, bash-language-server
- Updated `~/.zshenv` to initialize mise instead of nvm/pyenv

**Rationale**: Hermetic installation prevents system contamination and sudo requirements. Modern tools (uv, mise) are significantly faster and provide better isolation than traditional package managers (pip, nvm, pyenv).

**Language servers installed**:
- C/C++: `clangd` (via apt, with clang-tidy and IWYU)
- Python: `python-lsp-server` (via uv tool)
- TypeScript: `typescript-language-server` (via mise + npm)
- Bash: `bash-language-server` (via mise + npm)

**Benefits**:
- No system package conflicts
- No sudo needed (except for clangd)
- Faster installation and updates
- Easy per-project version management (with mise)
- Integrates automatically with Emacs eglot

**Documentation**: See `~/var/doc/emacs.md` for Emacs usage and `~/var/build/emacs.md` for build instructions


### Enhanced Consult Search with Preview

**Changes**:
- Added `C-c R` keybinding for `consult-ripgrep` from current directory (`C-c r` still searches from project root)
- Enabled automatic preview for ripgrep/grep results (0.2s debounce to avoid flickering)
- Enabled instant preview for yank-ring (`M-y`) - restores Helm-like behavior
- Added `consult-customize` configuration for preview across all search commands

**Rationale**: Addresses usability issues from Helm migration. Users needed `C-u C-c r` to search current directory (now just `C-c R`). Preview functionality replicates Helm's useful "see before you jump" behavior while maintaining Vertico's speed and clean interface.

**Usage**:
- `C-c R` - ripgrep from current directory
- `C-c r` - ripgrep from project root
- Navigate with `C-n`/`C-p` to see automatic preview
- Type more terms to narrow results (space-separated)
- Use `!word` to exclude matches containing "word"


### Replaced Helm with Vertico/Consult Completion Stack

**Changes**:
- Replaced Helm with modern Vertico/Consult/Marginalia/Embark stack in `~/etc/dotfiles/dot-emacs.d/init.el`
- Updated savehist configuration to use Vertico/Consult history variables
- Removed helm-specific history variables (`helm-M-x-input-history`, `helm-find-files-history`, `helm-grep-history`)
- Added `file-name-history` and `consult--grep-history` to persist search patterns across sessions

**Rationale**: Vertico/Consult provides a more modular, faster, and actively maintained completion framework compared to Helm. Uses standard Emacs completion APIs.

### Fixed ikill Self-Termination Bug

**Problem**: `~/bin/ikill` would occasionally kill itself when matching patterns, especially when it appeared first in the process list.

**Changes**:
- Added filtering to exclude processes containing 'ikill' in their command line
- Filters out current process ID (`$$`) from results
- Prevents premature termination when ikill matches its own search pattern

**Rationale**: Self-termination prevented ikill from completing its job. Filtering ensures only target processes are affected.

### Added NPM Registry Configuration

**Changes**:
- Added `NPM_CONFIG_REGISTRY=https://registry.npmjs.org/` to `~/.zshenv`

**Rationale**: Explicitly sets npm to use the official registry across all shells and npm operations.

### Added Git Submodule Update Script

**Problem**: `git submodule update --remote` leaves submodules in detached HEAD state, requiring manual branch checkout.

**Changes**:
- Created `~/bin/git-update-submodules` script
- Updates all submodules with `--merge` flag to avoid detached HEAD
- Automatically detects and checks out correct branch (main/master) for each submodule
- Handles mixed main/master repositories gracefully

**Rationale**: Automates the tedious process of updating submodules and ensuring they're on proper branches. Works for all present and future submodules without manual configuration.

## 2026-01-16

### Added Go Toolchain and Bluetuith Bluetooth Manager

**Changes**:
- Added Go installation section to `configure-system.sh`
- Added bluetuith installation via `go install github.com/darkhz/bluetuith@latest`
- Go version fetched dynamically from official API (no hardcoded versions)

**Details**:
- Go installed to `/usr/local/go` (standard location)
- Bluetuith binary placed in `~/go/bin`
- Both `/usr/local/go/bin` and `~/go/bin` added to PATH
- Installations only run if commands not already present

**Rationale**: Bluetuith is a terminal-based Bluetooth manager that provides better control than GUI tools. Go installation method ensures cross-distro compatibility and always-latest versions.

## 2026-01-13

### Removed Unstable TigerVNC X11 Extension

**Problem**: X server crashing 5-6 times daily due to TigerVNC X11 extension.

**Changes**:
- Removed `/etc/X11/xorg.conf.d/10-vnc.conf` (manually created config)
- Purged `tigervnc-xorg-extension` package
- Removed X11/udev keyboard configuration sections from `configure-system.sh` (replaced by `~/bin/watch-keyboard-hotplug.sh`)

**Rationale**: VNC extension running inside X server process was too unstable for daily use. Will use external VNC server (e.g., `x11vnc`) when needed - external servers are isolated from X server, so crashes don't bring down the desktop.

### Cleaned Up configure-system.sh

**Changes**:
- Removed `ENABLE_KEYBOARD_CONFIG` flag (no longer needed)
- Removed X11 configuration deployment section
- Removed udev rules deployment section
- Script now automatically reloads sxhkd if running (instead of instructing user)

**Rationale**: Keyboard configuration now handled by event-driven `~/bin/watch-keyboard-hotplug.sh`. Old X11/udev approach was replaced.
