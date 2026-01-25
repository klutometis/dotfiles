# Emacs Configuration & Usage

*Last updated: 2026-01-25*

## Modern Stack (2025-2026)

### Completion Framework (Minibuffer)
- **Vertico** - Vertical completion UI
- **Consult** - Enhanced commands (grep, find, etc.)
- **Marginalia** - Rich annotations
- **Embark** - Contextual actions (C-.)
- **Orderless** - Flexible matching (space-separated, out-of-order)

### Auto-Completion (In-Buffer)
- **Corfu** - Fast popup completions (capf-native)
- **Cape** - Completion source stacking
- **Eglot** - Built-in LSP client (Emacs 29+)

### Diagnostics & Help
- **Flymake** - Built-in syntax/error checking (integrates with Eglot)
- **Helpful** - Enhanced help system with source code and references
- **Wgrep** - Edit grep/ripgrep results directly and apply to files

### Session & Workspace Management
- **activities.el** - Unified activity management (suspend/resume named workspaces with automatic state saving)

### Key Bindings

#### Completion & Navigation
- `TAB` / `S-TAB` - Navigate corfu completions
- `C-M-i` / `M-TAB` - Manual completion-at-point
- `M-/` - dabbrev-expand (still available)
- `C-.` - Embark actions on candidate
- `<` - Narrow in consult (e.g., in consult-buffer)

#### Search & Find
- `C-c r` - consult-ripgrep (search from project root)
- `C-c R` - consult-ripgrep-current-dir (search from current directory)
- `C-c f` - consult-find (find files)
- `C-c h o` - consult-line (search current buffer)
- `C-c o` - occur (standard occur)
- `C-o` (in isearch) - occur from search

**Ripgrep workflow:**
- Type search term, then add more to narrow (space-separated)
- `C-n`/`C-p` - Navigate results (automatic preview!)
- `M-<`/`M->` - Jump to first/last result
- Type `!word` to exclude lines containing "word"
- `RET` - Jump to match
- `C-.` - Embark actions (export to buffer, save, etc.)

#### Buffers & Files
- `C-x b` - consult-buffer (enhanced buffer switching)
- `C-x C-f` - find-file (enhanced by Vertico)
- `C-x C-r` - consult-recent-file
- `M-y` - consult-yank-pop (kill ring)

#### Code Navigation (Eglot)
- `M-.` - xref-find-definitions (jump to definition)
- `M-,` - xref-pop-marker-stack (go back)
- `M-?` - xref-find-references
- `C-c l r` - eglot-rename
- `C-c l a` - eglot-code-actions
- `C-c l f` - eglot-format

#### Help System
- `C-h f` - helpful-callable (describe function with source)
- `C-h v` - helpful-variable (describe variable)
- `C-h k` - helpful-key (describe keybinding)
- `C-h x` - helpful-command (describe command)

#### Diagnostics (Flymake)
- `C-c ! n` - Next error/warning
- `C-c ! p` - Previous error/warning
- `C-c ! l` - List all diagnostics in buffer

#### Editing Search Results (Wgrep)
- `C-c C-p` (in grep buffer) - Make results editable
- Edit the results directly in the buffer
- `C-c C-c` - Apply all changes to source files
- `C-c C-k` - Abort changes

#### Custom Helpers
- `C-a` - smarter-move-beginning-of-line (toggles indent/bol)
- `M-o` - smart-open-line (below)
- `C-o` - smart-open-line-above
- `C-c C-k` - copy-line
- `C-c u` - kill-line-backward
- `C-c k` - kill-whole-line
- `C-c P` - copy-file-name-to-clipboard
- `C-c m` - chmod-current-file

#### Activity Management (Workspaces)
- `C-x C-a C-n` - New empty activity
- `C-x C-a C-d` - Define activity from current window config
- `C-x C-a C-a` - Resume activity (where you left off)
- `C-x C-a C-s` - Suspend activity (save state, close tab)
- `C-x C-a C-k` - Kill activity (discard state, close tab)
- `C-x C-a RET` - Switch to active activity
- `C-x C-a b` - Switch to buffer within activity
- `C-x C-a g` - Revert activity to default state
- `C-x C-a l` - List all activities

#### Window Management
- `C-x o` - ace-window (quick window switching)
- `<up>/<down>/<left>/<right>` - windmove (move between windows)
- `C-c w t` - transpose-frame
- `C-c w f/F` - flip/flop-frame
- `C-c w r/c/a` - rotate-frame (clockwise/anticlockwise)

#### Avy (Jump to Anywhere)
- `C-c a c` - avy-goto-char
- `C-c a C` - avy-goto-char-2
- `C-c a t` - avy-goto-char-timer
- `C-c a l` - avy-goto-line
- `C-c a w` - avy-goto-word-1
- `C-c a e` - avy-goto-word-0

#### Git (Magit)
- `C-c g` - magit-status

#### AI Tools
- `C-c A` - agent-shell (unified interface for Claude Code, Gemini CLI, and Codex via ACP)

## Language Servers (Eglot)

### Installed Servers
- **C/C++**: clangd (with clang-tidy, IWYU)
- **Python**: python-lsp-server (pylsp)
- **TypeScript/JavaScript**: typescript-language-server
- **Bash**: bash-language-server

### Auto-Enabled Modes
Eglot starts automatically in:
- c++-mode, c-mode
- python-mode, python-ts-mode
- typescript-mode, typescript-ts-mode, js-mode
- sh-mode

### Checking Status
```elisp
M-x eglot  ;; Shows connection status
M-x eglot-events-buffer  ;; Debug LSP communication
M-x eglot-stderr-buffer  ;; Server error logs
```

### Manual Control
```elisp
M-x eglot  ;; Start eglot in current buffer
M-x eglot-shutdown  ;; Stop server for current buffer
M-x eglot-reconnect  ;; Restart server
```

## Clipboard Integration

### Clipetty (OSC 52)
System clipboard integration via OSC 52 escape sequences - works both locally and over SSH.

**How it works**:
- Emacs → clipetty → OSC 52 → tmux (with `allow-passthrough`) → Alacritty → system clipboard
- Copies work transparently - no special commands needed
- Normal Emacs kill-ring operations (`C-k`, `M-w`, etc.) sync to system clipboard

**Configuration**:
- **Emacs**: `clipetty-assume-nested-mux t` forces OSC 52 usage even on local terminal
- **tmux**: `allow-passthrough on` passes OSC 52 sequences to Alacritty
- **Alacritty**: Native OSC 52 support (no config needed)

**Fallback (xclip)**:
- `M-C-w` - Copy selection to clipboard via xclip
- `M-C-y` - Paste from clipboard via xclip
- Only needed if clipetty fails

## Completion Sources (Cape)

Auto-completion draws from multiple sources:
1. **Eglot** (LSP) - Language-aware completions (when LSP active)
2. **cape-file** - File path completions
3. **cape-dabbrev** - Dynamic abbreviations from open buffers
4. **cape-ispell** - Spell-check suggestions

## Preview Features (Like Helm!)

Consult provides automatic preview as you navigate:

**Ripgrep/Grep:**
- Automatic preview with 0.2s debounce (avoids flickering)
- See the match context as you navigate with `C-n`/`C-p`
- No need to jump to each file - preview shows you the content

**Yank-ring (M-y):**
- Instant preview of kill-ring entries
- Navigate with `C-n`/`C-p` to see what each entry contains
- Just like Helm's kill-ring interface!

**Buffers/Files:**
- Preview files and buffers as you navigate
- Press `RET` only when you've found what you want

**Vertico shows candidates vertically:**
- Default: 20 candidates visible at once
- Not one-at-a-time cycling - you see the full list!
- If you only see one candidate, Vertico isn't working properly

## Activities Workflow

**Think of activities like i3 workspaces for Emacs:**
- Each activity = named workspace with window layout + buffers
- Two states: default (clean entry point) and last (where you left off)
- Auto-saves when idle and on exit

**Typical workflow:**
1. Arrange windows/buffers for a task (e.g., Python project with code + shell + tests)
2. `C-x C-a C-d` - Define activity, name it "Python Work"
3. Work for a while, rearrange things, make progress
4. `C-x C-a C-s` - Suspend activity (saves state, closes tab)
5. Later: `C-x C-a C-a` - Resume "Python Work" (exactly where you left off!)
6. `C-x C-a g` - Revert to default state (clean slate)
7. `C-u C-x C-a C-d` - Redefine default state to current layout

**Buffer scoping:**
- `C-x C-a b` - Switch to buffer within current activity only (filters by activity!)
- Compilation, shell, and terminal buffers restore via bookmark system

## Configuration File

**Location**: `~/etc/dotfiles/dot-emacs.d/init.el`

**Structure**:
- Single file configuration (init-settings.el merged in)
- All packages managed by **Elpaca** (migrated from straight.el 2026-01-25)
- Auto-updates daily at midnight via `midnight-mode`
- Global settings in `(use-package emacs ...)` stanza

## Hermetic Language Server Installation

See: `~/bin/configure-system.sh`

Language servers installed via hermetic, isolated tools:
- **uv** - Python package manager (10-100x faster than pip)
- **mise** - Polyglot version manager (replaces nvm/pyenv)
- No system contamination, no sudo needed

## Tips & Tricks

### Finding Commands
```elisp
M-x consult-apropos  ;; Search commands (enhanced)
C-h k <key>  ;; Describe key binding
C-h f <func>  ;; Describe function
C-h v <var>  ;; Describe variable
```

### Checking Completion Sources
```elisp
M-: completion-at-point-functions RET
;; Shows active completion sources
```

### Eglot Performance
If eglot feels slow:
```elisp
M-x eglot-shutdown  ;; Restart the language server
M-x eglot-reconnect
```

### Corfu Configuration
```elisp
;; In init.el, corfu settings:
(corfu-auto t)                 ;; Auto-trigger
(corfu-auto-delay 0.1)         ;; Delay (seconds)
(corfu-auto-prefix 2)          ;; Min characters to trigger
```

## Recent Changes

### 2026-01-25: Migrated to Elpaca Package Manager
- Migrated from `straight.el` to `elpaca` for faster parallel package installation
- Removed unused packages: keyfreq, yasnippet, yasnippet-snippets, exec-path-from-shell, xclip, claude-code, monet, mcp, eat
- Kept vterm for terminal emulation
- Added `:ensure nil` to all built-in packages for clarity
- Added custom recipe for combobulate (`:ensure (:host github :repo "mickeynp/combobulate")`)
- Configured auto-update to run daily at midnight
- Fixed consult error by removing obsolete `consult--source-*` variables
- Created backup: `~/.emacs.d/init.el.straight-backup-20260125-032933`

### 2026-01-25: Removed Unused AI Packages
- Removed `aidermacs` and `gptel` packages (both supplanted by `agent-shell`)
- Reclaimed `C-c A` keybinding for `agent-shell` (unified ACP interface for Claude Code, Gemini CLI, Codex)
- Simplified configuration by removing 172 lines of unused AI tooling

### 2026-01-25: Removed Legacy Packages and Added Modern Tooling
- Removed `auto-package-update` (doesn't work with straight.el)
- Replaced `flycheck` with `flymake` (built-in, better Eglot integration)
- Replaced `desktop-save-mode` with `activities.el` (unified workspace/session management)
- Added `wgrep` for editing search results in-place
- Configured `helpful` with proper keybindings for enhanced help
- Rebound `C-h` to restore help prefix, moved `kill-whole-line` to `C-c k`
- Moved documentation from `var/doc/emacs.md` to `notes/emacs.md` (flat structure per CLAUDE.md)

**New workflows enabled**:
- **Activities**: Define named workspaces (like i3), suspend/resume with state preservation, auto-saves when idle
  - Arrange windows → `C-x C-a C-d` to define → `C-x C-a C-s` to suspend → `C-x C-a C-a` to resume later
  - Each activity saves two states: default (clean entry point) and last (where you left off)
  - Special buffers (compilation, shells, terminals) restore via bookmark system
- **Wgrep refactoring**: Run `consult-ripgrep`, press `C-c C-p` to edit results, `C-c C-c` to apply changes to all files
- **Enhanced help**: Press `C-h f` on any function to see source code, references, and better formatting
- **Flymake diagnostics**: Navigate errors with `C-c ! n/p`, list all with `C-c ! l`

### 2026-01-24

### Configuration Consolidation
- Merged `init-settings.el` into `init.el`
- Single source of truth for configuration
- Removed all Helm remnants
- Fixed `consult-apropos` → `apropos`

### Modern Completion Stack
- Replaced `company-mode` with `corfu`
- Added `cape` for source stacking
- Added `eglot` for LSP (replaces commented lsp-mode)
- Replaced `hippie-expand` workflow with modern popup completions

### Consult Preview Improvements
- Added automatic preview for ripgrep/grep results (0.2s debounce)
- Added instant preview for yank-ring (`M-y`) - just like Helm!
- Added `C-c R` for ripgrep from current directory
- `C-c r` still searches from project root
- Preview shows match context as you navigate

### Hermetic Tooling
- Added `uv` for Python tools
- Added `mise` for Node.js tools
- Replaced `nvm` and `pyenv`
- Language servers installed hermetically

## Troubleshooting

### Completions not working?
```elisp
;; Check if corfu is active
M-x describe-mode  ;; Should show corfu-mode

;; Manually trigger
C-M-i  ;; or M-TAB
```

### LSP not connecting?
```bash
# Verify language server is installed
which pylsp clangd typescript-language-server bash-language-server

# Check Emacs can find it
M-x shell
echo $PATH
```

### Vertico not showing?
```elisp
M-x vertico-mode  ;; Should be enabled
M-: (bound-and-true-p vertico-mode) RET  ;; Should return t
```

## References

- **Vertico**: https://github.com/minad/vertico
- **Corfu**: https://github.com/minad/corfu
- **Consult**: https://github.com/minad/consult
- **Eglot**: Built into Emacs 29+, `C-h i m eglot`
- **Configuration**: `~/etc/dotfiles/dot-emacs.d/init.el`
- **Build instructions**: `~/var/build/emacs.md`
