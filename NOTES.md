## TODO

- **i3 window rules via wrapper script**: Investigate using wrapper scripts with i3-msg for project-specific window rules instead of config files. Would allow launching apps with dynamic configuration (float, sticky, position, size) without polluting dotfiles. Example: `launch-discord-overlay` script that spawns process and configures via `i3-msg "[pid=$PID] floating enable"`. More scriptable and portable than config includes.

## 2026-01-24 - Emacs Configuration Consolidation

### Decision: Merge init-settings.el into init.el

**Context**: Emacs configuration was split between `init.el` (package configurations) and `init-settings.el` (global settings, functions, and keybindings). The split was a historical artifact that created discoverability issues.

**The Problem:**
- **Awkward setup**: Required knowing about `init-settings.el` existence
- **Helm remnants**: Still had `("C-x C-f" . helm-find-files)` binding in init-settings.el
- **Missing functions**: `copy-line` and `kill-line-backward` were bound but not defined
- **Unclear organization**: Not obvious which settings belonged where

**Implementation:**
Merged all init-settings.el content into the `(use-package emacs ...)` stanza in init.el:
- Global settings (backups, custom-file, compilation, etc.) → `:custom` section
- Mode activation (subword-mode, delete-selection-mode, etc.) → `:config` section  
- Helper functions (smart-open-line, copy-file-name-to-clipboard, etc.) → `:config` section
- Global keybindings → `:bind` section
- Hooks (sh-mode whitespace cleanup) → `:hook` section

**Why Keybindings Live in use-package emacs:**
All keybindings are in `(use-package emacs :bind ...)` because:
1. **They're global** - Should work everywhere, not mode-specific
2. **They map to global-map** - `:bind` in `use-package emacs` puts them in the global keymap
3. **Custom functions live here** - Functions like `copy-line`, `smart-open-line` are defined in this stanza
4. **Built-in commands** - Core Emacs commands like `compile`, `occur`, `recompile` work as global bindings

Could theoretically split them into individual package stanzas (e.g., move `compile`/`recompile` to `use-package compile`), but that's overkill for simple keybindings and makes them harder to find. Centralized approach is cleaner.

**Cleanup Actions:**
- ✅ Added missing `copy-line` and `kill-line-backward` function definitions
- ✅ Removed duplicate `C-c C-h` binding (kept only `C-c h`)
- ✅ Removed Helm reference: `("C-x C-f" . helm-find-files)`
- ✅ Removed unused `org-store-link` binding
- ✅ Fixed `consult-apropos` → `apropos` (consult doesn't provide apropos wrapper; Vertico already enhances built-in)
- ✅ Removed obsolete comment "Additional bindings from init-settings.el"
- ✅ Commented out and later removed `(load "~/.emacs.d/init-settings.el")`

**Benefits:**
- ✅ **Single source of truth**: All configuration in init.el
- ✅ **Better discoverability**: No hidden settings files
- ✅ **No Helm remnants**: Clean break from old completion framework
- ✅ **Logical organization**: Settings grouped by package/purpose

**Files:**
- `~/etc/dotfiles/dot-emacs.d/init.el` - consolidated configuration
- `~/etc/dotfiles/dot-emacs.d/init-settings.el` - no longer loaded (backup exists)
- `~/etc/dotfiles/dot-emacs.d/init-settings.el.backup` - archived original
- `~/etc/dotfiles/dot-emacs.d/init.el.backup` - archived pre-merge version

**Key Insight:** Modern `use-package` with `:demand t` and proper load ordering eliminates the need for separate "front-loaded" settings files. Everything can live in appropriate package stanzas.


## 2026-01-24 - Modern Auto-Completion Stack Migration

### Decision: Replace hippie-expand/company with Corfu + Cape + Eglot

**Context**: Previous completion setup relied on `hippie-expand` (M-/) for basic expansion and `company-mode` (disabled) for LSP. The commented-out `lsp-mode` indicated an abandoned attempt at LSP integration. This setup was outdated and didn't provide modern IDE-like completions.

**The Problem with hippie-expand:**
- ❌ **No UI** - Just cycles through options sequentially
- ❌ **Slow** - Checks each source one by one until it finds matches
- ❌ **No LSP integration** - Can't leverage language servers
- ❌ **No spell checking** - No integrated corrections
- ❌ **Sequential checking** - Tries backends in order, doesn't merge sources

**The Problem with company + lsp-mode:**
- ❌ **lsp-mode was commented out** - Never actually working
- ❌ **company-mode orphaned** - Hooked to lsp-mode but lsp-mode disabled
- ❌ **Heavy** - company has significant overhead, modal overlays
- ❌ **Older capf integration** - Wraps instead of native integration

**Modern Stack: Corfu + Cape + Eglot**

**Why This Stack:**
1. **Aligns with Vertico/Consult philosophy** - Minimal, fast, native Emacs integration
2. **Eglot built into Emacs 29+** - Zero external dependencies for LSP
3. **Capf-native** - Uses Emacs' `completion-at-point-functions` natively
4. **Community standard 2025-2026** - Current recommendation from Emacs community

**Architecture:**

```
User types → completion-at-point-functions (capf)
                ↓
            Cape stacks sources:
                1. eglot (LSP completions)
                2. cape-file (file paths)
                3. cape-dabbrev (buffer words)
                4. cape-ispell (spell check)
                ↓
            Corfu displays popup UI
                ↓
            User selects with TAB/RET
```

**Implementation Details:**

**Corfu Configuration:**
- `corfu-auto t` - Auto-trigger completions
- `corfu-auto-delay 0.1` - 100ms delay before showing
- `corfu-auto-prefix 2` - Trigger after 2 characters
- `corfu-cycle t` - Wrap around at list ends
- TAB/S-TAB for navigation (familiar muscle memory)

**Cape Sources (order matters):**
1. **cape-file** - File path completions (first for immediate paths)
2. **cape-dabbrev** - Dynamic abbreviations from buffers
3. **cape-ispell** - Spell checking completions
4. **eglot capf** - Added automatically by eglot when active

**Eglot Configuration:**
- Auto-enabled via hooks for: C++, C, Python, TypeScript, JavaScript, Shell
- `eglot-autoshutdown t` - Clean up when buffers close
- `eglot-sync-connect nil` - Async connection for faster startup
- Custom clangd args: `--background-index`, `--clang-tidy`, `--header-insertion=iwyu`

**Why Eglot Over lsp-mode:**
1. **Built-in Emacs 29+** - No external package needed
2. **Minimal config** - Works out of box with sensible defaults
3. **Native integration** - Uses flymake, project.el, xref (all built-in)
4. **Faster startup** - Lighter architecture
5. **Stability** - Part of Emacs core, rigorously tested
6. **Matches philosophy** - We chose Vertico over Helm, eglot over lsp-mode follows same logic

**Language Server Support:**
- **C/C++**: clangd (with clang-tidy, IWYU)
- **Python**: python-lsp-server or pyright (auto-detected)
- **TypeScript/JavaScript**: typescript-language-server
- **Shell**: bash-language-server

**Benefits:**

**vs hippie-expand:**
- ✅ Popup UI with visible candidates
- ✅ Multiple sources merged intelligently
- ✅ LSP integration for smart completions
- ✅ Spell checking integrated
- ✅ Much faster for large completion sets

**vs company + lsp-mode:**
- ✅ 10x lighter configuration
- ✅ Faster startup and completion
- ✅ Native Emacs integration (no modal overlays)
- ✅ Works seamlessly with Vertico/Consult
- ✅ eglot built-in (no external dependencies)

**Compatibility:**
- ✅ `M-/` still works (dabbrev-expand) for quick single expansions
- ✅ `C-M-i` / `M-TAB` (completion-at-point) now shows corfu popup
- ✅ All Vertico/Consult commands unaffected
- ✅ Can disable corfu-auto and use manual `C-M-i` if preferred

**Migration Notes:**
- Removed `(use-package company ...)` entirely
- Removed commented `(use-package lsp-mode ...)` and `(use-package lsp-ui ...)`
- Added corfu, cape, eglot in single location after compile package
- All settings self-contained and documented

**Key Insight:** Modern Emacs completion is built on **capf** (completion-at-point-functions). Corfu is a UI frontend, Cape stacks sources, and eglot provides LSP via capf. This architecture is cleaner than older approaches (company backends, helm sources) and aligns with Emacs' built-in systems.

**Testing:**
```elisp
;; Verify packages loaded
(fboundp 'corfu-mode)  ;; t
(fboundp 'eglot)       ;; t  
(fboundp 'cape-file)   ;; t

;; Check completion sources
completion-at-point-functions
;; => (cape-file cape-dabbrev cape-ispell ...)

;; In a code buffer with eglot:
;; => (eglot-completion-at-point cape-file cape-dabbrev ...)
```

**Future Improvements:**
- Could add `corfu-popupinfo` for inline documentation
- Could add `cape-keyword` for language keywords
- Could add `cape-dict` for dictionary completions
- Could tune `cape-dabbrev-check-other-buffers` for performance

**References:**
- Modern Emacs completion guide: https://blog.tjll.net/a-beginners-guide-to-extending-emacs
- Corfu wiki: https://github.com/minad/corfu
- Eglot manual: Built into Emacs, see `C-h i m eglot`
- 2025 community consensus: corfu > company, eglot > lsp-mode for most users


## 2026-01-24 - Hermetic Language Server Installation

### Decision: Replace nvm/pyenv with uv/mise for hermetic language server installation

**Context**: Emacs eglot needs language servers installed. Traditional approaches (pip, npm global installs) contaminate system packages and require sudo. The existing nvm setup was slow and Node-only.

**The Problem with Traditional Installation:**
- ❌ **pip global install**: Breaks system Python, requires sudo, conflicts between tools
- ❌ **npm global install**: Contamination, permission issues, version conflicts
- ❌ **nvm**: Slow (shell script, 20-40x slower than Rust alternatives), Node-only, shell eval overhead
- ❌ **pyenv**: Unnecessary complexity for tool installation, just managing Python versions

**Modern Hermetic Stack: uv + mise**

**Why This Stack:**
1. **Zero system contamination** - All user-level, isolated installations
2. **No sudo required** (except for clangd system package)
3. **Significantly faster** - Both are Rust-based, 10-100x faster than predecessors
4. **Reproducible** - Easy to replicate setup across machines
5. **Clean uninstall** - Just delete `~/.local` and `~/.cargo` directories

**Architecture:**

```
Language Servers Needed:
├── C/C++: clangd (system package via apt)
├── Python: python-lsp-server
│   └── Installed via: uv tool install
│       └── Location: ~/.local/bin/pylsp
│       └── Isolated environment, no system Python contamination
├── TypeScript: typescript-language-server
│   └── Installed via: mise Node + npm
│       └── Location: ~/.local/share/mise/installs/node/X.X.X/bin/
│       └── Shimmed to: ~/.local/share/mise/shims/
└── Bash: bash-language-server
    └── Installed via: mise Node + npm
        └── Same location as TypeScript
```

**Tool Details:**

### uv (Python Package Manager)
- **Replaces**: pip, pipx, pyenv
- **Speed**: 10-100x faster than pip (Rust vs Python)
- **Installation**: `curl -LsSf https://astral.sh/uv/install.sh | sh`
- **Location**: `~/.cargo/bin/uv`
- **Key Feature**: `uv tool install` creates isolated environments per tool
- **Benefits**:
  - No virtualenv needed for CLI tools
  - Each tool has own dependencies, no conflicts
  - Automatic dependency resolution
  - Lock file support for reproducibility

### mise (Polyglot Version Manager)
- **Replaces**: nvm, fnm, volta, pyenv, rbenv, asdf
- **Speed**: Fastest among all alternatives (Rust-based)
- **Installation**: `curl https://mise.run | sh`
- **Location**: `~/.local/bin/mise`
- **Key Features**:
  - **Shims**: Symlinks in `~/.local/share/mise/shims/` point to actual binaries
  - **Auto-activation**: Detects `.tool-versions`, `package.json`, `.node-version`
  - **Multi-language**: Can manage Python, Ruby, Go, etc. (not just Node)
  - **No shell overhead**: Unlike nvm's `eval "$(nvm init)"` on every shell
- **How it works**:
  ```
  1. mise use --global node@lts
     → Downloads Node to ~/.local/share/mise/installs/node/X.X.X/
  
  2. mise x -- npm install -g typescript-language-server
     → Uses mise's Node, installs to ~/.local/share/mise/installs/node/X.X.X/bin/
  
  3. Creates shim: ~/.local/share/mise/shims/typescript-language-server
     → Symlink points to ~/.local/bin/mise
     → mise intercepts and runs correct version
  
  4. PATH includes ~/.local/share/mise/shims/
     → Commands "just work" from any directory
  ```

**Shell Configuration (.zshenv):**

```bash
# Old (removed):
export PYENV_ROOT="$HOME/.pyenv"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
command -v pyenv >/dev/null 2>&1 && eval "$(pyenv init -)"

# New:
export MISE_DATA_DIR="$HOME/.local/share/mise"
command -v mise >/dev/null 2>&1 && eval "$(mise activate zsh)"
```

**Integration with Emacs Eglot:**

Eglot searches PATH for language servers. With mise shims in PATH:
```elisp
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (sh-mode . eglot-ensure))
  ...)
```

When you open a TypeScript file:
1. `eglot-ensure` runs
2. Eglot looks for `typescript-language-server` in PATH
3. Finds `~/.local/share/mise/shims/typescript-language-server`
4. Shim executes `~/.local/bin/mise` which finds real binary
5. mise runs correct version for current project

**Benefits:**

**vs pip:**
- ✅ No system contamination
- ✅ No sudo needed
- ✅ 10-100x faster
- ✅ Isolated per-tool dependencies

**vs nvm:**
- ✅ 3-40x faster (Rust vs shell script)
- ✅ No shell eval overhead
- ✅ Multi-language support (not just Node)
- ✅ True per-project isolation

**Future: Per-Project Versions**

mise supports project-specific versions:

```bash
cd ~/project-a
mise use node@18
echo "node 18" > .tool-versions

cd ~/project-b
mise use node@20
# Auto-switches when you cd between projects!
```

**Verification:**

```bash
# Check installations
which uv                          # ~/.cargo/bin/uv
which mise                        # ~/.local/bin/mise
which pylsp                       # ~/.local/bin/pylsp
which typescript-language-server  # ~/.local/share/mise/shims/...

# Check versions
uv --version
mise --version
pylsp --version
typescript-language-server --version

# See mise status
mise ls        # Lists installed runtimes
mise which node  # Shows which node binary is active
```

**Documentation:**
- Usage guide: `~/var/doc/emacs.md`
- Build instructions: `~/var/build/emacs.md`

**Key Insight:** Hermetic installation via modern Rust-based tools (uv, mise) eliminates the decade-old problems of system contamination, permission issues, and version conflicts. The architecture is cleaner: each tool gets its own isolated environment, shims provide transparent access, and everything is user-level (no sudo).

**Migration Notes:**
- Old `~/.nvm/` no longer loaded (can be deleted after verification)
- Old `~/.pyenv/` no longer loaded (can be deleted after verification)
- `.zshenv` updated to use mise instead of nvm/pyenv
- `configure-system.sh` replaces nvm section with full language server stack

**References:**
- uv: https://docs.astral.sh/uv/
- mise: https://mise.jdx.dev/



## 2026-01-16 - Go Installation and Bluetuith Configuration

### Decision: Add Go toolchain installation and bluetuith Bluetooth manager to configure-system.sh

**Context**: User wanted to add bluetuith (terminal-based Bluetooth manager) to system configuration. This required first installing the Go toolchain since bluetuith is most reliably installed via `go install`.

**Implementation:**

**1. Go Installation:**
- Fetches latest Go version dynamically using canonical API: `curl -s "https://go.dev/dl/?mode=json" | jq -r '.[0].version'`
- Downloads and installs to `/usr/local/go` (standard location per go.dev documentation)
- Adds both `/usr/local/go/bin` and `$HOME/go/bin` to PATH for current session
- Skips installation if Go already present

**2. Bluetuith Installation:**
- Installed via `go install github.com/darkhz/bluetuith@latest`
- Binary placed in `~/go/bin` by Go toolchain
- Only installs if not already present in PATH

**Why Go Installation Method:**
- Cross-distribution compatibility (not all distros package bluetuith)
- Always gets latest version directly from source
- No PPA or manual binary management required
- Follows upstream project's recommended installation method

**Benefits:**
- ✅ **Always current**: Fetches latest Go version on each run
- ✅ **Portable**: Works across different Linux distributions
- ✅ **Reliable**: Uses official Go installation method from go.dev
- ✅ **No version drift**: No hardcoded versions that become stale

**Key Insight:** Using the JSON API endpoint (`https://go.dev/dl/?mode=json`) with `jq` provides a reliable, canonical way to fetch the latest Go version. This is superior to hardcoding versions or scraping HTML.

## 2026-01-13 - Documentation Directory Consolidation

### Decision: Consolidate ~/doc/ into ~/var/doc/

**Context**: Had redundant documentation directories (`~/doc/` and `~/var/doc/`) serving unclear purposes.

**The Problem:**
- **~/doc/**: Abandoned location with random notes and Anki flashcard data
- **~/var/doc/**: Active working documents (aider history, project files, logs)
- **~/var/build/**: Build instructions paired with automation scripts
- Unclear where new documentation should go

**Why Consolidate to ~/var/doc/:**
- **FHS alignment**: `/var` is for variable/changing data - exactly what working documents are
- **Existing structure**: Already had `~/var/build/` and `~/var/web/` (Downloads symlink)
- **Clear semantics**: `~/var/` signals "stuff that changes" vs static configs/binaries
- **Consistency**: Keeps all variable data under one parent directory

**Files Moved:**
- `~/doc/notes/voice-dictation.md` → `~/var/doc/notes/`
- `~/doc/txt/anki/*` (Fry word lists, math flashcards) → `~/var/doc/txt/anki/`
- `~/doc/txt/chronos-first-text-file.txt` → `~/var/doc/txt/`

**Final Structure:**
```
~/var/
├── build/     # Build instructions + automation (established pattern)
├── doc/       # Working documents, notes, project files (consolidated)
└── web/       # Downloads symlink (established)
```

**Benefits:**
- ✅ **Clarity**: Single location for all variable documentation
- ✅ **FHS-compliant**: Follows Unix filesystem hierarchy principles
- ✅ **Consistent**: All `/var`-style data under `~/var/`
- ✅ **No confusion**: Obvious where to put new documents

**Key Insight:** The `~/prg/` exception (non-FHS) works because it's short and memorable. Generic directories like `~/doc/` don't need to be at root level when `~/var/doc/` provides better organization and FHS alignment.

## 2026-01-13 - Emacs Completion: Migrated from Helm to Vertico Stack

### Decision: Replace Helm with Vertico + Consult + Orderless + Marginalia + Embark

**Context**: Helm had become a performance bottleneck and felt too opinionated/heavyweight. User wanted a lighter, faster, more modular completion framework.

**Problems with Helm:**
- **Slow buffer switching**: `helm-buffers-list` could take 1+ seconds with many buffers open
- **Clunky file finding**: `helm-find-files` was sluggish and frustrating (see init.el comments: "I can't fucking stand helm-find-files")
- **Monolithic**: Heavy, opinionated framework that takes over Emacs
- **TRAMP issues**: Known performance problems with remote files
- **Mode pollution**: Helm mode affects too many things globally

**Why Vertico Stack:**
- **Modular**: Each component does one thing well, can be swapped independently
- **Fast**: Instant response even with thousands of candidates
- **Native integration**: Works with standard Emacs `completing-read` APIs
- **Lightweight**: Minimal footprint compared to Helm
- **Not opinionated**: Compose behavior from small packages rather than fighting a framework

**Packages Chosen:**

1. **Vertico** - Minimal vertical completion UI
   - Just displays candidates efficiently
   - Cycles through results
   - Shows 20 items by default

2. **Orderless** - Flexible matching engine
   - Space-separated terms match in any order
   - Type `fbuf` to match `find-buffer`
   - Works across all completion contexts

3. **Marginalia** - Rich annotations
   - Shows file info, keybindings, documentation in minibuffer
   - Adds context without cluttering

4. **Consult** - Enhanced commands
   - Modern replacements for common operations
   - Live preview for searches
   - Intelligent grouping (buffers, files, bookmarks)

5. **Embark** - Context actions on candidates
   - Like right-click menus for completion
   - Perform actions without selecting

**Key Binding Migration:**

| Function | Old (Helm) | New (Consult/Native) |
|----------|------------|----------------------|
| Command palette | `M-x` → `helm-M-x` | `M-x` (native + vertico) |
| Switch buffer | `C-x b` → `helm-buffers-list` | `C-x b` → `consult-buffer` |
| Find file | `C-x C-f` → `helm-find-files` | `C-x C-f` (native + orderless) |
| Kill ring | `M-y` → `helm-show-kill-ring` | `M-y` → `consult-yank-pop` |
| Buffer search | `C-c h o` → `helm-occur` | `C-c h o` → `consult-line` |
| Help apropos | `<f1> a` → `helm-apropos` | `<f1> a` → `consult-apropos` |
| Grep files | `C-c r` → `rgrep` | `C-c r` → `consult-ripgrep` |
| Find by name | `C-c f` → `find-grep-dired` | `C-c f` → `consult-find` |

**Bonus Commands Added:**
- `M-g i` → `consult-imenu` (jump to functions/classes)
- `M-g M-g` → `consult-goto-line` (with live preview)
- `C-x C-r` → `consult-recent-file` (quick recent files)
- `C-.` → `embark-act` (context actions)
- `M-.` → `embark-dwim` (smart default action)

**Technical Details:**
- Configured orderless with `basic` fallback for file completion
- Set `vertico-cycle t` for wraparound navigation
- Integrated `consult-ripgrep` to use ripgrep when available
- Preserved `find-name-dired` at `C-c n` (still useful for some workflows)

**Benefits Achieved:**
- ✅ **Speed**: Buffer/file operations now instant
- ✅ **Clarity**: Minimal UI, no visual noise
- ✅ **Flexibility**: Can replace individual components without breaking workflow
- ✅ **Better UX**: Live preview, intelligent grouping, contextual actions
- ✅ **Lower friction**: No fighting with opinionated defaults

**Comparison to Other Stacks:**
- **Ivy/Counsel/Swiper**: Lighter than Helm but still somewhat monolithic; Vertico is more modular
- **Ido**: Simple but limited; doesn't scale to modern workflows
- **Icomplete/Fido**: Built-in but minimal features compared to Vertico stack

**Key Insight:** The Emacs ecosystem has moved toward composition of small, focused packages rather than monolithic frameworks. Vertico stack represents current best practices: minimal core (vertico) + composable enhancements (consult, orderless, marginalia, embark).

**Integration:** Works seamlessly with existing packages (avy, magit, projectile, etc.). No conflicts with other configured tools.

## 2025-11-09 - MCP Git/Shell Server Root Directory Detection

### Decision: Auto-detect git repository root and cd to it before launching aider

**Context**: MCP git and shell servers were receiving incorrect repository paths when aider was launched from subdirectories. When the model passed `repo_path: "."` to git MCP tools, it resolved to aider's `$PWD` (the subdirectory) instead of the git repository root.

**The Problem:**

**Initial symptom**: Git status returned blank/incorrect results
- Launched aider from `/home/danenberg/bin` (a subdirectory)
- MCP git server configured with `--repository /home/danenberg` (correct)
- But when model called `git_status(repo_path: ".")`, the `.` resolved to `/home/danenberg/bin`
- Result: Git commands failed or returned incorrect output

**Root cause**: Even with correct `--repository` flag, MCP tools resolve relative paths like `.` against aider's current working directory (`$PWD`), not the configured repository.

**Solutions Considered:**

**❌ Set `AIDER_MCP_ROOT` from Emacs**
- Would require modifying aidermacs Emacs package
- Couples the solution to Emacs (doesn't work from command line)
- More complex to maintain

**✅ Auto-detect in bash wrapper + cd to root**
- Self-contained in `bin/aider-claude` wrapper script
- Works from Emacs AND command line
- Simple to understand and maintain

**Implementation:**

**1. Auto-detect git root or fallback to PWD:**
```bash
MCP_ROOT_DIR=$(git rev-parse --show-toplevel 2>/dev/null || echo "$PWD")
```

**2. Template MCP config at runtime:**
- Created `etc/mcp-template.json` with `$MCP_ROOT_DIR` placeholders
- Use `envsubst` to replace variables on each aider invocation
- Generate temporary config file that gets cleaned up on exit

**3. Critical fix - cd to root before launching aider:**
```bash
cd "$MCP_ROOT_DIR"
```

This ensures when the model passes `repo_path: "."`, it resolves correctly to the git root.

**Files Modified:**

**bin/aider-mcp** (new base script):
- Auto-detect git root using `git rev-parse --show-toplevel`
- Fall back to `$PWD` if not in a git repository
- Template `etc/mcp-template.json` using `envsubst`
- **cd to `$MCP_ROOT_DIR` before launching aider**
- Accepts profile name as first argument (claude, gemini, or gpt)
- Contains all MCP logic and comprehensive documentation

**bin/aider-claude, bin/aider-gemini, bin/aider-gpt** (refactored to minimal wrappers):
- Each is now just 2 lines: `exec "$(dirname "$0")/aider-mcp" <profile> "$@"`
- All three profiles now benefit from the same MCP root detection and templating
- Maintains backward compatibility (same command-line interface)

**etc/mcp-template.json** (new file):
```json
{
  "mcpServers": {
    "git": {
      "command": "uvx",
      "args": ["mcp-server-git", "--repository", "$MCP_ROOT_DIR"]
    },
    "shell": {
      "command": "npx",
      "args": ["-y", "@mkusaka/mcp-shell-server", "--working-dir", "$MCP_ROOT_DIR"]
    },
    // ... other MCP servers
  }
}
```

**etc/dotfiles/dot-zshenv:**
- Added `export MCP_TEMPLATE_FILE="$HOME/etc/mcp-template.json"`
- Allows overriding template location (e.g., for corporate machines without git/shell MCPs)

**Variable Naming:**
- Initially used `ROOT_DIR` (too generic)
- Renamed to `MCP_ROOT_DIR` to avoid potential conflicts with other scripts

**Test Results:**

**From subdirectory** (`/home/danenberg/bin`):
```
MCP_ROOT_DIR: /home/danenberg
```
✅ Correctly detected git root, MCP servers operate on full repository

**From non-git directory** (`/`):
```
MCP_ROOT_DIR: /
```
✅ Falls back to PWD, shell MCP still works (git MCP fails gracefully as expected)

**Benefits:**
- ✅ **Git MCP works correctly**: Sees full repository history and structure
- ✅ **Shell MCP works correctly**: Executes from repository root
- ✅ **Model UX**: Can use simple `repo_path: "."` instead of absolute paths
- ✅ **Emacs-agnostic**: Works from command line, Emacs, or any launcher
- ✅ **Graceful fallback**: Works in non-git directories (shell MCP only)
- ✅ **Flexible**: Can override template via `$MCP_TEMPLATE_FILE` env var
- ✅ **Maintainable**: Single source of truth in `bin/aider-mcp` for all profiles
- ✅ **Consistent**: All three profiles (claude, gemini, gpt) get identical MCP behavior

**Key Insight:** The `cd "$MCP_ROOT_DIR"` before launching aider is critical. Without it, even with correct `--repository` and `--working-dir` flags, relative paths in tool arguments resolve against the wrong directory. By changing to the root before launching, we ensure aider's `$PWD` matches the MCP server expectations.

**Integration**: Works seamlessly with existing encrypted MCP configuration (`envmcp` + `git-crypt` from 2025-09-14) and mcp-proxy systemd service (from 2025-10-28).

## 2025-10-29 - Aider Configuration: YOLO Mode and Tool Usage Guidelines

### Decision: Enable yes-always mode and add guidance for efficient tool usage

**Context**: User wanted to streamline aider workflow by eliminating confirmation prompts and preventing redundant tool usage.

**Changes Made:**

**1. YOLO Mode Activation:**
- Enabled `yes-always: true` in `~/etc/dotfiles/dot-aider.conf.yml`
- Eliminates all confirmation prompts - aider will proceed immediately with changes
- "Living dangerously" mode for faster iteration
- Trade-off: Speed vs. safety (relying on git for rollback if needed)

**2. Updated AGENTS.md Guidelines:**

**Problem:** LLMs were using shell MCP unnecessarily:
- Running `cat` on files already in aider's context (wasting tokens/money)
- Using `cat >` to rewrite entire files instead of surgical SEARCH/REPLACE blocks
- Shell commands for operations aider handles natively

**Solution:** Added "Working with Aider" section to AGENTS.md:
- **Files in context**: Edit directly with SEARCH/REPLACE blocks, don't shell cat them
- **Surgical edits**: Use aider's search/replace for modifications, not full file rewrites
- **Shell MCP fallback**: Only for things aider can't do (tests, git, packages, system state)
- Key principle: "Shell MCP is powerful but expensive in context. Use aider's native features when available."

**3. Streamlined Git Guidelines:**
- Removed verbose examples and explanations from commit message section
- Kept only essential format and key guidelines
- Saves context window for more important content

**The Irony:**
Immediately after writing guidelines saying "don't use cat > to rewrite files already in context," I violated my own rule by using `cat > ~/AGENTS.md` instead of proper SEARCH/REPLACE blocks. Classic "do as I say, not as I do" moment. 😅

**Benefits:**
- ✅ **Faster workflow**: No confirmation prompts with YOLO mode
- ✅ **Cost savings**: Avoiding redundant shell commands for file operations
- ✅ **Better scaling**: Surgical edits work for large files, full rewrites don't
- ✅ **Clearer guidelines**: AGENTS.md now explicitly guides tool selection
- ✅ **Leaner docs**: Trimmed git section saves context tokens

**Key Insight:** The shell MCP server is powerful but should be used judiciously. When files are already in aider's context, using shell commands to read/write them is wasteful. Aider's SEARCH/REPLACE blocks are designed exactly for this use case and scale much better.

## 2025-10-28 - MCP Proxy Environment Variable Configuration Fix

### Decision: Remove API key env fields from mcp-proxy servers.json, rely on systemd --pass-environment

**Context**: The mcp-proxy service was repeatedly restarting in aider, and MCP servers were receiving HTTP 401 errors due to missing API keys.

**Problems Identified:**

**1. Binary Shadowing Issue:**
- `bin/mcp-proxy` was shadowing the real mcp-proxy binary
- Client instances spawned new servers instead of connecting to the running daemon on port 3100
- Solution: Deleted `bin/mcp-proxy` to allow proper connection to daemon

**2. Environment Variable Passing:**
- MCP servers (openmemory, firecrawl, linkup, context7, perplexity) were getting HTTP 401 errors
- API keys from `~/.env.mcp` weren't being passed through to the servers
- The `env` fields in `~/.config/mcp-proxy/servers.json` weren't receiving the environment variables

**Root Cause:**
The `env` fields in `servers.json` were trying to reference environment variables, but mcp-proxy wasn't substituting them. The systemd service's `--pass-environment` flag is designed to handle this automatically.

**Solution:**
- Removed all `env` fields containing API key references from `servers.json` (CONTEXT7_API_KEY, FIRECRAWL_API_KEY, LINKUP_API_KEY, OPENMEMORY_API_KEY, PERPLEXITY_API_KEY)
- Let the systemd service's `--pass-environment` flag pass through all variables from `~/.env.mcp`
- Preserved literal configuration values (e.g., `"CLIENT_NAME": "openmemory"` in openmemory server config)

**Key Insight:**
The `--pass-environment` flag in the systemd service (`etc/dotfiles/dot-config/systemd/user/mcp-proxy.service`) automatically makes environment variables available to all spawned MCP servers. Explicitly declaring them in the `env` section of `servers.json` was redundant and caused issues.

**Benefits:**
- ✅ **Stability**: mcp-proxy daemon runs without repeatedly restarting
- ✅ **Simplicity**: Cleaner configuration without redundant env declarations
- ✅ **Security**: API keys remain in encrypted `~/.env.mcp` file
- ✅ **Reliability**: All MCP servers now receive proper authentication

**Integration**: Works seamlessly with the envmcp wrapper and git-crypt encrypted environment variables established in previous sessions.

## 2025-01-12 - MCP Server Configuration Expansion

### Decision: Added filesystem and shell MCP servers to configuration

**Context**: Extended MCP server capabilities by adding two new essential servers for file operations and shell command execution.

**New MCP Servers Added:**

**1. Filesystem Server** (`@modelcontextprotocol/server-filesystem`):
- Provides file system access capabilities
- Default path: `/home/user` 
- Enables reading, writing, and managing files through MCP

**2. Shell Server** (`@mkusaka/mcp-shell-server`):
- Enables execution of shell commands via MCP
- Provides system-level command execution capabilities
- Useful for automation and system administration tasks

**Implementation Details:**
- Added both servers to `etc/mcp.json` configuration
- Maintained alphabetical order in `mcpServers` section
- Followed existing configuration patterns for consistency
- Both servers use standard npm package installation approach

**Benefits:**
- ✅ **File Operations**: Direct file system access through MCP protocol
- ✅ **Shell Commands**: Execute system commands programmatically  
- ✅ **Automation**: Enhanced capabilities for system automation tasks
- ✅ **Consistency**: Maintains existing configuration structure and patterns

**Integration**: Servers work alongside existing MCP configuration including encrypted environment variables via `envmcp` wrapper established in previous session.

## 2025-08-11 - Downloads Directory Reorganization  

### Decision: Move Downloads to ~/var/web with symlink compatibility

**Context**: The default ~/Downloads directory violates FHS principles and feels out of place in a well-organized home directory structure.

**Problem with ~/Downloads:**
- Capitalized and verbose (doesn't match bin/, etc/, var/ structure)
- Not FHS-compliant 
- Takes up prime real estate in home directory

**Alternatives considered:**
- `~/var/spool` - Technically correct (data awaiting processing) but semantically awkward
- `~/var/dl` - Obvious abbreviation but not descriptive  
- `~/var/in` - Clean "inbox" concept
- `~/var/recv` - For "received" files

**Why we chose ~/var/web:**
- Only 9 characters (vs 12 for ~/Downloads)
- Semantically accurate - files grabbed from the web
- Fits perfectly with existing FHS structure (bin/, etc/, var/, doc/, prg/)
- Clear indication of file source

**Implementation**: 
- Added task to `configure-system.sh` to create `~/var/web` and symlink `~/Downloads -> ~/var/web`
- Maintains compatibility with applications that hardcode ~/Downloads
- Clean organizational structure while preserving functionality

## 2025-08-07 - Environment Variables and Secrets Management

### Decision: Use git-crypt + set -a instead of pass show

**Context**: Considered using `pass show` directly in `.env-secrets` instead of storing encrypted values.

**Why we didn't use `pass show`:**
- GPG unlock timing: We unlock GPG lazily (first browser use), but `.zshenv` loads early
- Shell startup would hang waiting for GPG unlock
- Background processes/scripts can't prompt for GPG passphrase  
- Performance: Every new shell would call `pass` multiple times

**Why we chose set -a approach:**
- Cleaner than repetitive `export` statements
- Fast shell startup (no external calls)
- Works in all contexts (cron, scripts, etc.)
- git-crypt provides adequate security for this use case

**Implementation**: Used `set -a` / `set +a` in `.env-secrets` to automatically export all variable assignments without repetitive `export` keywords.

## 2025-09-14 - MCP Configuration Security and Environment Variable Substitution

### Decision: Use envmcp wrapper with git-crypt encrypted environment files

**Context**: Needed secure API key management for MCP server configurations without exposing credentials in version control.

**The Problem:**
- MCP configurations require API keys for services (OpenMemory, Firecrawl, Linkup, etc.)
- Hardcoding keys in JSON configs creates security risks
- Most MCP clients don't support `${env:VARIABLE_NAME}` syntax yet
- Need to share configurations while keeping credentials secure

**Solutions Investigated:**

**❌ Native Environment Variable Substitution**
- `${env:VARIABLE_NAME}` syntax doesn't work in Claude Desktop, Cursor
- Feature requested but not implemented in most MCP clients
- Works in VS Code's `launch.json`/`tasks.json` but not MCP configs
- GitHub issues: VS Code (#264448, #245237), Cursor (#79639)

**✅ envmcp Package**
- Lightweight wrapper (11.5 kB, 611 weekly downloads)  
- Loads variables from `.env.mcp` files
- Substitutes `$VARIABLE_NAME` in command arguments
- Works with existing MCP server environment variable expectations
- Syntax: `npx envmcp [options] command [args...]`

**⚖️ mcpipe Package**
- Feature-rich wrapper (24.3 kB, 33 weekly downloads)
- Includes all envmcp functionality PLUS debugging features
- Debug JSON-RPC messages, tool name prefixing, process monitoring  
- Better for development/debugging, overkill for production

**Final Implementation:**

**1. Encrypted Environment File** (`~/etc/dotfiles/dot-env.mcp`):
```bash
# MCP-specific environment variables (git-crypt encrypted)
ANTHROPIC_API_KEY="..."
FIRECRAWL_API_KEY="..."
LINKUP_API_KEY="..."
OPENMEMORY_API_KEY="..."
```

**2. Clean MCP Configuration** (`~/etc/mcp.json`):
```json
{
  "mcpServers": {
    "openmemory": {
      "command": "npx",
      "args": ["envmcp", "npx", "-y", "openmemory"],
      "env": {
        "OPENMEMORY_API_KEY": "$OPENMEMORY_API_KEY",
        "CLIENT_NAME": "openmemory"
      }
    }
  }
}
```

**Benefits Achieved:**
- ✅ **Security**: API keys encrypted by git-crypt, never exposed in plain text
- ✅ **Shareable**: MCP configuration can be safely committed to version control
- ✅ **Standard**: Uses familiar `.env` file approach developers expect
- ✅ **Compatibility**: Works with existing MCP servers expecting environment variables
- ✅ **Maintainable**: Clear separation of secrets and configuration

**Key Insights:**
- `envmcp` substitutes variables in the `env` section, not just command args
- Git-crypt pattern must be added BEFORE creating encrypted files
- GNU stow manages the dotfile → home directory symlink (`~/.env.mcp`)
- Solution works immediately while waiting for native MCP client support

**Recommendation:** Use `envmcp` for production (lightweight, focused), `mcpipe` for debugging complex MCP communication issues.

## 2025-01-11 - Web Search API Comparison for MCP Servers

### Decision: Comprehensive analysis of top web search APIs for AI applications

**Context**: Researched the best web search MCP servers and APIs for AI agent integration.

**Performance & Accuracy Rankings (SimpleQA Benchmark):**
1. **Linkup**: 91.0% (State-of-the-art)
2. **Exa**: 90.04% (Very close second) 
3. **Perplexity Sonar Pro**: 86%
4. **Perplexity Sonar**: 77%
5. **Tavily**: 73%

**Detailed Comparison:**

**🏆 Linkup**
- Highest accuracy on factual benchmarks
- API-first company (dedicated focus)
- Simple, predictable pricing ($5/1000 calls)
- Premium content partnerships
- Fixed-rate pricing model
- Deep search mode for complex multi-step tasks
- Fast response times (2-20 seconds depending on mode)
- **Best for:** Business intelligence, lead enrichment, factual research, cost-conscious developers

**🥈 Exa (formerly Metaphor)**
- Neural/semantic search (understands meaning, not just keywords)
- Very high accuracy (90.04%)
- Fast response times (~2-3 seconds)
- Good for finding semantically similar content
- Strong developer community adoption
- **Weaknesses:** Can struggle with multi-step complex tasks, more expensive for research-heavy workflows
- **Best for:** Semantic search, finding similar content, speed-critical applications

**🥉 Tavily**
- AI-optimized search specifically for agents
- Good balance of search + scraping capabilities
- Extract API for content scraping
- Modular approach (separate search/scrape endpoints)
- Easy LangChain integration
- **Weaknesses:** Lower accuracy compared to Linkup/Exa, more expensive than Linkup ($8/1000 calls)
- **Best for:** AI agents needing both search and scraping, LangChain workflows

**🔍 Perplexity Sonar**
- Real-time web access with generated answers and citations
- Good for conversational AI
- **Weaknesses:** Lower accuracy scores, complex variable pricing (65% more expensive than Linkup), performance "tuned down by design" to protect consumer product
- **Best for:** Conversational AI where you want pre-generated answers

**Cost Comparison (per 1000 calls):**
- Linkup: $5 (fixed rate)
- Exa: $5 (basic), higher for research mode
- Tavily: $8
- Perplexity Sonar: $9.44-$16.44 (variable)
- Perplexity Sonar Pro: $70.52-$78.52 (variable)

**Developer Feedback Summary:**
- Speed ranking: Exa > Tavily > Linkup
- Quality ranking: Linkup > Tavily > Exa
- Cost-effectiveness: Linkup consistently most affordable
- Reliability: Linkup praised for consistency

**Recommendation:** For most AI applications requiring web search, **Linkup appears to be the current leader** due to its combination of highest accuracy, lowest cost, and API-first focus.
