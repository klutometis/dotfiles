# Emacs LLM Integration Research

Comprehensive analysis of different approaches to integrating LLMs (specifically Claude/OpenCode) with Emacs.

## Overview

Multiple approaches exist for LLM integration in Emacs:
1. **Inline completions** (minuet)
2. **Chat/Q&A** (gptel)
3. **Tool-based integration** (colobas/opencode.el - gptel with OpenCode tools)
4. **Webserver control** (sczi/opencode.el - HTTP API)
5. **MCP-based control** (manzaltu/claude-code-ide.el - Model Context Protocol)
6. **Subset command support** (xenodium/agent-shell - OpenCode over ACP)

## Key Projects

### 1. mwolson's Setup (Reference Implementation)

**URL**: https://mwolson.org/guides/emacs-ai-setup/

**Stack**:
- **minuet** for inline completions
- **gptel** for chat/Q&A
- **OpenCode** (external) for agentic workflows
- **OpenCode Zen** as unified provider

**Key Insights**:
- Uses `auto-revert-mode` (enabled by Magit) to sync OpenCode changes
- Runs OpenCode in external terminal (Ghostty)
- Syncs minuet provider settings from gptel config (DRY)
- Minimal Emacs integration - relies on file system sync

**Minuet Configuration Highlights**:
```elisp
(setopt minuet-add-single-line-entry nil
        minuet-auto-suggestion-debounce-delay 0.3
        minuet-n-completions 1)  ; Cursor-style single suggestions

;; Block suggestions except at end of non-empty lines
(defun my-minuet-block-suggestions ()
  (not (and (not buffer-read-only)
            (not (bolp))
            (looking-at-p "\\s*$"))))
```

**Provider Sync Pattern**:
```elisp
(defun my-minuet-sync-options-from-gptel (m-backend g-backend &optional g-model)
  "Synchronize Minuet provider options from the current gptel backend.")
```

**Workflow**:
1. Inline completions via minuet (TAB for first line, C-c C-c for full)
2. Function completion via gptel-fn-complete
3. Complex tasks via OpenCode in external terminal
4. Emacs auto-refreshes files changed by OpenCode

**Model Choices**:
- Kimi K2 for minuet (inline completions)
- Claude Opus 4.5 for gptel prompts
- Uses OpenCode Zen exclusively

---

### 2. colobas/opencode.el (Tool Integration)

**URL**: https://github.com/colobas/opencode.el

**Status**: ⚠️ Unmaintained ("heavily vibe-coded", author seeking new maintainer)

**Approach**: Port OpenCode tools/prompts to gptel

**Architecture**:
```
┌─────────────────────────────────────────────────────────────┐
│                   opencode.el (Main Package)                │
│  ┌─────────────────┬─────────────────┬─────────────────┐   │
│  │   Setup &       │   Agent         │   Integration   │   │
│  │   Autoloads     │   Presets       │   Functions     │   │
│  └─────────────────┴─────────────────┴─────────────────┘   │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                Tool Layer                                   │
│  ┌─────────────────┬─────────────────┬─────────────────┐   │
│  │ opencode-tools  │ opencode-lsp    │ opencode-agents │   │
│  │                 │                 │                 │   │
│  │ • File Ops      │ • Diagnostics   │ • System        │   │
│  │ • Commands      │ • Symbols       │   Prompts       │   │
│  │ • Search        │ • Hover Info    │ • Presets       │   │
│  │ • Editing       │ • Auto-start    │ • Specialization│   │
│  │ • Tasks         │                 │                 │   │
│  └─────────────────┴─────────────────┴─────────────────┘   │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│              Description Layer                              │
│                opencode-descriptions                        │
└─────────────────────────────────────────────────────────────┘
```

**Key Features**:
- **Rich Tool Descriptions**: Guide LLM behavior with usage patterns
- **Deep Emacs Integration**: Native buffer ops, LSP integration
- **LSP-Powered**: Real-time diagnostics, symbol search
- **Task Management**: Structured todo system
- **Permission System**: Configurable command security
- **Agent Specialization**: Different presets for coding/research

**Tool Categories**:

| Category | Tools | LSP Integration |
|----------|-------|-----------------|
| File Ops | Read, edit, create_file, list_directory, apply_diff_fenced | ✅ |
| Search | Glob (find/ripgrep), grep, lsp_symbols | LSP for symbols |
| Commands | Bash (with permissions) | - |
| Tasks | todowrite, todoread | - |
| Emacs | read_buffer, edit_buffer, append_to_buffer, list_buffers, read_documentation | ✅ |
| External | search_web (SearXNG) | - |

**Agent Presets**:
- `opencode` - Full experience (all tools)
- `opencode-coding` - Development focused
- `opencode-general` - Research & analysis
- `opencode-minimal` - Essential only

**Permission System**:
```elisp
(setq opencode-bash-permissions
      '(("ls*" . "allow")
        ("git*" . "allow")
        ("rm*" . "ask")
        ("sudo*" . "deny")
        ("*" . "ask")))

(setq opencode-edit-permissions "ask")  ; "allow", "deny", or "ask"
```

**LSP Integration**:
- Automatic LSP startup when files accessed
- Real-time diagnostics in tool output
- Workspace-wide symbol search
- Enhanced context for LLM

**Setup**:
```elisp
(use-package opencode
  :vc (:url "https://github.com/colobas/opencode.el"
            :rev :newest
            :branch "main")
  :after gptel
  :config
  (opencode-setup-coding))  ; or opencode-setup, opencode-setup-minimal
```

**Strengths**:
- Comprehensive tool suite
- LSP integration for code intelligence
- Security-conscious permission system
- Works entirely within Emacs (no external OpenCode process)

**Weaknesses**:
- Unmaintained/experimental
- No actual OpenCode CLI integration
- Reimplements OpenCode functionality in Elisp

---

### 3. sczi/opencode.el (HTTP Webserver Control)

**URL**: https://codeberg.org/sczi/opencode.el

**Approach**: Control OpenCode via its HTTP webserver API

**Key Discovery**: OpenCode has a built-in webserver!

**OpenCode Server**:
```bash
# Start headless HTTP server
opencode serve  # Default: http://127.0.0.1:4096

# Flags
--port 4096               # Default port
--hostname 127.0.0.1      # Default host
--mdns                    # Enable discovery
--cors                    # Allow browser origins
```

**TUI Control Endpoints** (for IDE integration):

| Method | Path | Description | Response |
|--------|------|-------------|----------|
| POST | `/tui/show-toast` | Show toast notification | boolean |
| GET | `/tui/control/next` | Wait for next control request | Control object |
| POST | `/tui/control/response` | Respond to control request | boolean |

**Other Endpoints**:
- `PUT /auth/:id` - Set credentials
- `GET /event` - SSE stream (starts with `server.connected`)
- Full OpenAPI 3.1 spec at `/doc`

**Architecture**:
```
┌───────────────────────────────────────────────────────────┐
│                     Emacs (sczi/opencode.el)              │
│                                                           │
│  ┌─────────────────┬──────────────────┬────────────────┐ │
│  │ opencode.el     │ opencode-api.el  │ opencode-      │ │
│  │ (main)          │ (HTTP client)    │ sessions.el    │ │
│  │                 │                  │ (persistence)  │ │
│  └─────────────────┴──────────────────┴────────────────┘ │
└───────────────────────────┬───────────────────────────────┘
                            │ HTTP API
                            ▼
┌───────────────────────────────────────────────────────────┐
│              OpenCode Server (opencode serve)             │
│                                                           │
│  ┌──────────────────┬────────────────┬────────────────┐  │
│  │ /tui/control/*   │ /event (SSE)   │ /auth/:id      │  │
│  │ (TUI control)    │ (events)       │ (credentials)  │  │
│  └──────────────────┴────────────────┴────────────────┘  │
└───────────────────────────┬───────────────────────────────┘
                            │
                            ▼
                    ┌───────────────┐
                    │ OpenCode CLI  │
                    │ (actual work) │
                    └───────────────┘
```

**Key Features**:
- Full OpenCode CLI functionality via HTTP
- Session management and restoration
- Question tool handling (interactive prompts)
- Event streaming via SSE
- Persistent session storage

**Strengths**:
- Uses actual OpenCode CLI (not reimplementation)
- Programmatic control via HTTP API
- Can control OpenCode from anywhere (not just Emacs)
- OpenAPI spec for SDK generation

**Weaknesses**:
- Requires OpenCode server running separately
- Network overhead (even on localhost)
- Less direct integration than terminal approach

**Implementation Status**:
- 146 commits, actively maintained
- Handles question tool (2026-02-10)
- Session persistence

---

### 4. manzaltu/claude-code-ide.el (MCP Integration)

**URL**: https://github.com/manzaltu/claude-code-ide.el

**Approach**: Terminal integration (vterm/eat) + MCP tools server

**Architecture**:
```
┌─────────────────────────────────────────────────────────────┐
│                    Emacs + claude-code-ide                  │
│                                                             │
│  ┌──────────────────┬─────────────────┬─────────────────┐  │
│  │ Terminal         │ MCP Tools       │ Project         │  │
│  │ (vterm/eat)      │ Server          │ Management      │  │
│  │                  │                 │                 │  │
│  │ • Color support  │ • xref          │ • Per-project   │  │
│  │ • Smart render   │ • tree-sitter   │   sessions      │  │
│  │ • Anti-flicker   │ • imenu         │ • Multi-project │  │
│  └──────────────────┴─────────────────┴─────────────────┘  │
└─────────────────────────┬───────────────────────────────────┘
                          │ MCP (HTTP)
                          ▼
                    ┌─────────────┐
                    │ Claude Code │
                    │     CLI     │
                    └─────────────┘
```

**Key Features**:
- **Bidirectional MCP Bridge**: Claude can call Emacs functions
- **Terminal Integration**: Full vterm or eat support
- **Project-Aware**: Automatic session per project
- **Ediff Integration**: Review/modify suggestions before applying
- **Diagnostics**: Flycheck/Flymake integration
- **Custom Tools**: Expose any Emacs function to Claude

**Built-in MCP Tools**:
- `xref-find-references` - Find all symbol references
- `xref-find-apropos` - Find symbols by pattern
- `treesit-info` - Tree-sitter AST analysis
- `imenu-list-symbols` - List file symbols
- `project-info` - Project metadata

**Creating Custom Tools**:
```elisp
(defun my-project-grep (pattern)
  "Search for PATTERN in current session's project."
  (claude-code-ide-mcp-server-with-session-context nil
    (let* ((project-dir default-directory)
           (results (shell-command-to-string
                    (format "rg -n '%s' %s" pattern project-dir))))
      results)))

(claude-code-ide-make-tool
 :function #'my-project-grep
 :name "my_project_grep"
 :description "Search for pattern in project files"
 :args '((:name "pattern"
          :type string
          :description "Pattern to search for")))

(claude-code-ide-emacs-tools-setup)
```

**Terminal Optimization**:
```elisp
;; Anti-flicker for vterm
(setq claude-code-ide-vterm-anti-flicker t)
(setq claude-code-ide-vterm-render-delay 0.005)  ; 5ms batching

;; Or use eat instead
(setq claude-code-ide-terminal-backend 'eat)
```

**Window Management**:
```elisp
;; Side window configuration
(setq claude-code-ide-window-side 'right)
(setq claude-code-ide-window-width 90)

;; Ediff behavior
(setq claude-code-ide-use-ide-diff t)
(setq claude-code-ide-focus-claude-after-ediff t)
(setq claude-code-ide-show-claude-window-in-ediff t)
```

**Strengths**:
- Most mature/active project (1.3k stars, 101 commits)
- Real Claude Code CLI integration
- MCP allows Claude to use Emacs features (LSP, tree-sitter, etc.)
- Visual diff review with ediff
- Extensive configuration options

**Weaknesses**:
- Requires terminal emulator (vterm or eat)
- More complex setup than pure gptel

**Setup**:
```elisp
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind (("C-c C-'" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup))
```

---

### 5. xenodium/agent-shell (OpenCode over ACP)

**URL**: https://github.com/xenodium/agent-shell

**Approach**: OpenCode control via ACP (Anthropic Command Protocol?)

**Status**: Supports subset of OpenCode commands

**Key Characteristics**:
- Manages own sessions
- Limited command support (possibly by design)
- Simpler than full integration

**Questions**:
- Is ACP a subset of MCP?
- Why subset of commands?
- Session management differences?

**Note**: Requires further investigation (content truncated in scrape)

---

## OpenCode's Webserver Discovery

**Command**: `opencode serve`

**Default**: http://127.0.0.1:4096

**Key Endpoints**:

### TUI Control
- `POST /tui/show-toast` - Show notifications
- `GET /tui/control/next` - Wait for control requests
- `POST /tui/control/response` - Respond to requests

### Events
- `GET /event` - SSE stream (server.connected, etc.)

### Auth
- `PUT /auth/:id` - Set credentials

### Documentation
- `GET /doc` - OpenAPI 3.1 spec

**Flags**:
- `--port` - Port (default: 4096)
- `--hostname` - Host (default: 127.0.0.1)
- `--mdns` - Enable mDNS discovery
- `--cors` - Allow browser origins

**Auth**:
- `OPENCODE_SERVER_PASSWORD` env var
- Username: `opencode`

**SDK**:
- JS/TS SDK available for type-safe clients
- Generate SDKs from OpenAPI spec

**Related**:
- `opencode web` - Browser UI mode
- sczi/opencode.el uses this API
- Could potentially be used from any language/environment

---

## Comparison Matrix

| Feature | mwolson | colobas | sczi | manzaltu | xenodium |
|---------|---------|---------|------|----------|----------|
| **Integration Type** | External | Tool Port | HTTP API | MCP Terminal | ACP |
| **OpenCode CLI** | ✅ (external) | ❌ (reimpl) | ✅ (server) | ✅ (terminal) | ✅ (subset) |
| **Emacs→Claude** | File sync | gptel tools | HTTP | MCP server | ACP |
| **Claude→Emacs** | ❌ | ❌ | ❌ | ✅ (MCP) | Partial? |
| **LSP Integration** | External | ✅ Built-in | ❌ | ✅ via MCP | Unknown |
| **Inline Completion** | ✅ minuet | ❌ | ❌ | ❌ | ❌ |
| **Visual Diffs** | ❌ | ❌ | ❌ | ✅ ediff | Unknown |
| **Session Management** | Manual | Via preset | ✅ | ✅ | ✅ |
| **Maintenance** | Personal | Unmaintained | Active | Active | Active |
| **Complexity** | Low | High | Medium | High | Low? |
| **Maturity** | Stable | Experimental | Growing | Mature | Unknown |

---

## Use Cases & Recommendations

### For Inline Completions
**Use**: minuet (mwolson approach)
- Cursor-style single suggestions
- Debounced, non-intrusive
- Works with any chat backend

### For Chat/Q&A
**Use**: gptel
- Standard for LLM chat in Emacs
- Backend-agnostic
- Stream support

### For Agentic Workflows

#### Option 1: External OpenCode (mwolson)
**Best for**: Simple, stable setup
- Run OpenCode in external terminal
- Rely on `auto-revert-mode` for sync
- Minimal Emacs integration

**Pros**:
- Simple, reliable
- No complex dependencies
- Works with any terminal

**Cons**:
- No bidirectional communication
- Manual context switching
- No visual diff review

#### Option 2: manzaltu/claude-code-ide.el
**Best for**: Deep integration, active development
- Terminal integration in Emacs
- MCP tools for Claude→Emacs
- Visual diff review with ediff
- Project-aware sessions

**Pros**:
- Most mature/active
- Claude can use Emacs features
- Visual diff review
- Extensive configuration

**Cons**:
- Complex setup
- Requires vterm or eat
- Higher resource usage

#### Option 3: sczi/opencode.el
**Best for**: HTTP API fans, programmatic control
- Control OpenCode via HTTP
- OpenAPI spec for extensions
- Session persistence

**Pros**:
- Clean HTTP API
- Language-agnostic control
- OpenAPI documentation

**Cons**:
- Requires separate server
- Network overhead
- No Claude→Emacs tools

#### Option 4: colobas/opencode.el
**Best for**: Experimental, tool-rich setup
- ⚠️ Unmaintained
- Comprehensive tool suite
- LSP integration

**Pros**:
- Rich tool descriptions
- Permission system
- LSP-powered

**Cons**:
- Unmaintained
- No actual OpenCode CLI
- Experimental codebase

---

## Integration Ideas

### Hybrid Approach
Combine strengths of different approaches:

1. **minuet** for inline completions (mwolson)
2. **gptel** for chat/Q&A (mwolson)
3. **manzaltu/claude-code-ide.el** for agentic workflows
   - Get bidirectional MCP tools
   - Visual diff review
   - Project management

**Config**:
```elisp
;; Inline completions
(use-package minuet
  :config
  (setopt minuet-n-completions 1))

;; Chat/Q&A
(use-package gptel
  :config
  (setopt gptel-backend 'opencode-zen))

;; Agentic workflows
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el")
  :bind (("C-c C-'" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup))
```

### Custom MCP Tools
Add project-specific tools to manzaltu setup:

```elisp
;; Example: Project-specific grep
(defun my-project-search (pattern)
  (claude-code-ide-mcp-server-with-session-context nil
    (shell-command-to-string (format "rg -n '%s'" pattern))))

(claude-code-ide-make-tool
 :function #'my-project-search
 :name "project_search"
 :description "Search in project"
 :args '((:name "pattern" :type string :description "Search pattern")))
```

### Provider Sync
Adopt mwolson's pattern for keeping configs DRY:

```elisp
(defun my-sync-llm-providers ()
  "Sync minuet and gptel provider configs."
  ;; Implementation based on mwolson's approach
  )
```

---

## Key Learnings

### 1. OpenCode Has a Webserver!
- `opencode serve` exposes HTTP API
- OpenAPI 3.1 spec at `/doc`
- TUI control endpoints for IDE integration
- Could be used from any language

### 2. Multiple Integration Patterns
- **File sync** (mwolson) - Simple but one-way
- **Tool porting** (colobas) - Reimplements in Elisp
- **HTTP API** (sczi) - Clean but requires server
- **MCP terminal** (manzaltu) - Deep bidirectional integration
- **ACP subset** (xenodium) - Simpler, limited commands

### 3. Bidirectional is Key
Only manzaltu/claude-code-ide.el provides Claude→Emacs tools:
- Claude can call xref-find-references
- Claude can use tree-sitter for AST analysis
- Claude can access project metadata
- Makes Claude truly Emacs-aware

### 4. Visual Diff Review Matters
manzaltu's ediff integration allows:
- See exactly what Claude wants to change
- Modify suggestions before applying
- Better control over code changes

### 5. Provider Consolidation
mwolson's OpenCode Zen approach:
- One provider for all models
- Easier model switching
- Unified billing/credits
- Better stability than multi-provider

---

## Next Steps

### Short Term
1. Try manzaltu/claude-code-ide.el in current setup
2. Keep minuet for inline completions
3. Use gptel for Q&A (as backup)

### Medium Term
1. Define custom MCP tools for common workflows
2. Sync provider configs (adopt mwolson's pattern)
3. Document personal configuration

### Long Term
1. Explore OpenCode webserver for scripting
2. Consider contributing to active projects
3. Possibly combine best features into personal config

---

## Resources

### Documentation
- OpenCode Server API: https://opencode.ai/docs/server/
- OpenCode SDK: https://opencode.ai/docs/sdk/
- OpenCode CLI: https://opencode.ai/docs/cli/

### Projects
- mwolson setup: https://mwolson.org/guides/emacs-ai-setup/
- mwolson config: https://github.com/mwolson/emacs-shared
- colobas/opencode.el: https://github.com/colobas/opencode.el
- sczi/opencode.el: https://codeberg.org/sczi/opencode.el
- manzaltu/claude-code-ide.el: https://github.com/manzaltu/claude-code-ide.el
- xenodium/agent-shell: https://github.com/xenodium/agent-shell
- minuet: https://github.com/milanglacier/minuet-ai.el
- gptel: https://github.com/karthink/gptel

### Related
- gptel-fn-complete: https://github.com/mwolson/gptel-fn-complete
- OpenCode Zen: https://opencode.ai/docs/zen

---

*Last Updated: 2026-02-13*
*Status: Initial research, not yet implemented*
