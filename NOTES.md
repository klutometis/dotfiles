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
