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
