# MCP Server Configurations

## Overview

This document catalogs MCP (Model Context Protocol) servers that have been tested, configured, or considered for use with this repository. It serves as a reference for future experimentation and documents why certain servers were accepted or rejected.

## Configuration Files

### Aider
**Location:** `etc/mcp.json`  
**Servers:** All servers (local + proxied)
- `git` - Local git operations
- `shell` - Shell command execution
- `context7`, `firecrawl`, `linkup`, `openmemory`, `perplexity` - Proxied API services

### Claude Code
**Location:** `~/.config/claude-code/mcp_settings.json`  
**Dotfile source:** `~/etc/dotfiles/dot-config/claude-code/mcp_settings.json`  
**Servers:** Only proxied API services
- `context7`, `firecrawl`, `linkup`, `openmemory`, `perplexity`  
**Rationale:** Claude Code has built-in `Bash` tool that handles git and shell operations, so local MCP servers are redundant

**Setup:** Symlinked via `~/bin/bootstrap-dotfiles.sh` (which runs `stow -v --restow --adopt dotfiles`)

To update MCP configuration:
- **Aider:** Edit `etc/mcp.json` and restart Aider
- **Claude Code:** Edit `etc/dotfiles/dot-config/claude-code/mcp_settings.json`, run `~/bin/bootstrap-dotfiles.sh`, and restart Claude Code

## Active Servers

See `etc/mcp.json` for currently enabled servers.

**Current setup:**
- **Local servers** - Run directly via `uvx`/`npx`, have access to current working directory
- **Proxied servers** - Accessed through `mcp-proxy` at `http://127.0.0.1:3100`, for remote API services
- **Proxy startup** - Use `~/bin/start-mcp-proxy` to start the proxy server

---

## Local Servers (Filesystem Context Required)

These servers need access to the current working directory and should run directly via `uvx`/`npx` rather than through `mcp-proxy`.

### git (Active)

**Status:** ✅ Currently active via `mcp-server-git`

**Why local:** Git operations must be relative to aider's working directory (the repo).

```json
"git": {
  "command": "uvx",
  "args": ["mcp-server-git"]
}
```

### git-mcp-server (Alternative)

**Status:** ⚠️ Alternative git implementation  
**Notes:** More feature-rich but `mcp-server-git` is simpler and sufficient for current needs.

```json
"git-mcp-server": {
  "command": "npx",
  "args": ["@cyanheads/git-mcp-server"],
  "env": {
    "MCP_LOG_LEVEL": "info",
    "GIT_SIGN_COMMITS": "false"
  }
}
```

### filesystem

**Status:** ❌ Rejected - too broad, security concerns  
**Issue:** Grants access to entire home directory; prefer more targeted tools.  
**Use case:** General filesystem read/write operations.

```json
"filesystem": {
  "command": "npx",
  "args": [
    "-y",
    "@modelcontextprotocol/server-filesystem",
    "/home/danenberg"
  ]
}
```

**Alternative with environment variable:**
```json
"filesystem": {
  "command": "npx",
  "args": [
    "envmcp",
    "npx",
    "-y",
    "@modelcontextprotocol/server-filesystem",
    "$HOME"
  ]
}
```

### shell

**Status:** ❌ Rejected - security concerns  
**Issue:** Arbitrary command execution, even with allowlist. Potential for abuse.  
**Use case:** Execute shell commands from AI assistant.

**Version 1: mcp-shell-server**
```json
"shell": {
  "command": "uvx",
  "args": ["mcp-shell-server"],
  "env": {
    "ALLOW_COMMANDS": "ls,cat,pwd,grep,wc,touch,find,git"
  }
}
```

**Version 2: @mkusaka/mcp-shell-server**
```json
"shell": {
  "command": "npx",
  "args": ["-y", "@mkusaka/mcp-shell-server"]
}
```

**Version 3: With envmcp wrapper**
```json
"shell": {
  "command": "npx",
  "args": [
    "envmcp",
    "npx",
    "-y",
    "@mkusaka/mcp-shell-server"
  ]
}
```

---

## Proxied Servers (Remote API Services)

These servers access remote APIs and don't depend on local filesystem context. They're accessed through `mcp-proxy`.

### context7 (Active)

**Status:** ✅ Currently active  
**Use case:** Documentation lookup for libraries and frameworks.

```json
"context7": {
  "command": "mcp-proxy",
  "args": ["http://127.0.0.1:3100/servers/context7/sse"]
}
```

### firecrawl (Active)

**Status:** ✅ Currently active  
**Use case:** Web scraping, crawling, content extraction.

```json
"firecrawl": {
  "command": "mcp-proxy",
  "args": ["http://127.0.0.1:3100/servers/firecrawl/sse"]
}
```

**Alternative: Direct connection (not recommended)**
```json
"firecrawl-mcp": {
  "command": "npx",
  "args": ["-y", "firecrawl-mcp"],
  "env": {
    "FIRECRAWL_API_KEY": "fc-9a718b37f9254c02b82c6635748b94c0"
  }
}
```

### linkup (Active)

**Status:** ✅ Currently active  
**Use case:** Web search with source citations.

```json
"linkup": {
  "command": "mcp-proxy",
  "args": ["http://127.0.0.1:3100/servers/linkup/sse"]
}
```

**Alternative: Direct connection (not recommended)**
```json
"linkup": {
  "command": "uvx",
  "args": ["mcp-search-linkup"],
  "env": {
    "LINKUP_API_KEY": "fc0e8db5-b24e-45f3-9e56-52fe9b12d9c1"
  }
}
```

### openmemory (Active)

**Status:** ✅ Currently active  
**Use case:** Persistent memory/context storage across sessions.

```json
"openmemory": {
  "command": "mcp-proxy",
  "args": ["http://127.0.0.1:3100/servers/openmemory/sse"]
}
```

**Alternative: Direct connection (not recommended)**
```json
"openmemory": {
  "command": "npx",
  "args": ["-y", "openmemory"],
  "env": {
    "OPENMEMORY_API_KEY": "om-kz4seudfndr9dy7dlr62wueou8qbbmjz",
    "CLIENT_NAME": "openmemory"
  }
}
```

### perplexity (Active)

**Status:** ✅ Currently active  
**Use case:** AI-powered search and research.

```json
"perplexity": {
  "command": "mcp-proxy",
  "args": ["http://127.0.0.1:3100/servers/perplexity/sse"]
}
```

**Alternative: perplexity-ask via envmcp**
```json
"perplexity-ask": {
  "command": "npx",
  "args": [
    "envmcp",
    "npx",
    "-y",
    "server-perplexity-ask"
  ],
  "env": {
    "PERPLEXITY_API_KEY": "$PERPLEXITY_API_KEY"
  }
}
```

### google-pse-mcp

**Status:** ⚠️ Not configured - requires Google API setup  
**Issue:** Need to obtain Google Custom Search API key and CX (Search Engine ID).  
**Use case:** Google Custom Search integration.

```json
"google-pse-mcp": {
  "command": "npx",
  "args": [
    "-y",
    "google-pse-mcp",
    "https://www.googleapis.com/customsearch",
    "<api_key>",
    "<cx>"
  ]
}
```

**Setup required:**
1. Create a Custom Search Engine at https://programmablesearchengine.google.com/
2. Get API key from Google Cloud Console
3. Replace `<api_key>` and `<cx>` with actual values

### gitmcp

**Status:** ⚠️ Experimental - remote git service  
**Notes:** Alternative to local git server; queries remote docs. Unclear advantage over local git + context7.

```json
"gitmcp": {
  "command": "npx",
  "args": [
    "envmcp",
    "npx",
    "-y",
    "mcp-remote",
    "https://gitmcp.io/docs"
  ]
}
```

---

## Configuration Patterns

### Environment Variables

**Direct (discouraged - exposes secrets in config):**
```json
"env": {
  "API_KEY": "actual-key-value"
}
```

**Environment variable reference (preferred):**
```json
"env": {
  "API_KEY": "${env:API_KEY}"
}
```

**Shell variable with envmcp wrapper:**
```json
"command": "npx",
"args": ["envmcp", "npx", "-y", "package-name"],
"env": {
  "API_KEY": "$API_KEY"
}
```

### Working Directory Considerations

**Problem:** When using `mcp-proxy`, the working directory is relative to where `mcp-proxy` was started, not where aider is running.

**Solution:**
- Local filesystem operations (git, filesystem, shell) → Run directly via `uvx`/`npx`
- Remote API services (firecrawl, linkup, etc.) → Proxy through `mcp-proxy`

---

## Testing New Servers

To test a new MCP server:

1. **Copy the configuration** from this document to `etc/mcp.json`
2. **Restart aider** to load the new configuration
3. **Test functionality** with relevant queries
4. **Document results:**
   - If accepted: Update `etc/mcp.json`, note here as active
   - If rejected: Document why in this file, remove from `etc/mcp.json`

---

## References

- [MCP Specification](https://modelcontextprotocol.io/)
- [Aider MCP Documentation](https://aider.chat/docs/mcp.html)
- [mcp-proxy](https://github.com/sparfenyuk/mcp-proxy)
