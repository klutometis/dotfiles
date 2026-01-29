# opencode wrapper script

This script is a wrapper for `opencode-ai` that allows for easy profile selection.

## Usage

```bash
opencode [--profile work] [-- opencode arguments]
```

### Flags

-   `-p, --profile work`: Use work profile (optional)

### Profiles

#### Default (personal)

Uses the default opencode configuration at `~/.config/opencode/opencode.json`.

#### Work profile

Uses work-specific configuration at `~/.config/opencode/opencode-work.json` with:
-   **Model**: `google-vertex-anthropic/claude-sonnet-4-5@20250929`
-   **Small model**: `google-vertex-anthropic/claude-sonnet-4-5@20250929`
-   No MCP servers configured (add as needed)

### Examples

**Run with default personal profile:**
```bash
opencode acp
```

**Run with work profile:**
```bash
opencode --profile=work acp
```
---
