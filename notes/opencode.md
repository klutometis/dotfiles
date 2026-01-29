# opencode wrapper script

This script is a wrapper for `opencode-ai` that allows for easy management of different configurations and models using profiles and flags.

## Usage

```bash
opencode [flags] -- [opencode arguments]
```

### Flags

-   `-p, --profile <name>`: Specifies the profile to use. The available profiles are `personal` (default) and `work`.
-   `-m, --model <model_name>`: Specifies the model to use. This overrides the default model of the selected profile.
-   `-c, --config <path>`: Specifies the path to the configuration file. This overrides the default configuration of the selected profile.

### Profiles

#### `personal` (default)

This profile uses the default `opencode` configuration. No `OPENCODE_CONFIG` environment variable is set.

#### `work`

This profile is configured for work-related tasks. It sets the following defaults, which can be overridden by the `--model` and `--config` flags:

-   **Config file**: `~/.config/opencode/opencode-work.json`
-   **Model**: `google-vertex-anthropic/claude-sonnet-4-5@20250929`

### Examples

**Run with the default `personal` profile:**
```bash
opencode acp
```

**Run with the `work` profile:**
```bash
opencode --profile=work acp
```

**Run with the `work` profile but override the model:**
```bash
opencode --profile=work --model=google/gemini-3-flash-preview acp
```

**Run with a custom config file:**
```bash
opencode --config=~/my-custom-opencode.json acp
```
---
