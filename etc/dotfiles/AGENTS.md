# Conventions

## Working with Aider

When working in an aider session, use aider's built-in capabilities first:

- **Files in context**: If a file is already added to the chat, edit it directly using search/replace blocks. Don't use shell commands to read or rewrite it.
- **Shell MCP fallback**: Use shell commands only for things aider can't do natively: running tests, checking system state, git operations, installing packages, etc.
- **Surgical edits**: For file modifications, use aider's search/replace blocks rather than rewriting entire files with `cat >`.

The shell MCP is powerful but expensive in context. Use aider's native features when available.

## Build Documentation and Scripts

For build instructions and automation:

- **Documentation**: Place detailed build instructions in `var/build/NAME.md`
- **Scripts**: Place the corresponding automated build script in `bin/build/NAME.sh`

For example:
- `var/build/emacs.md` - detailed instructions for building Emacs
- `bin/build/emacs.sh` - automated script that implements those instructions

The script should be a distillation of the documentation in executable form.

## Documentation Conventions

### TODO.md vs NOTES.md

- **TODO.md**: Concrete, actionable tasks and checklists. Items that can be checked off when complete.
- **NOTES.md**: Context, rationale, decisions, future directions, and non-actionable observations. Things to remember but not necessarily do.

### Scaling NOTES.md for Long-Running Projects

For projects where NOTES.md grows unwieldy (>500 lines), consider this structure:

- **notes/topics/[TOPIC].md** - Living documentation per system/feature
- **notes/archive/YYYY-MM-DD.md** - Historical snapshots

See detailed workflow in project-specific AGENTS.md files.

### Pruning TODO.md

TODO.md should be forward-looking, not archival. When it exceeds ~300-400 lines, prune completed work.

**What to remove:**
- Fully completed sections (all checkboxes checked)
- Implementation details of finished features
- Old known issues that were resolved

**What to keep:**
- Current/in-progress tasks (any section with unchecked boxes)
- High-level milestone structure (phase headers)
- Brief one-line summary of completed phases

**Archive before pruning:**
```bash
cp TODO.md TODO-archive-$(date +%Y-%m-%d).md
# Then manually prune TODO.md
```

**Example transformation:**

Before (60 lines):
```markdown
### v1 - Cinematic Spawn - COMPLETED ✓
- [x] Implement falling spawn
  - [x] Players spawn at 2500 studs
  - [x] Camera locked looking down
  - [x] Sigmoid deceleration at 200 studs
  ... 15 more completed items
```

After (2 lines):
```markdown
### v1 - Cinematic Spawn - COMPLETED ✓
Brief: Players spawn high and descend cinematically with camera transitions.
```

Or even simpler (1 line):
```markdown
- [x] v1: Cinematic spawn system
```

**Philosophy**: Completed work is already documented in git history, NOTES.md/topic files, and the code itself. TODO.md doesn't need to maintain historical checklists.

### TODO.md → CHANGELOG.md → Archive Flow

When pruning TODO.md, completed work should flow to CHANGELOG.md before being archived.

**Workflow:**

1. **Active development**: Tasks live in TODO.md
2. **Completion**: When a feature/phase is complete, move summary to CHANGELOG.md
3. **Archival**: When CHANGELOG.md grows large (>1000 lines), archive old entries

**CHANGELOG.md structure:**

```markdown
# Changelog

## [Unreleased]
- Feature in progress

## [1.2.0] - 2026-01-15
### Added
- Cinematic spawn system with camera transitions
- OAuth2 login support

### Fixed
- Timeout on large API requests

### Changed
- Simplified error handling logic
```

**Archiving changelogs:**

When CHANGELOG.md becomes unwieldy:

```bash
# Archive entries older than 6 months
mkdir -p changelog/archive
mv CHANGELOG.md changelog/archive/CHANGELOG-2025.md
# Create fresh CHANGELOG.md with recent entries
```

Or use dated archives similar to notes:
- **changelog/archive/2025-H1.md** - First half of 2025
- **changelog/archive/2025-H2.md** - Second half of 2025

**Philosophy**: CHANGELOG.md is user-facing release history. Archives preserve the full timeline without cluttering the main file.

## Package Management

### Node.js / npm

When adding or updating npm packages:

- **Use npm commands**: Run `npm install package@latest` to get current versions
- **Don't edit package.json directly**: Avoid hardcoding version numbers, which may be outdated from training data (2023)
- **Let npm manage versions**: This ensures you get the actual latest versions and proper dependency resolution

Example:
```bash
npm install react@latest
npm install --save-dev typescript@latest
```

### Python / pip

When adding or updating Python packages:

- **Use pip/uv commands**: Run `pip install package` or `uv add package` to get current versions
- **Don't edit requirements.txt/pyproject.toml directly**: Let the package manager handle version resolution
- **For specific versions**: Only hardcode versions when there's a compatibility requirement

## Git Operations

### Staging Files

Be surgical when staging files for commits:

- **Stage specific files**: Use `git add <file>` for files you've actually modified
- **Never use `git add -A` or `git add .`**: Not all repos have complete .gitignore coverage
- **Check what you're adding**: Run `git status` to see untracked files before staging
- **Avoid noise**: Don't stage build artifacts, editor configs, or other unrelated files

Example workflow:
```bash
git status                    # Review what changed
git add src/feature.ts        # Stage only relevant files
git add tests/feature.test.ts
git commit -m "feat: add feature"
```

### Commit Messages

Use semantic commit messages following conventional commits format. This makes history readable and enables automation.

### Format

```
<type>(<scope>): <description>

<body>

<footer>
```

### Types

- **feat**: new feature
- **fix**: bug fix
- **docs**: documentation changes
- **style**: formatting changes (no code change)
- **refactor**: code restructuring (no behavior change)
- **test**: add or update tests
- **chore**: maintenance tasks (dependencies, build tools)
- **build**: build system changes
- **ci**: CI configuration changes
- **perf**: performance improvements

### Guidelines

- **Description**: imperative mood ("add" not "added"), lowercase, no period, under 50 chars
- **Body**: explain what and why (not how), wrap at 100 chars, use bullets for clarity
- **Footer**: reference issues/PRs, note breaking changes
- **Breaking changes**: add `!` after type/scope (e.g., `feat(api)!: change endpoint format`)
- **No attribution footers**: Don't add tool attribution or co-authorship footers to commits

### Examples

```
feat(auth): add OAuth2 login support

fix(api): resolve timeout on large requests

docs(readme): update installation steps

refactor: simplify error handling logic
```

### Google3 / Critique

For Critique CLs, append:
```
R=[reviewers]
BUG=[bug ID]
MARKDOWN=true
```
