# Conventions

## Documentation Conventions

### TODO.md vs NOTES.md

- **TODO.md**: Actionable tasks and checklists
- **NOTES.md**: Context, rationale, decisions, observations

### Scaling NOTES.md

When NOTES.md exceeds ~500 lines, use LLM to extract themes into topic files. Archive before extraction.

**Structure:**
- `notes/[TOPIC].md` - Living docs per system/feature (LLM-extracted themes)
- `notes/YYYY-MM-DD.md` - Snapshot before extraction (date pattern indicates archive)

**Post-compaction**: NOTES.md becomes a brief summary of recent decisions with pointers to topic files.

### Pruning TODO.md

When TODO.md exceeds ~300-400 lines, archive then prune completed work.

**Structure:**
- `todo/YYYY-MM-DD.md` - Full snapshots (date pattern indicates archive)

**Remove**: Completed sections, implementation details, resolved issues
**Keep**: Only in-progress tasks and active work

**Post-compaction**: TODO.md contains only undone/in-progress items (no summary of completed work).

### Archiving CHANGELOG.md

When CHANGELOG.md exceeds ~1000 lines, move old entries to archive. Keep recent ~6 months visible.

**Structure:**
- `changelog/YYYY-MM-DD.md` - Old releases (date pattern indicates archive)

**Post-compaction**: CHANGELOG.md retains recent releases for visibility. It's user-facing, not just internal notes.

## Package Management

Use package managers to install/update dependencies, not manual file edits. Avoids stale versions from training data.

- **Node.js**: `npm install package@latest`
- **Python**: `uv add package` (ensure `pyproject.toml` has `[tool.uv]` with `package = false` for dependency-only projects)
- **Rust**: `cargo add package`
- **Go**: `go get package@latest`

**General principle**: If a language has a standard package manager command, use it instead of editing dependency files (package.json, pyproject.toml, Cargo.toml, go.mod, etc.) directly.

## Git Operations

### Staging Files

**Never use `git add -A` or `git add .`** - Not all repos have complete .gitignore coverage. Stage specific files only.

### Commit Messages

Use conventional commits format (`feat:`, `fix:`, `docs:`, etc.). No tool attribution or co-authorship footers.
