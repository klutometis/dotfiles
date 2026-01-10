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

## Git Commit Messages

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
