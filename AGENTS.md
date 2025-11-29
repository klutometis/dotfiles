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

## Common Workflows

### Quick Note and Commit

When you say "quick note and commit" or "QNC":

Reflect on the work completed since the last commit and:
1. Add a comprehensive summary to NOTES.md covering all changes and their significance
2. Stage all changes (`git add -A`)
3. Create a commit with an appropriate message

Don't just document the last immediate action - capture the full scope of work in the session.

## Git Commit Messages

Write commit messages that explain **what** changed and **why**. Use this format:

```
One-line summary (50 chars, imperative mood)

- The problem being solved
- What changed and why this approach
- Relevant context, links, or breaking changes
```

### Guidelines

- **Summary**: Imperative mood ("Add feature" not "Added"), capitalized, no period
- **Body**: Explain the why. Provide context. Use bullet points for clarity.
- **Before committing**: Review to ensure the message reflects final changes

### Google3 / Critique

For Critique CLs, append:
```
R=[reviewers]
BUG=[bug ID]
MARKDOWN=true
```
