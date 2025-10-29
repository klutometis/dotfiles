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
