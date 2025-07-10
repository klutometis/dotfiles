# Conventions

## Build Documentation and Scripts

For build instructions and automation:

- **Documentation**: Place detailed build instructions in `var/build/NAME.md`
- **Scripts**: Place the corresponding automated build script in `bin/build/NAME.sh`

For example:
- `var/build/emacs.md` - detailed instructions for building Emacs
- `bin/build/emacs.sh` - automated script that implements those instructions

The script should be a distillation of the documentation in executable form.

## Git Commit Messages

Use the following format for commit messages:

```
One-line summary of the change

- Detailed point about what was changed
- Another detail about the implementation
- Any relevant context or reasoning
- Breaking changes or deprecations if applicable
```

### Guidelines

- Keep the summary line under 50 characters
- Use imperative mood ("Add feature" not "Added feature")
- Capitalize the first letter of the summary
- No period at the end of the summary line
- Leave a blank line between summary and bullet points
- Use bullet points for detailed explanations
- Reference issue numbers when applicable
