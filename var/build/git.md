# Git Configuration and Tips

## Hide Untracked Files in Local Repository

To stop Git from showing untracked files in status output for a specific repository:

```bash
git config --local status.showUntrackedFiles no
```

This sets the `status.showUntrackedFiles` configuration to `no` for the current repository only.

### Options for showUntrackedFiles:

- `no` - Show no untracked files
- `normal` - Show untracked files and directories (default)
- `all` - Show individual files in untracked directories

### To revert back to showing untracked files:

```bash
git config --local status.showUntrackedFiles normal
```

### To check current setting:

```bash
git config --local status.showUntrackedFiles
```

### Global vs Local Configuration

- Use `--local` to apply only to the current repository
- Use `--global` to apply to all repositories for the current user
- Use `--system` to apply system-wide for all users
