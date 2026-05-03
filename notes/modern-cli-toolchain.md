# Modern CLI Toolchain

Rust-based replacements for classic Unix tools. Set up 2026-05-02.

## Files Changed

- `~/bin/configure-system.sh` — installs everything; re-runnable/idempotent
- `~/.zshrc` — aliases, shell integrations, key bindings

## Tools

| Replaces | Tool | Binary | Cargo crate | Source |
|----------|------|--------|-------------|--------|
| `cat` | bat | `bat` | `bat` | [sharkdp/bat](https://github.com/sharkdp/bat) |
| `grep` | ripgrep | `rg` | `ripgrep` | [BurntSushi/ripgrep](https://github.com/BurntSushi/ripgrep) |
| `find` | fd | `fd` | `fd-find` | [sharkdp/fd](https://github.com/sharkdp/fd) |
| `ls` | eza | `eza` | `eza` | [eza-community/eza](https://github.com/eza-community/eza) |
| `du` | dust | `dust` | `du-dust` | [bootandy/dust](https://github.com/bootandy/dust) |
| `git diff` | delta | `delta` | `git-delta` | [dandavison/delta](https://github.com/dandavison/delta) |
| `tldr` | tealdeer | `tldr` | `tealdeer` | [tealdeer-rs/tealdeer](https://github.com/tealdeer-rs/tealdeer) |
| `Ctrl+R` etc. | fzf | `fzf` | — (Go) | [junegunn/fzf](https://github.com/junegunn/fzf) |
| `cd` (additive) | zoxide | `zoxide` | `zoxide` | [ajeetdsouza/zoxide](https://github.com/ajeetdsouza/zoxide) |
| cheatsheet | navi | `navi` | `navi` | [denisidoro/navi](https://github.com/denisidoro/navi) |

## Ubuntu/Debian Naming Conflicts

apt uses different binary names for three tools due to naming conflicts with
unrelated packages. cargo installs the canonical names. `.zshrc` provides
reverse-compatibility shims so scripts expecting the old names still work.

| Tool | apt binary | cargo binary | Shim added |
|------|-----------|--------------|------------|
| bat | `batcat` | `bat` | `alias batcat='bat'` |
| fd | `fdfind` | `fd` | `alias fdfind='fd'` |
| delta | `git-delta` | `delta` | `alias git-delta='delta'` |

## Installation Method

All Rust tools: `cargo install [--locked] <crate>`. cargo skips if already
at the latest version so re-running configure-system.sh is safe.

fzf is Go-based: `go install github.com/junegunn/fzf@latest` → `~/go/bin/fzf`.

Canonical install per each project's own GitHub README was checked before
writing the script; no apt, no PPAs, no snaps.

## Shell Integrations (.zshrc)

### fzf key bindings (via `source <(fzf --zsh)`)

| Key | Action |
|-----|--------|
| `Ctrl+R` | Fuzzy history search |
| `Ctrl+T` | Fuzzy file picker, inserts path at cursor |
| `Alt+C` | Fuzzy cd into subdirectory |

fzf is configured to use fd for file listing (respects `.gitignore`, includes
hidden files) and bat for syntax-highlighted previews on `Ctrl+T`.

### navi — `Ctrl+G`

Interactive cheatsheet widget. First invocation downloads community cheatsheets
to `~/.local/share/navi/cheats/`. Add your own `.cheat` files there.

### zoxide — `z`

Smarter cd. Learns frecency from normal navigation; no explicit seeding needed.
`z <partial>` jumps to best match. `zi <partial>` opens fzf picker.

## delta git config

delta is configured globally as the git pager:

```
core.pager = delta
interactive.diffFilter = delta --color-only
delta.navigate = true
delta.side-by-side = true
delta.line-numbers = true
merge.conflictStyle = diff3
diff.colorMoved = default
```

## Upgrading

```bash
# Rust tools (all at once)
cargo install-update -a          # requires cargo-update crate
# or individually:
cargo install --locked bat       # etc.

# fzf
cd && go install github.com/junegunn/fzf@latest

# Re-run the full toolchain section
~/bin/configure-system.sh
```

## MANPAGER

bat is set as the man page pager:

```zsh
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
```

`man <anything>` now renders with syntax highlighting.
