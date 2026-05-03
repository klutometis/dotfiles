# Handoff: Modern CLI Toolchain Setup (Linux)


## Context


The user is upgrading their CLI toolchain from classic Unix tools (grep, cat, find, ls, etc.) to
modern Rust-based replacements. This doc is for Claude to execute from the `pi` opencode instance
on a Linux system.


**Philosophy:** Aliases should be transparent replacements, not nags. `alias cat='bat'` is correct.
`alias cat='cowsay "use bat" | lolcat'` is funny for 10 minutes and breaks everything forever.
The muscle memory transfer happens *passively* — old command fires, new tool runs, habit forms.


---


## Step 0: Detect Environment


Before installing anything, determine the distro and package manager:


```bash
cat /etc/os-release
which apt || which dnf || which pacman || which zypper
```


**Critical Ubuntu/Debian gotchas:**
- `bat` installs as `batcat` (naming conflict with another package)
- `fd` installs as `fdfind` (naming conflict)
- `delta` apt package is called `git-delta`
- `eza`, `dust`, `zoxide` may not be in apt — fall back to cargo or GitHub releases
- Aliases must account for these binary name differences (see aliases section)


---


## Step 1: Install via Package Manager


### Ubuntu / Debian


```bash
sudo apt update
sudo apt install -y \
  ripgrep \
  fd-find \
  bat \
  fzf \
  btop \
  tldr \
  git-delta \
  eza
```


Then check which of these actually installed and which binary names they used:


```bash
which rg || echo "MISSING: ripgrep"
which fdfind || which fd || echo "MISSING: fd"
which batcat || which bat || echo "MISSING: bat"
which fzf || echo "MISSING: fzf"
which btop || echo "MISSING: btop"
which tldr || echo "MISSING: tldr"
which delta || echo "MISSING: delta"
which eza || echo "MISSING: eza"
```


### For anything missing (dust, zoxide, or eza if apt failed):


Option A — cargo (requires Rust; install if needed: `curl https://sh.rustup.rs -sSf | sh`):


```bash
cargo install du-dust
cargo install zoxide
cargo install eza
```


Option B — GitHub releases (no Rust needed). For each missing tool, download the appropriate
Linux binary from:
- zoxide: https://github.com/ajeetdsouza/zoxide/releases
- dust: https://github.com/bootandy/dust/releases
- eza: https://github.com/eza-community/eza/releases


Download, extract, and place in `~/.local/bin/` or `/usr/local/bin/`.


---


## Step 2: Detect Shell


```bash
echo $SHELL
```


Edit the appropriate rc file:
- bash → `~/.bashrc`
- zsh  → `~/.zshrc`
- fish → `~/.config/fish/config.fish` (aliases use different syntax — see note below)


---


## Step 3: Add Aliases and Shell Integrations


Append the following block to `~/.bashrc` (or `~/.zshrc`). **Read the gotcha comments** — the
Ubuntu binary name differences matter.


```bash
# ============================================================
# Modern CLI Toolchain — transparent alias replacements
# Installed: bat, ripgrep (rg), fd, eza, zoxide, fzf, delta,
#            btop, dust, tldr, navi
# ============================================================


# --- bat (replaces cat) ---
# Ubuntu/Debian installs bat as "batcat"; other distros as "bat"
if command -v batcat &>/dev/null; then
  alias bat='batcat'
  alias cat='batcat'
elif command -v bat &>/dev/null; then
  alias cat='bat'
fi
# bat as colorized man pager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"


# --- ripgrep (replaces grep) ---
# rg is the binary name on all distros
if command -v rg &>/dev/null; then
  alias grep='rg'
fi


# --- fd (replaces find) ---
# Ubuntu/Debian installs as "fdfind"; other distros as "fd"
if command -v fdfind &>/dev/null; then
  alias fd='fdfind'
  alias find='fdfind'
elif command -v fd &>/dev/null; then
  alias find='fd'
fi


# --- eza (replaces ls) ---
if command -v eza &>/dev/null; then
  alias ls='eza'
  alias ll='eza -la --git'
  alias lt='eza --tree --level=2'
  alias la='eza -a'
fi


# --- zoxide (replaces cd — note: 'z' not 'cd'; see init below) ---
# zoxide uses 'z' as its command, not aliased over 'cd'
# It learns frecency over time; seed manually with: zoxide add /path


# --- delta (replaces git's built-in diff pager — configured in .gitconfig) ---
# See Step 4 below


# --- tldr (quick reference) ---
# No alias needed; just use 'tldr <command>' directly
# Run 'tldr --update' once after install to fetch pages


# --- dust (replaces du) ---
if command -v dust &>/dev/null; then
  alias du='dust'
fi


# ============================================================
# Shell integrations (must come after PATH is set)
# ============================================================


# fzf — fuzzy finder shell keybindings
# Ctrl+R: fuzzy history search (replaces default reverse-i-search)
# Ctrl+T: fuzzy file picker inserted at cursor
# Alt+C:  fuzzy cd into subdirectory
if command -v fzf &>/dev/null; then
  # Ubuntu/Debian path:
  [ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && \
    source /usr/share/doc/fzf/examples/key-bindings.bash
  # Generic path (from source/cargo install):
  [ -f ~/.fzf.bash ] && source ~/.fzf.bash


  # fzf + bat: file preview with syntax highlighting
  export FZF_DEFAULT_OPTS="--preview 'bat --color=always --style=numbers --line-range=:200 {}' --preview-window=right:50%:wrap"


  # fzf + rg: use ripgrep for file listing (respects .gitignore)
  if command -v rg &>/dev/null; then
    export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  fi
fi


# zoxide — smart directory jumper
# Usage: z <partial-dir-name>  (e.g. 'z proj' jumps to ~/projects/myproject)
# Usage: zi — interactive fuzzy picker (requires fzf)
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init bash)"   # change 'bash' to 'zsh' if on zsh
fi
```


**For zsh:** change `eval "$(zoxide init bash)"` → `eval "$(zoxide init zsh)"`


**For fish:** syntax is entirely different. Skip this block and instead run:
```fish
zoxide init fish | source
# and use 'alias' replaced by 'abbr' or functions
```


---


## Step 4: Configure delta as Git Pager


```bash
git config --global core.pager delta
git config --global interactive.diffFilter "delta --color-only"
git config --global delta.navigate true
git config --global delta.side-by-side true
git config --global delta.line-numbers true
git config --global merge.conflictstyle diff3
git config --global diff.colorMoved default
```


Verify with `git diff` in any repo — should be syntax-highlighted and split side-by-side.


---


## Step 5: Apply and Verify


```bash
source ~/.bashrc   # or ~/.zshrc


# Spot-check each tool:
echo "hello world" | cat          # should show bat output with line numbers
rg --version
fd --version 2>/dev/null || fdfind --version
eza --version
z --version 2>/dev/null || echo "zoxide: use 'z' after opening a new shell"
fzf --version
btop --version
tldr --version
dust --version
delta --version
```


---


## Step 6: Seed zoxide


zoxide needs to learn frecency before `z` becomes useful. Seed it manually with directories you
visit constantly:


```bash
# Run these manually, or adapt to the user's actual frequent dirs:
zoxide add ~
zoxide add ~/projects   # or wherever code lives
zoxide add /tmp
# Then just navigate normally — zoxide learns over time
```


---


## Step 7 (Optional): navi — interactive cheatsheet widget


`navi` is the closest thing to Anki for CLI muscle memory. It surfaces command templates at the
moment of need. Press Ctrl+G at any prompt to fuzzy-search commands.


```bash
# Install via cargo:
cargo install navi
# Or via bash installer:
bash <(curl -sL https://raw.githubusercontent.com/denisidoro/navi/master/scripts/install)


# Add to .bashrc / .zshrc:
eval "$(navi widget bash)"   # or 'zsh'
```


After installing, Ctrl+G at any prompt opens the cheatsheet picker. Community cheatsheets are
bundled; you can add your own `.cheat` files in `~/.local/share/navi/cheats/`.


---


## Summary: What Got Set Up


| Old command | Now runs    | Notes                              |
|-------------|-------------|------------------------------------|
| `cat`       | `bat`       | syntax highlight, git markers      |
| `grep`      | `rg`        | faster, .gitignore-aware           |
| `find`      | `fd`        | simpler syntax, colored output     |
| `ls`        | `eza`       | colors, git status, tree mode      |
| `du`        | `dust`      | visual tree, human-readable        |
| `cd`        | `z` (additive) | frecency-based; `cd` still works |
| `git diff`  | `delta`     | syntax highlight, side-by-side     |
| `man`       | `bat -l man`| colorized man pages                |
| `Ctrl+R`    | `fzf`       | fuzzy history search               |
| `Ctrl+T`    | `fzf`       | fuzzy file insert at cursor        |


**Key things to actually use daily to build habit:**
1. `Ctrl+R` for history — this replaces itself immediately and is the fastest win
2. `ll` — you'll notice the git status column within hours
3. `z` — takes a week to learn your dirs, then it's magic
4. `tldr <anything>` — replaces Googling "rg examples"