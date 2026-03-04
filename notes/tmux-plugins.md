# Tmux Plugins

Managed by TPM (Tmux Plugin Manager). Plugins are declared in
`~/.tmux.conf` and installed to `~/.tmux/plugins/`.

## TPM itself

Plugin manager. Lives at `~/.tmux/plugins/tpm`.

| Keybinding | Action |
|---|---|
| `C-z I` | Install new plugins from `.tmux.conf` |
| `C-z U` | Update all plugins |
| `C-z alt-u` | Remove plugins not in `.tmux.conf` |

## tmux-sensible

Sane defaults that don't override your explicit settings. Adds:

- `history-limit 50000` (up from tmux's default 2000)
- `display-time 4000` (messages visible for 4s instead of 750ms)
- `focus-events on` (lets programs detect focus)
- `aggressive-resize on` (better multi-monitor with grouped sessions)
- `status-keys emacs` (emacs keys in the tmux command prompt)
- `C-z C-p` / `C-z C-n` for previous/next window

Your explicit settings (`escape-time 10`, `status-interval 10`,
`default-terminal`) take precedence over sensible's defaults.

## tmux-resurrect

Save and restore tmux sessions (windows, panes, layouts, working
directories) across server restarts.

| Keybinding | Action |
|---|---|
| `C-z C-s` | Save current session state |
| `C-z C-r` | Restore last saved session state |

Saves to `~/.tmux/resurrect/`. Each save creates a new file; the
latest is symlinked as `last`.

What it saves: sessions, windows, pane layouts, working directories,
pane contents (optional). What it does NOT save: running processes,
shell history, environment variables within panes.

## tmux-continuum

Auto-saves resurrect state every 15 minutes. Also auto-restores
on tmux server start (configured with `@continuum-restore 'on'`).

No keybindings. It just works in the background. The save interval
is set to 15 minutes via `@continuum-save-interval '15'`.

To check status: `tmux show-option -g @continuum-save-interval`

## tmux-yank

Copies tmux selections to the system clipboard via xclip/xsel.
Backup for when OSC 52 passthrough fails (e.g. nested sessions, SSH).

### In copy mode (`C-z [`):

| Keybinding | Action |
|---|---|
| `y` | Copy selection to system clipboard |
| `Y` | Copy selection and paste it to the command line |

### From normal mode:

| Keybinding | Action |
|---|---|
| `C-z y` | Copy current command line to clipboard |
| `C-z Y` | Copy current working directory to clipboard |

Mouse selection copies to system clipboard automatically
(`@yank_selection_mouse 'clipboard'` is configured).

Works alongside OSC 52 -- they're independent clipboard paths.
tmux-yank talks to X11 directly; OSC 52 goes through the terminal.

Note: tmux 3.6+ requires `terminal-features` to declare clipboard
support per terminal. We set `alacritty:clipboard` in .tmux.conf
so both OSC 52 and tmux-yank work.

## tmux-copycat

Regex search in scrollback with pre-defined pattern shortcuts.

| Keybinding | Action |
|---|---|
| `C-z /` | Regex search (prompts for pattern) |
| `C-z C-f` | Search for file paths |
| `C-z C-u` | Search for URLs |
| `C-z C-d` | Search for numbers (digits) |
| `C-z alt-h` | Search for SHA-1/SHA-256 hashes |
| `C-z alt-i` | Search for IP addresses |

Once in search results: `n` / `N` to jump between matches.
`Enter` to copy the highlighted match.

## tmux-fingers

Visual hint-based text grabbing. Overlays letter hints on
"grabbable" patterns in the current pane.

| Keybinding | Action |
|---|---|
| `C-z F` | Enter fingers mode |

### While in fingers mode:

| Key | Action |
|---|---|
| `a`-`z` | Copy highlighted match to clipboard |
| `Ctrl` + `a`-`z` | Copy and open (e.g. URLs in browser) |
| `Shift` + `a`-`z` | Copy and paste into command line |
| `Tab` | Toggle multi-select mode |
| `q` / `Esc` / `C-c` | Exit fingers mode |

### Default patterns detected:

- File paths
- Git SHAs
- Numbers (4+ digits)
- Hex numbers
- IP addresses
- Kubernetes resources
- UUIDs
- Git status/diff output

## tmux-fuzzback

Fuzzy search the entire scrollback buffer using fzf.

| Keybinding | Action |
|---|---|
| `C-z ?` | Open fzf with full scrollback |

Standard fzf interface: type to fuzzy-search, arrow keys to
navigate, Enter to select. Selected line is copied to clipboard.

## Quick reference

| Want to... | Do this |
|---|---|
| Save session | `C-z C-s` |
| Restore session | `C-z C-r` |
| Copy selection to clipboard | `y` in copy mode |
| Regex search scrollback | `C-z /` |
| Find URLs in scrollback | `C-z C-u` |
| Find file paths in scrollback | `C-z C-f` |
| Grab visible text with hints | `C-z F` |
| Fuzzy search all scrollback | `C-z ?` |
| Install new plugins | `C-z I` |
| Update plugins | `C-z U` |
