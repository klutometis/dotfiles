# Plan: Migrate from alacritty+tmux to wezterm

## Context

Moving local terminal stack from {alacritty, tmux} to {kitty} for:
- Kitty Graphics Protocol → inline images in `emacs -nw` via kitty-graphics.el
- Kitty sessions → project-specific terminal launchers (replacing tmux locally)
- tmux stays for remote SSH persistence only

## Current State

- **Terminal launcher**: `~/bin/terminal` → alacritty + tmux
- **Alacritty config**: `~/.config/alacritty/alacritty.toml`
  - Font: `Fixed6x13` (bitmap — may not work in kitty, needs testing)
  - Colors: black bg (#000000), white fg (#ffffff), custom 16-color palette
  - No padding, no opacity settings
- **Shell**: zsh (oh-my-zsh, lambda theme)
- **Emacs**: built from source at `~/build/emacs`, config at `~/.emacs.d/init.el`
  - Package manager: elpaca + use-package
  - Already has: `eat`, `ai-code` (opencode via ai-code-interface.el)
  - Launches as `emacs -nw` (not daemon/emacsclient)
- **Workflow**: multiple terminal windows, each project-specific; emacs does
  heavy lifting (shells via eat, opencode via ai-code); ad-hoc stuff inside emacs
- **OS**: Debian rodete (Linux)
- **kitty**: not yet installed

## Decisions Made

1. **Prefix key**: `C-z` (not F7) — matches existing tmux muscle memory
2. **Default session**: full-screen emacs only, no split — all ad-hoc work inside emacs
3. **Font**: try `Fixed6x13` first; fallback to `DejaVu Sans Mono` (already installed)
4. **ssh.conf**: minimal — just `remote_kitty if-needed`; dotfiles are git-cloned on remotes
5. **No `startup_session`** in kitty.conf — sessions controlled per-invocation via `~/bin/terminal`
6. **No dotfile copying** via kitten ssh
7. **OS window title**: `terminal (<session>)` for local, `terminal (<session> @ <host>)` for remote — matches current convention
8. **kitty-graphics.el**: install via elpaca, guard with `KITTY_PID`

## Key Concepts to Remember

### kitty sessions are NOT tmux sessions
- Sessions are startup templates (layout + programs), not persistent state
- If the kitty window closes, it's gone — no reattach
- `save_as_session` snapshots current layout for re-creation
- `goto_session` switches between sessions within a single kitty process
- Session name comes from filename (e.g. `email.kitty-session` → "email")
- For persistence: rely on emacs auto-save/undo locally; tmux over SSH remotely

### KITTY_PID with multiple instances
- Each kitty OS process gets its own PID
- `KITTY_PID` env var is set per-instance, inherited by all children
- kitty-graphics.el just checks "am I in any kitty?" — no cross-talk
- Graphics protocol writes to the terminal emacs is attached to

### C-z prefix — no conflict with emacs
- Emacs captures `C-z` before kitty sees it (emacs uses it for undo/suspend-frame)
- Kitty only sees `C-z` when foreground is a plain shell
- In a bare shell, you lose `C-z` for job suspend (acceptable tradeoff)

### kitten ssh
- Copies terminfo to remote (so remote knows about xterm-kitty)
- Enables shell integration on remote (prompt tracking, CWD detection)
- Optionally copies `kitten` binary for graphics/file-transfer on remote
- Does NOT replace tmux for session persistence
- Ideal remote combo: `kitten ssh -t remote -- tmux new -A -s session`

---

## Implementation Tasks

### Phase 1: Install kitty

```sh
curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
```

Add to `~/.zshrc` (near top, PATH section):

```sh
export PATH="$HOME/.local/kitty.app/bin:$PATH"
```

Verify: `kitty --version` should print >= 0.38.0

### Phase 2: Create ~/.config/kitty/kitty.conf

```
# Fonts — try bitmap first; fallback commented below
font_family      Fixed6x13
bold_font        auto
italic_font      auto
bold_italic_font auto
# font_family      DejaVu Sans Mono
# font_size        13.0

# Cursor
cursor_shape beam
cursor_blink_interval -1

# Scrollback
scrollback_lines 10000

# Mouse
mouse_hide_wait 3.0
copy_on_select   no

# Performance
repaint_delay 8
input_delay   2
sync_to_monitor yes

# Bell
enable_audio_bell no

# Window
confirm_os_window_close -1

# Tab bar — show session name
tab_bar_edge bottom
tab_bar_style powerline
tab_powerline_style slanted
tab_bar_min_tabs 1
tab_title_template "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{session_name} {title}"
active_tab_font_style bold
# Only show tabs from current session
tab_bar_filter session:~ or session:^$

# Colors — ported from alacritty.toml
foreground #ffffff
background #000000
color0  #000000
color1  #ff7c4d
color2  #22ff00
color3  #ffcc00
color4  #3399ff
color5  #ff61df
color6  #00ffff
color7  #888888
color8  #000000
color9  #ff7c4d
color10 #22ff00
color11 #ffcc00
color12 #3399ff
color13 #ff61df
color14 #00ffff
color15 #ffffff

# Shell integration
shell_integration enabled

# Remote control (required for save_as_session, goto_session, etc.)
allow_remote_control socket-only
listen_on unix:/tmp/kitty-{kitty_pid}

# Hyperlinks
allow_hyperlinks yes

# Clipboard (match alacritty osc52 behavior)
clipboard_control write-clipboard read-clipboard

# ── Keybindings ──────────────────────────────────────────────

# Prefix: C-z (like tmux)
# Sessions
map ctrl+z>s save_as_session --use-foreground-process --base-dir ~/.config/kitty/sessions
map ctrl+z>/ goto_session ~/.config/kitty/sessions
map ctrl+z>- goto_session -1
map ctrl+z>c close_session

# Windows (splits)
map ctrl+shift+enter new_window_with_cwd
map ctrl+shift+h neighboring_window left
map ctrl+shift+l neighboring_window right
map ctrl+shift+k neighboring_window up
map ctrl+shift+j neighboring_window down

# Tabs
map ctrl+shift+t new_tab_with_cwd

# Zoom (toggle stack layout)
map ctrl+shift+z toggle_layout stack

# Prompt navigation (shell integration)
map ctrl+shift+up   scroll_to_prompt -1
map ctrl+shift+down scroll_to_prompt 1

# Config reload
map ctrl+shift+f5 load_config_file
```

### Phase 3: Session files

Create `~/.config/kitty/sessions/`

#### default.kitty-session
```
layout tall
cd ~
launch --title "emacs" sh -c 'TERM=xterm-256color exec emacs -nw'
```

(Other project sessions follow this pattern — change `cd` path.)

### Phase 4: ~/bin/terminal-kitty (parallel to existing ~/bin/terminal)

```sh
#!/usr/bin/env bash
#
# terminal-kitty - open a named kitty session
#
# Usage:
#   terminal [SESSION]                   local kitty session (default: work)
#   terminal --remote HOST [SESSION]     tmux session on HOST via kitten ssh
#
# Examples:
#   terminal                             → terminal (work)
#   terminal dev                         → terminal (dev)
#   terminal --remote box                → terminal (work @ box)
#   terminal --remote box dev            → terminal (dev @ box)

remote=""
args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --remote)
      remote="$2"
      shift 2
      ;;
    *)
      args+=("$1")
      shift
      ;;
  esac
done

session="${args[0]:-work}"
session_dir="$HOME/.config/kitty/sessions"
session_file="$session_dir/${session}.kitty-session"

if [[ -n "$remote" ]]; then
  exec kitty --title "terminal ($session @ $remote)" \
    -e kitten ssh -t "$remote" -- tmux new -A -s "$session" "${args[@]:1}"
else
  # Fall back to default session if project-specific one doesn't exist
  if [[ ! -f "$session_file" ]]; then
    session_file="$session_dir/default.kitty-session"
  fi
  exec kitty --title "terminal ($session)" \
    --session "$session_file"
fi
```

### Phase 5: kitty-graphics.el in emacs

Add to `~/.emacs.d/init.el` (after elpaca-wait):

```elisp
(use-package kitty-graphics
  :ensure (:host github :repo "cashmeredev/kitty-graphics.el")
  :if (and (not (display-graphic-p)) (getenv "KITTY_PID"))
  :config
  (kitty-graphics-mode 1))
```

Ensure imagemagick is installed:
```sh
sudo apt install imagemagick
```

### Phase 6: Shell config (~/.zshrc additions)

```sh
# ── kitty integration ────────────────────────────────────────
if [[ -n "$KITTY_PID" ]]; then
  alias icat="kitten icat"
  alias s="kitten ssh"
fi

# ── Emacs terminal launch ───────────────────────────────────
# TERM=xterm-256color because emacs can't find xterm-kitty terminfo
e() {
  TERM=xterm-256color emacs -nw "$@"
}
```

### Phase 7: SSH config

Create `~/.config/kitty/ssh.conf`:

```
# Don't copy dotfiles — they're git-cloned on remotes
# Just ensure kitten binary is available for graphics/transfer
remote_kitty if-needed
```

---

## Verification Checklist

1. `kitty --version` >= 0.38.0
2. `kitten icat /path/to/test.png` renders inline
3. `terminal` opens kitty with full-screen emacs
4. `terminal --remote somehost` opens kitty → kitten ssh → tmux
5. Inside emacs in kitty: `C-x C-f image.png` renders visually (not binary)
6. `C-z s` prompts for session save path
7. `C-z /` shows session picker
8. Font renders acceptably (if not, uncomment DejaVu Sans Mono fallback)
9. Colors match alacritty (black bg, white fg, custom palette)
10. `tmux` still works standalone (unchanged)

## Font: Fixed6x13 in Kitty (The Problem)

Kitty categorically does not support bitmap fonts (confirmed by Kovid Goyal:
"it needs fonts to be freely resizable, so it does not support bitmapped fonts").

Fixed6x13 is a PCF bitmap font (`scalable: False`, `pixelsize: 13`).
Kitty renders it through FreeType which anti-aliases/scales it → fuzzy.
Adjusting `font_size` helps with cell dimensions but not with fuzz.

Alacritty renders Fixed6x13 pixel-perfect at native 13px with no scaling.
Alacritty config has no explicit `font.size` — it uses default 11.25pt but
renders the bitmap at native pixel size regardless.

### Option A: Convert Fixed6x13 to TTF via bitmap2ttf (best case)

[bitmap2ttf](https://github.com/ali1234/bitmap2ttf) traces PCF bitmap fonts
into TTF outlines. It would produce a scalable version of our *exact* glyphs.

```sh
pip install bitmap2ttf   # or clone from github
pcftottf /usr/share/fonts/X11/misc/6x13-ISO8859-1.pcf.gz \
  ~/.local/share/fonts/Fixed6x13.ttf
fc-cache -fv ~/.local/share/fonts/
```

Then set `font_family Fixed6x13` in kitty.conf (now picks up the TTF).
Find the right `font_size` that maps to exactly 13px at 98 DPI (~9.5pt).
Formula: `pointsize = pixelsize * 72 / DPI` → `13 * 72 / 98 ≈ 9.55`

Anti-aliasing may still soften edges slightly, but at native pixel size
the outlines should align to the pixel grid and be close to pixel-perfect.

### Option B: Cozette (same 6x13 bounding box, different glyphs)

[Cozette](https://github.com/the-moonwitch/Cozette) — 6x13px bitmap font,
3.5k stars, actively maintained (v2.0.0, Mar 7 2026). Based on Dina/Proggy.

Ships as:
- `.otb` — OpenType bitmap (preferred on Linux, pixel-perfect at 13pt)
- `CozetteVector.ttf` — auto-vectorized (kitty needs this; author warns
  "doesn't look right at any size" due to anti-aliasing)
- HiDpi variant — 2x upscaled bitmap, downscales well to ~10pt

For kitty specifically, Cozette's README documents:
- A [patched kitty AUR package](https://aur.archlinux.org/packages/kitty-bitmap/)
  that enables bitmap font support
- A fontconfig trick: set `spacing=100` and `scalable=true` to make kitty
  accept the OTB bitmap
- Or just use CozetteVector.ttf and accept the softness

At 100 DPI (ours is 98), CozetteVector looks right at 9.4pt.

### Option C: ProggyClean TTF

Already exists as TTF. Similar lineage to Fixed (Proggy → Dina → Cozette).
Familiar feel. Might be too small.

### Option D: Patched kitty with bitmap support

A community patch exists (from Cozette's issue tracker) that flips
`allow_bitmapped_fonts` in kitty. Nuclear option — custom kitty build.
Not recommended unless everything else fails.

### Option E: Terminus (TTF) — already installed

Traced from Terminus bitmap (different proportions than Fixed6x13 — wider).
Already installed at `~/.local/share/fonts/TerminusTTF-*.ttf`.
Available sizes: 12, 14, 16, 18, 20, 22, 24px.
User found it unappealing — different glyph design, not just rendering.

### Current state (2026-03-13)

Option A completed: Fixed6x13 converted to TTF via bitmap2ttf.
- Font installed at `~/.local/share/fonts/Fixed6x13.ttf`
- kitty.conf uses `font_family Fixed6x13.ttf`, `font_size 9.5`
- fontconfig rule disables hinting/antialiasing for this font:
  `~/.config/fontconfig/fonts.conf`
- `text_composition_strategy legacy` added to reduce boldness
- Result: noticeably better than raw bitmap, close to alacritty but
  still slightly softer (kitty lacks subpixel rendering)

### Alacritty bold behavior

Alacritty has `draw_bold_text_with_bright_colors = true`, which uses
bright colors (8-15) for bold instead of a bold font face. Kitty uses the
actual bold font variant — this is a deliberate design choice by Kovid
that many users find makes text look too heavy.

## Alternative terminals (from Reddit thread + research)

If kitty's rendering remains unsatisfying, these are worth evaluating:

### Foot — "Perfection" (per Reddit OP)
- Wayland-only (we're on X11 — dealbreaker?)
- Best font rendering according to multiple users
- Sixel support (works through tmux now!)
- Text config file, minimal, fast, low RAM
- No kitty graphics protocol — uses sixel instead
- No built-in multiplexer

### WezTerm
- Supports kitty graphics protocol (Linux/Mac)
- Built-in multiplexer (true session management, not just startup templates)
- Lua configurable
- Cross-platform (Linux/Mac/Windows)
- Slightly higher latency than kitty (not noticeable in practice)
- Better font rendering than kitty (uses platform-native rendering)
- Has its own image protocol on Windows (iterm2)

### tmux + sixel
- tmux now supports sixel! (arewesixelyet.com)
- This means: alacritty + tmux + sixel could give us inline images
  WITHOUT leaving our current terminal stack
- Would need: sixel-capable terminal (alacritty? foot? wezterm?)
  + tmux with sixel passthrough + emacs sixel support
- Status: alacritty does NOT support sixel (as of 2026)
- foot supports sixel, wezterm supports sixel

### Summary of graphics protocol support

| Terminal  | Kitty Graphics | Sixel | Font Rendering | Multiplexer |
|-----------|---------------|-------|----------------|-------------|
| kitty     | native        | no    | controversial   | sessions    |
| wezterm   | yes           | yes   | good           | built-in    |
| foot      | no            | yes   | best           | no          |
| alacritty | no            | no    | good (bitmap!) | no          |

### Decision: stick with kitty for now

The bitmap2ttf conversion + fontconfig hack + text_composition_strategy
gets us 90% there. kitty-graphics.el is the killer feature (inline images
in emacs -nw). If the remaining 10% font fuzz becomes intolerable,
wezterm is the most promising alternative (kitty graphics + better rendering
+ built-in multiplexer).

## Risks / Open Issues

- **Font rendering**: still slightly softer than alacritty. Kitty fundamentally
  lacks subpixel rendering. The fontconfig hack helps but doesn't fully close
  the gap.
- **kitty-graphics.el**: young project (46 stars, 1 contributor). May have rough edges.
- **No session persistence locally**: if kitty crashes, layout is gone. Emacs
  auto-save protects file content. Could add `save_as_session` to a periodic
  habit if this becomes painful.
- **C-z in bare shell**: loses job suspend. Acceptable since emacs handles
  process management via eat/vterm.
- **wezterm as fallback**: if kitty font rendering remains annoying, wezterm
  supports kitty graphics protocol AND has better font rendering. Could be
  a straight swap with minimal config changes.
