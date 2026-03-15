# Plan: Clipboard image sync over SSH (screenshots in remote OpenCode)

## Problem

When running OpenCode inside tmux over SSH to a remote dev machine, pasting
images doesn't work. Locally in Emacs, image paste works because Emacs has
direct access to the system clipboard. Over SSH, no terminal protocol (OSC 52
or otherwise) carries image/png clipboard data -- this is true for Alacritty,
Kitty, and WezTerm alike.

Current workaround is `scp`, which is painful (security key touch every time,
manual path management, multiple steps).

## Solution

Four pieces, all on the local machine except one tmux keybind on the remote:

1. **SSH ControlMaster** -- multiplexes all SSH connections to the remote host
   over a single authenticated connection. One security key touch per boot;
   all subsequent ssh/rsync/scp calls piggyback with zero auth.

2. **`~/bin/screenshot` modification** -- the existing script (triggered by
   Super-Z via sxhkd) already captures screenshots to a temp file and copies
   to clipboard via xclip. Add ~4 lines to also save a timestamped copy to
   `/tmp/clip/` with a `latest.png` symlink.

3. **`~/bin/clipsync`** -- a wrapper script (~35 lines) that:
   - Takes `<host> [local_path] [remote_path]` (defaults: `/tmp/clip/` for both)
   - Checks for existing ControlMaster socket; opens one if needed (`ssh -fNM`)
   - Ensures directories exist on both sides
   - Generates a temp lsyncd config
   - Runs `lsyncd -nodaemon` in the foreground (Ctrl+C to stop)
   - lsyncd uses inotify (not polling) -- zero CPU/network when idle,
     rsyncs within ~1s of a file change

4. **tmux keybind** (remote, optional) -- `prefix + P` to paste
   `/tmp/clip/latest.png` into the pane. SKIPPED FOR NOW -- may already
   have a prefix+P binding. Can just type the path or use @/tmp/clip/latest.png.

## Topology

```
LOCAL:  local workstation
        - X11, sxhkd running
        - Alacritty + tmux (local)
        - ~/bin/screenshot (Super-Z via sxhkd)
        - lsyncd watches /tmp/clip/ via inotify
        ↓
        ssh (ControlMaster multiplexed)
        ↓
REMOTE: remote dev machine
        - tmux + OpenCode
        - rsync available, /tmp writable
        - Images appear at /tmp/clip/latest.png
```

## Daily workflow

```
Terminal 1:  ssh REMOTE_HOST                <- touch security key once
Terminal 2:  clipsync REMOTE_HOST
             [lsyncd watching /tmp/clip/, blocking in foreground]

Working:     Super-Z -> screenshot -> /tmp/clip/latest.png
             -> lsyncd detects inotify event
             -> rsync over ControlMaster (no touch) -> remote:/tmp/clip/latest.png (~1s)
             -> reference /tmp/clip/latest.png in OpenCode

Done:        Ctrl+C in Terminal 2
```

## File layout

```
/tmp/clip/                         (both local and remote, ephemeral)
├── latest.png -> 2026-03-15T14-23-01.png   (symlink, always most recent)
├── 2026-03-15T14-23-01.png
├── 2026-03-15T14-20-45.png
└── ...
```

/tmp auto-cleans on reboot. No pruning logic needed.

## Files to create/modify

### 1. ~/.ssh/config (edit, add at top)

```
Host REMOTE_HOST
    ControlMaster auto
    ControlPath ~/.ssh/sockets/%r@%h-%p
    ControlPersist yes
```

Also: `mkdir -p ~/.ssh/sockets`

Notes:
- Scoped to just the target host (not Host *)
- ControlPersist yes = infinite (master dies on reboot or explicit kill)
- Works with ProxyCommand-based SSH setups (relays, jump hosts, etc.)
- ServerAliveInterval keepalives keep master alive over idle periods

### 2. ~/bin/screenshot (edit, add ~4 lines at end)

After existing `import | tee | xclip` pipeline, add:

```bash
mkdir -p /tmp/clip
ts=$(date +%Y-%m-%dT%H-%M-%S)
cp "$tmpfile" "/tmp/clip/${ts}.png"
ln -sf "${ts}.png" /tmp/clip/latest.png
```

Where $tmpfile is the mktemp file the script already creates.

Note: the existing script uses `tee "$(mktemp --suffix=.png)"` inline in a
pipeline, so we need to capture that path to a variable first. The script
needs a small refactor to make the temp file path available.

### 3. ~/bin/clipsync (new, ~35 lines)

```
Usage: clipsync <host> [local_path] [remote_path]

  host         Remote host (as in ~/.ssh/config)
  local_path   Local directory to sync (default: /tmp/clip)
  remote_path  Remote directory to sync to (default: same as local_path)

Examples:
  clipsync myhost
  clipsync myhost /tmp/clip
  clipsync myhost /tmp/clip /tmp/clip
  clipsync myhost .              # sync pwd
  clipsync myhost . /home/x/y   # different remote path
```

### 4. Install lsyncd

```
sudo apt install lsyncd
```

lsyncd is only needed on local machine. Remote side needs nothing (just
rsync + ssh, both already present).

## SSH ControlMaster details

- ControlMaster auto: first connection becomes master, subsequent reuse it
- ControlPath ~/.ssh/sockets/%r@%h-%p: socket file per user@host-port
- ControlPersist yes: master stays alive until reboot or explicit kill
- Master stays alive as long as ANY session uses the socket (interactive SSH
  or lsyncd rsync calls)
- lsyncd only fires rsync on inotify events (no polling), so during idle
  periods there's zero network traffic -- but the master stays alive via
  ServerAliveInterval keepalives
- Kill manually if needed: ssh -O exit REMOTE_HOST

## Security notes

- ControlMaster is client-side only; server sees one normal SSH connection
  with multiple channels (standard SSH2 behavior)
- No new ports opened on remote side
- No daemons on remote side
- rsync over SSH uses the existing encrypted tunnel

## Future extensions

- **Non-screenshot images** (browser copies, etc.): Add a clipboard poller
  (~30 lines bash, polls xclip every ~1s, writes to /tmp/clip/ on change).
  Can run as a systemd user service. The rest of the pipeline (lsyncd, rsync,
  ControlMaster) stays the same.

- **Multiple remote hosts**: Run multiple clipsync instances in separate
  terminals, each targeting a different host. ControlMaster is per-host.

- **tmux keybind**: If desired later, add to remote .tmux.conf:
  `bind P send-keys '/tmp/clip/latest.png'`

## Status

Implemented and tested 2026-03-15. End-to-end pipeline works:
screenshot -> /tmp/clip/latest.png -> lsyncd inotify -> rsync over
ControlMaster -> remote:/tmp/clip/latest.png (~3s total latency).
SSH ControlMaster config moved to ~/etc/secrets (private repo, via stow).
