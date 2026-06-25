# ~/prg table-stakes repos: manifest, not submodules

**Decision (2026-06-25):** the `~/prg` repos that the configured system
depends on at runtime are managed by a declarative manifest
(`~/etc/prg-repos.txt`) synced from `configure-system.sh`, **not** as git
submodules of the home dotfiles repo.

## Why not submodules

The home repo already uses submodules — correctly — for **pinned,
low-churn, third-party or data** deps: `lib/{wait-for-it,shflags,bashlog}`,
`.oh-my-zsh`, `build-with-ai/pre-commit`, `etc/secrets`,
`dot-password-store`. That's what submodules are for.

The table-stakes `~/prg` repos are the opposite species: **repos I actively
develop and push to daily** (mcp-gateway, voice-mcp, wss-bridge, the
`*-mcp` servers). Submodules fight that:

- **Double-commit dance.** Every commit in a submodule needs a second
  commit in the superproject to bump the pinned SHA. Forget once → broken
  clone on the next machine.
- **Wrong intent.** Submodules *pin* a dependency to a SHA. For my own
  tooling on my own machines I want it to **float on main**, not freeze.
- **Clobber hazard.** Bootstrap runs `git submodule update --init
  --recursive`; with WIP in those repos that can detach HEAD / discard work.
- **Invisible drift.** The home repo runs `status.showUntrackedFiles no`,
  so "modified submodule" pointer noise is exactly what I don't see.
- **Doesn't scale.** ~70 repos live in `~/prg`; only a handful are
  table stakes. Submoduling them all is untenable; the manifest makes the
  required-vs-experiment boundary explicit.

The proof was already in the tree: 4 early `prg/*` submodules, 60+ later
repos cloned independently. I'd voted with my feet; this just formalizes it.

## How it works

- **Manifest:** `~/etc/prg-repos.txt` — `<dir> <remote> <role>` lines.
  Roles: `core` (every machine), `desktop` (only when `HEADLESS=0`),
  `mac` (skipped by the Linux script; the Mac's own bootstrap handles it).
- **Sync:** a block in `configure-system.sh` clones-if-missing and
  `git pull --ff-only` when the tree is clean. **Never touches a dirty
  working tree; never clobbers WIP.** Offline / diverged → skip, not fail.
- It's **config, not notes** — hence it lives in `~/etc/` (alongside
  `dictd.conf`, `hhkb/*.toml`, `udev` rules), not here. This file is the
  *rationale*; the manifest is the *data*.

## Migration done (2026-06-25)

- `chrome-cdp-mcp` had **no git remote** — a runtime dep (chrome launchers)
  that lived on one laptop only. Pushed to private
  `github.com/klutometis/chrome-cdp-mcp`, added to the manifest.
- `prg/q` and `prg/clipboard-mcp` converted from submodules → standalone
  clones in the manifest (`git submodule deinit` + `git rm` + re-clone).
  Left as submodules: `laptop-stickers` (+ the lib/ third-party ones).

## Future

- If a second consumer ever needs the list, it's already a standalone file.
- `laptop-stickers` could also move to the manifest for full consistency;
  left as-is for now (rarely changes).
