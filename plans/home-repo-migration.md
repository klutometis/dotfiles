# Home Repo Migration: `~/` → `~/etc/home/`

## Background

The repo at `github.com:klutometis/dotfiles` is currently materialized **as** `~/` itself: `bin/`, `notes/`, `plans/`, `var/`, `prg/`, `lib/`, `etc/`, `NOTES.md`, `TODO.md`, `CHANGELOG.md`, `README.md` all live at repo root, and `~/.git/` IS the repo's `.git`. Only `etc/dotfiles/` and `etc/secrets/` go through stow; everything else is tracked-in-place because `~` is the worktree.

This works but has a persistent cost: any tool (AI agents, IDE plugins, shell prompts, `find`, etc.) that walks up the parent chain from anywhere under `~/` finds `~/.git/` and treats `~` as a repo. `status.showUntrackedFiles = no` masks the symptom for `git status` but not for everything else.

Goal: make `~/etc/home/` a normal git checkout, stow it to `~/`, and remove `~/.git/` entirely. After migration, `~` is not a git worktree at all.

## Prerequisites

- GitHub repo `klutometis/dotfiles` is **public** (private/public toggle wiped stars/watchers, so renaming is cheap).
- Backup understanding: `~/.git/` is the only thing being destroyed; all tracked files stay in place (just become symlinks).
- `git-crypt`, `stow`, ssh keys for `klutometis/secrets` already present (current bootstrap requires them).

## Steps

### 1. Rename GitHub repo

`klutometis/dotfiles` → `klutometis/home`. GitHub redirects the old name, so existing clones keep working until `.git/config` remotes are updated.

### 2. Work in a separate checkout

Don't touch `~/` yet. Clone fresh:

```bash
git clone git@github.com:klutometis/home.git /tmp/home-restructure
cd /tmp/home-restructure
git submodule update --init --recursive
```

Do all restructuring on a branch here, push, then migrate.

### 3. Flatten: move `etc/dotfiles/*` to repo root

```bash
cd /tmp/home-restructure
git mv etc/dotfiles/* etc/dotfiles/.[!.]* .
rmdir etc/dotfiles
```

(The `.[!.]*` glob catches dotfiles like `.config` if any exist directly under `etc/dotfiles/`; verify before running.)

Use `git mv` to preserve history.

### 4. Update `.stowrc`

```
--dir=.
--dotfiles
--target=~/
```

(Was `--dir=etc`.)

### 5. Update `.gitmodules`

- `etc/dotfiles/dot-password-store` → path becomes `dot-password-store`
- `etc/secrets` → **remove the entry entirely** (becomes a sibling checkout, not a submodule)
- `.oh-my-zsh`, `lib/wait-for-it`, `lib/shflags`, `lib/bashlog`, `prg/q`, `prg/laptop-stickers`, `prg/clipboard-mcp`, `prg/build-with-ai/pre-commit` — unchanged

After editing `.gitmodules`, also:

```bash
git rm --cached etc/secrets       # de-register submodule
git submodule sync                # propagate path changes for password-store
```

Verify `.git/config` and `.git/modules/` are consistent.

### 6. Optional FHS pass

What's currently at repo root and where it stands:

| Path | FHS verdict |
|---|---|
| `bin/`, `etc/`, `lib/`, `var/` | clean |
| `prg/` | legitimate exception (no FHS slot for project workspaces) |
| `notes/`, `plans/` | no FHS slot; candidates for `var/notes/`, `var/plans/` (growing user data) |
| `.oh-my-zsh` | forced by tool |
| `.xsession → .xinitrc` | forced by display managers |
| `NOTES.md`, `TODO.md`, `CHANGELOG.md`, `AGENTS.md`, `README.md` | repo-meta; belong at repo root, stow them to `~/` root |

Decision needed: move `notes/` → `var/notes/`, `plans/` → `var/plans/`? Low-stakes either way. If yes, `git mv` and update any references.

### 7. Rewrite `bin/bootstrap-dotfiles.sh`

Current script does `cd ~ && git init && git remote add ... && git checkout -f main` — i.e. materializes the repo AS `~`. Replace with:

```bash
#!/usr/bin/env bash
set -e

mkdir -p ~/etc

# Clone home repo
if [ ! -d ~/etc/home/.git ]; then
    git clone git@github.com:klutometis/home.git ~/etc/home
else
    (cd ~/etc/home && git pull)
fi

# Submodules (no longer includes secrets)
(cd ~/etc/home && git submodule update --init --recursive)

# Clone secrets as parallel checkout
if [ ! -d ~/etc/secrets/.git ]; then
    git clone git@github.com:klutometis/secrets.git ~/etc/secrets
else
    (cd ~/etc/secrets && git pull)
fi

# Unlock secrets
if command -v git-crypt &> /dev/null; then
    (cd ~/etc/secrets && git-crypt unlock)
else
    echo "Error: git-crypt not installed" >&2
    exit 1
fi

# Stow home (uses .stowrc: --dir=. --dotfiles --target=~/)
if command -v stow &> /dev/null; then
    (cd ~/etc/home && stow -v --restow --adopt .)
    (cd ~/etc/secrets && stow -v --restow --adopt .)  # if secrets has its own stow layout
else
    echo "Error: stow not installed" >&2
    exit 1
fi

# System config
~/bin/configure-system.sh
```

Drop the `git config status.showUntrackedFiles no` line — irrelevant in the new world.

Note: verify how `secrets` is laid out for stow. Currently bootstrapped as `stow -v --restow --adopt secrets` from `~/etc/`. If secrets repo has files directly at root meant to land in `~/`, the same pattern applies (`cd ~/etc/secrets && stow .`).

### 8. Commit, push, verify in scratch checkout

```bash
cd /tmp/home-restructure
git add -- .stowrc .gitmodules <flattened files>   # surgical, never -A
git commit -m "feat: flatten dotfiles, restructure as ~/etc/home stow source"
git push origin main
```

Do a smoke test by cloning the pushed repo into another scratch dir and running stow against a throwaway target dir to verify symlinks land where expected.

### 9. In-place migration of live `~/`

Now the sticky part. The new repo's contents are byte-identical to what's currently at `~/` (same git history), so `stow --adopt` should mostly just create symlinks without moving anything.

```bash
# Clone into final location
git clone git@github.com:klutometis/home.git ~/etc/home
(cd ~/etc/home && git submodule update --init --recursive)

# Secrets
git clone git@github.com:klutometis/secrets.git ~/etc/secrets   # if not already there
(cd ~/etc/secrets && git-crypt unlock)

# Stow with --adopt
cd ~/etc/home
stow -v --restow --adopt .
```

Failure modes to expect:

- **Pre-existing real directories** (e.g. `~/bin/`) blocking tree-folding: stow will unfold by symlinking individual files instead of the whole dir. Acceptable. If you want full tree-folding, you'd need to remove the real dir first, but `--adopt` won't do that for non-empty dirs.
- **Diverged files**: `--adopt` moves the live file into the package (`~/etc/home/`). After stow, `cd ~/etc/home && git status` will show those as modifications. Diff each one and decide: commit (the live version is the new truth) or `git checkout -- <path>` (the repo version wins, then re-stow).
- **`.oh-my-zsh` submodule conflict**: `~/.oh-my-zsh/` exists as a real submodule checkout. After stow, it should be a symlink to `~/etc/home/.oh-my-zsh/`. `--adopt` may complain. Likely needs manual `rm -rf ~/.oh-my-zsh && stow .` since both sides are git-managed and we trust the new one.

### 10. Decommission `~/.git/`

Only after step 9 verifies clean:

```bash
mv ~/.git /tmp/old-home-git.$(date +%s)
```

Don't `rm -rf` for at least a session. Verify nothing breaks. After a few days of normal use, delete the backup.

Also remove stale config:

```bash
# In ~/.gitconfig (now a symlink to ~/etc/home/dot-gitconfig), no change
# But check ~/.config/git/ and similar for any home-repo-specific hooks
```

### 11. Update remotes / docs

- Anywhere the URL `klutometis/dotfiles` is hardcoded (READMEs, docs, scripts), update to `klutometis/home`. The GitHub redirect covers fetches but not aesthetics or future automation.
- Update `~/etc/home/README.md` to describe the new architecture.

## What this fixes

- `~/.git/` no longer exists. `git status` from `~` returns "not a git repository". AI agents stop being flooded.
- `~/.gitignore` becomes irrelevant; gitignore concerns move into `~/etc/home/.gitignore` and only apply to repo contents.
- `git status` inside `~/etc/home/` is naturally scoped — no `showUntrackedFiles` workaround needed.
- `git clean -fd` accidents in `~` are no longer possible.

## What this costs

- One-time migration effort (mostly mechanical).
- `~/bin/`, `~/notes/`, etc. become symlinks. Tools that `[ -d ~/notes ]` still pass; tools that resolve symlinks may surprise occasionally.
- Loses no history — same repo, just relocated.

## Open questions for the implementer

1. Should `notes/` and `plans/` move to `var/notes/` and `var/plans/` as part of step 6, or stay at root?
2. Does `klutometis/secrets` still need to be a separate stow package, or should its contents be merged into `klutometis/home` (with git-crypt at the home level)? Probably keep separate — different access control story.
3. After migration, is `bootstrap-dotfiles.sh` itself something that ships in the repo (chicken-and-egg: you need to clone the repo to get the script that clones the repo)? Currently it's at `bin/bootstrap-dotfiles.sh` which means you `curl` it raw from GitHub before the clone. Document this in README.
4. SSH key bootstrap: `klutometis/secrets` is private (SSH). The bootstrap assumes keys are in place. If migrating to a fresh machine, document the manual SSH key step that precedes `bootstrap-dotfiles.sh`.

## Rollback

If anything goes sideways during step 9:

```bash
mv /tmp/old-home-git.* ~/.git    # if already moved
cd ~ && git checkout -f main      # restore tracked files
rm -rf ~/etc/home                 # remove the new checkout
```

Symlinks created by stow point into `~/etc/home/`; once that's gone they're dangling and harmless, but worth `find ~ -maxdepth 2 -xtype l -delete` to clean them up.
