#!/usr/bin/env bash
# Bootstrap dotfiles when homedir is the repo but configs are in etc/dotfiles.
#
# Environment overrides:
#   GIT_CRYPT_KEY  Path to a git-crypt symmetric key file. If set, used to
#                  unlock etc/secrets without GPG. Otherwise GPG-based unlock
#                  is attempted; on failure, bootstrap continues with secrets
#                  still encrypted (warns and skips `stow secrets`).
#   HEADLESS       0 or 1; auto-detected when unset. Propagated to
#                  configure-system.sh, which uses it to skip X11/desktop
#                  packages and DNS reconfiguration.

set -e

is_headless() {
    [ ! -x /usr/bin/Xorg ] && [ -z "${DISPLAY:-}" ]
}
HEADLESS="${HEADLESS:-$(is_headless && echo 1 || echo 0)}"
export HEADLESS
SECRETS_LOCKED=0

cd ~

echo "=== Bootstrapping dotfiles in home directory ==="

# 1. Initialize git repo if needed
if [ ! -d .git ]; then
    echo "Initializing git repository..."
    git init
    git remote add origin git@github.com:klutometis/dotfiles.git
else
    echo "Git repository already initialized"
fi

# 2. Configure git to not show untracked files
git config status.showUntrackedFiles no

# 3. Pull the dotfiles. Use reset --hard so re-runs pick up new commits
# (git checkout -f main on its own doesn't fast-forward an existing local
# main branch to origin/main).
echo "Fetching dotfiles..."
git fetch origin main
if git rev-parse --verify main >/dev/null 2>&1; then
    git checkout main
    git reset --hard origin/main
else
    git checkout -f -b main origin/main
fi
git branch --set-upstream-to=origin/main main >/dev/null 2>&1 || true

# 4. Initialize submodules
echo "Initializing submodules..."
git submodule update --init --recursive

# 5. Unlock secrets submodule with git-crypt.
# Order: explicit key file > GPG-based unlock > warn-and-continue.
if ! command -v git-crypt &> /dev/null; then
    echo "Error: git-crypt not installed (sudo apt install git-crypt)"
    exit 1
fi

if [ -n "${GIT_CRYPT_KEY:-}" ]; then
    if [ -r "$GIT_CRYPT_KEY" ]; then
        echo "Unlocking secrets via key file: $GIT_CRYPT_KEY"
        (cd ~/etc/secrets && git-crypt unlock "$GIT_CRYPT_KEY")
    else
        echo "Error: GIT_CRYPT_KEY=$GIT_CRYPT_KEY is not readable"
        exit 1
    fi
elif (cd ~/etc/secrets && git-crypt unlock 2>/dev/null); then
    echo "Secrets unlocked via GPG"
else
    echo "Warning: secrets remain locked (no GIT_CRYPT_KEY, GPG unlock unavailable)"
    echo "  To unlock later: GIT_CRYPT_KEY=/path/to/keyfile $0"
    SECRETS_LOCKED=1
fi

# 6. Create symlinks via GNU stow.
#
# Settings come from ~/.stowrc (--dir=etc --dotfiles --target=~/), so we
# just name the packages. --restow makes re-runs idempotent. --adopt is
# important: when a real file pre-exists at the target (e.g., a fresh
# bootstrap where ~/.config/foo was created before stow ran), --adopt
# moves it into the repo and replaces it with a symlink, rather than
# refusing with a conflict.
#
# Caveat: --adopt promotes live content into the repo. If you have
# uncommitted changes in the repo, diff before running.

echo "Creating dotfile symlinks (stow)..."
stow -v --restow --adopt dotfiles
if [ "$SECRETS_LOCKED" = 0 ]; then
    stow -v --restow --adopt secrets
else
    echo "Skipping secrets (still encrypted)"
fi

# 7. Run system configuration
echo "Running system configuration..."
~/bin/configure-system.sh

echo "=== Bootstrap Complete ==="
echo "Please restart your session for all changes to take effect"
