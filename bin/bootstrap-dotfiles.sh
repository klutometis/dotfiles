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

# 6. Create symlinks. We use a tiny in-script `stow_lite` instead of GNU
# stow because stow 2.4.1's `--dotfiles` mode has a tree-unfolding bug
# that makes it impossible to merge two packages with overlapping
# subdirectories (e.g., dotfiles and secrets both providing dot-config/).
# The error looks like: `stow_contents() called with non-directory path:
# etc/dotfiles/.config`. Filed upstream; until fixed, this is the
# workaround that produces the same end state as stow would.
#
# stow_lite walks each package under ~/etc/$pkg, translates `dot-X` to
# `.X`, and creates per-entry symlinks in $HOME. When packages overlap
# at a directory level, it descends and creates per-file symlinks (no
# folding). Idempotent.

stow_lite() {
    local pkg=$1
    local src="$HOME/etc/$pkg"
    [ -d "$src" ] || { echo "  no $src; skipping"; return; }

    local count=0
    shopt -s nullglob dotglob
    _stow_lite_walk "$src" "$HOME" count
    shopt -u nullglob dotglob
    echo "  $pkg: $count link(s) created/updated"
}

_stow_lite_walk() {
    local src=$1 dst=$2 count_var=$3
    local entry name target real_src
    for entry in "$src"/*; do
        name=$(basename "$entry")
        # `.` and `..` are filtered by glob; just skip the .git tree
        case "$name" in .git|.gitmodules|.gitattributes|.gitignore) continue ;; esac
        # Translate dot-X (only at start of name) to .X
        target="$dst/${name/#dot-/.}"
        real_src=$(readlink -f "$entry")

        if [ -L "$target" ]; then
            if [ "$(readlink -f "$target")" = "$real_src" ]; then
                continue  # already correct
            fi
            if [ -d "$entry" ]; then
                # Need to merge: unfold target symlink into a real dir,
                # then descend so our entries get added alongside the
                # original package's contents.
                local prev_target=$(readlink -f "$target")
                rm "$target"; mkdir "$target"
                local sub subname
                for sub in "$prev_target"/*; do
                    subname=$(basename "$sub")
                    [ ! -e "$target/$subname" ] && ln -s "$sub" "$target/$subname" \
                        && eval "$count_var=\$((${!count_var}+1))"
                done
                _stow_lite_walk "$entry" "$target" "$count_var"
                continue
            fi
            # Plain file or non-mergeable conflict: this package wins
            rm "$target"
        elif [ -d "$target" ] && [ -d "$entry" ]; then
            _stow_lite_walk "$entry" "$target" "$count_var"
            continue
        elif [ -e "$target" ]; then
            echo "  skip $target (exists, not symlink)"
            continue
        fi
        ln -s "$real_src" "$target" && eval "$count_var=\$((${!count_var}+1))"
    done
}

echo "Creating dotfile symlinks (stow_lite)..."
stow_lite dotfiles
if [ "$SECRETS_LOCKED" = 0 ]; then
    stow_lite secrets
else
    echo "Skipping secrets (still encrypted)"
fi

# 7. Run system configuration
echo "Running system configuration..."
~/bin/configure-system.sh

echo "=== Bootstrap Complete ==="
echo "Please restart your session for all changes to take effect"
