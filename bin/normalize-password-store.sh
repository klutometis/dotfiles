#!/bin/bash
set -euo pipefail

# normalize-password-store.sh
# Normalizes password store structure:
# 1. Top-level .gpg files → domain/login
# 2. */api-key → */api  
# 3. Remove "username: " prefix on line 2

DRY_RUN=false
PASSWORD_STORE_DIR="${PASSWORD_STORE_DIR:-$HOME/.password-store}"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run|-n)
            DRY_RUN=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--dry-run|-n]"
            exit 1
            ;;
    esac
done

if $DRY_RUN; then
    echo "=== DRY RUN MODE - No changes will be made ==="
    echo
fi

# Function to execute or print command
run_cmd() {
    if $DRY_RUN; then
        echo "[DRY RUN] $*"
    else
        "$@"
    fi
}

cd "$PASSWORD_STORE_DIR"

# Step 1: Rename top-level .gpg files to domain/login
echo "=== Step 1: Normalizing top-level entries to domain/login ==="
find . -maxdepth 1 -name "*.gpg" | while read -r file; do
    # Remove leading ./ and trailing .gpg
    entry="${file#./}"
    entry="${entry%.gpg}"
    
    new_entry="${entry}/login"
    
    echo "  $entry → $new_entry"
    run_cmd pass mv "$entry" "$new_entry"
done

echo

# Step 2: Rename api-key to api
echo "=== Step 2: Renaming api-key to api ==="
find . -name "api-key.gpg" -o -name "api-key" -type d | while read -r file; do
    # Get the entry path without .password-store prefix and .gpg suffix
    entry="${file#./}"
    entry="${entry%.gpg}"
    
    # Replace api-key with api
    new_entry="${entry%-key}"
    
    echo "  $entry → $new_entry"
    run_cmd pass mv "$entry" "$new_entry"
done

echo

# Step 3: Fix username: prefix on line 2
echo "=== Step 3: Normalizing username format ==="
find . -name "*.gpg" | while read -r file; do
    # Get the entry path
    entry="${file#./}"
    entry="${entry%.gpg}"
    
    # Read the current content
    if ! content=$(pass show "$entry" 2>/dev/null); then
        continue
    fi
    
    # Check if line 2 starts with "username: "
    line2=$(echo "$content" | sed -n '2p')
    
    if [[ "$line2" =~ ^username:\ (.+)$ ]]; then
        username="${BASH_REMATCH[1]}"
        
        # Reconstruct content with modified line 2
        line1=$(echo "$content" | sed -n '1p')
        rest=$(echo "$content" | tail -n +3)
        
        new_content="$line1"$'\n'"$username"
        if [[ -n "$rest" ]]; then
            new_content="$new_content"$'\n'"$rest"
        fi
        
        echo "  $entry: 'username: $username' → '$username'"
        
        if $DRY_RUN; then
            echo "    [BEFORE]"
            echo "$content" | sed 's/^/      /'
            echo "    [AFTER]"
            echo "$new_content" | sed 's/^/      /'
        else
            echo "$new_content" | pass insert -m -f "$entry" >/dev/null
        fi
    fi
done

echo
if $DRY_RUN; then
    echo "=== DRY RUN COMPLETE - Run without --dry-run to apply changes ==="
else
    echo "=== NORMALIZATION COMPLETE ==="
fi
