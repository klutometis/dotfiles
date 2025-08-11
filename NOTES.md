## 2025-08-11 - Downloads Directory Reorganization  

### Decision: Move Downloads to ~/var/web with symlink compatibility

**Context**: The default ~/Downloads directory violates FHS principles and feels out of place in a well-organized home directory structure.

**Problem with ~/Downloads:**
- Capitalized and verbose (doesn't match bin/, etc/, var/ structure)
- Not FHS-compliant 
- Takes up prime real estate in home directory

**Alternatives considered:**
- `~/var/spool` - Technically correct (data awaiting processing) but semantically awkward
- `~/var/dl` - Obvious abbreviation but not descriptive  
- `~/var/in` - Clean "inbox" concept
- `~/var/recv` - For "received" files

**Why we chose ~/var/web:**
- Only 9 characters (vs 12 for ~/Downloads)
- Semantically accurate - files grabbed from the web
- Fits perfectly with existing FHS structure (bin/, etc/, var/, doc/, prg/)
- Clear indication of file source

**Implementation**: 
- Added task to `configure-system.sh` to create `~/var/web` and symlink `~/Downloads -> ~/var/web`
- Maintains compatibility with applications that hardcode ~/Downloads
- Clean organizational structure while preserving functionality

## 2025-08-07 - Environment Variables and Secrets Management

### Decision: Use git-crypt + set -a instead of pass show

**Context**: Considered using `pass show` directly in `.env-secrets` instead of storing encrypted values.

**Why we didn't use `pass show`:**
- GPG unlock timing: We unlock GPG lazily (first browser use), but `.zshenv` loads early
- Shell startup would hang waiting for GPG unlock
- Background processes/scripts can't prompt for GPG passphrase  
- Performance: Every new shell would call `pass` multiple times

**Why we chose set -a approach:**
- Cleaner than repetitive `export` statements
- Fast shell startup (no external calls)
- Works in all contexts (cron, scripts, etc.)
- git-crypt provides adequate security for this use case

**Implementation**: Used `set -a` / `set +a` in `.env-secrets` to automatically export all variable assignments without repetitive `export` keywords.
