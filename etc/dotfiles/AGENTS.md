# Conventions

## Scratch code

Multi-line programs belong in the `node` or `python` tool, not a bash heredoc:
source goes in on stdin, the real exit code comes back, and nothing is left on
disk. Piping through `2>&1 | tail` reports tail's exit status, so a crash reads
as success.

## Directories

Tool paths resolve against the session directory, so `cd` is rarely needed. A
`cd` in bash lasts exactly one call; to move the session itself, use `cwd`.

## Worktrees

Worth it for a change spanning many files, or one you may want to abandon.
`harness new <topic> --worktree` makes one, installs its dependencies and puts
the new session in it. By hand it is `git worktree add` and an install — then
`cwd` into it, because driving a worktree by absolute path from the main
checkout defeats the point.

Install dependencies; don't copy or symlink `node_modules` from the main
checkout. With npm workspaces the `@scope/*` entries are relative symlinks
that resolve back to the main checkout's source, so the worktree silently runs
the wrong code — and the symlink reads as untracked, which blocks reaping.

Reaping is manual and nothing runs on a timer, so they pile up. `harness
worktrees` classifies them; `harness worktrees reap <path>` removes an orphan;
`harness rm <session> --reap` retires a session and its worktree together.
