# ai-code-interface.el Workflows

Reference and practice guide for `ai-code-interface.el` with opencode as backend.
Keybinding: `C-c A` opens the transient menu.

---

## Quick Reference Card

```
C-c A   → Open transient menu

── AI CLI Session ──────────────────────────────────────────
a   Start opencode session (C-u: prompt for CLI args)
R   Resume previous session (--continue)
z   Switch to/hide session buffer (C-u: force hide)
s   Select backend (shows current)
u   Install / upgrade opencode
g   Open opencode config (e.g. add MCP server)
G   Open AGENTS.md / repo agent file
|   Pipe current file through opencode with a prompt

── Code Actions ────────────────────────────────────────────
^   Toggle prompt suffix on/off
c   Code change — scoped to function or region (C-u: +clipboard)
i   Implement TODO comment at point (C-u: +clipboard)
q   Ask question — scoped to function or region (C-u: +clipboard)
x   Explain code in scope
SPC Send free-form command (C-u: +files+repo context)
@   Context: add/show/clear stored context entries
C   Create file or directory with AI

── Agile Development ───────────────────────────────────────
T   Set auto-test mode (test-after-change / tdd / ask-me / off)
r   Refactor (full Fowler catalog — 70+ techniques)
t   TDD cycle (Red / Green / Refactor / Red+Green)
v   Pull or review diff (generate diff, start review)
!   Run current file or AI-assisted shell command
b   Build / test with AI follow-up
K   Create or open task file
n   Capture notes from AI session region into org

── Other Tools ─────────────────────────────────────────────
.   Init projectile
e   Debug exception (C-u: +clipboard for error text)
f   Fix flycheck errors in scope
k   Copy current file name to clipboard (C-u: full path)
o   Open recently modified git file (C-u: insert path)
p   Open prompt history file (.ai.code.prompt.org)
m   Debug python MCP server
N   Toggle desktop notifications
```

---

## The Big Shift: From Copy-Paste to Integration

**Before**: opencode in tmux pane → copy code → paste into opencode → copy output → `C-x C-f` to open file → `C-x e` to eval.

**Now**: cursor in code buffer → `C-c A c` → type instruction → opencode gets function + file context automatically → edits land in your file → auto-revert shows them instantly.

The core insight is **context is assembled for you**. You don't copy-paste code into the AI; the AI gets:
- The current function (via `which-function`)
- The active region (if you selected something)
- Other visible file buffers
- Any stored context entries (via `@`)
- Clipboard (if you prefix with `C-u`)

---

## Context Engineering

Context quality is the single biggest lever on output quality.

### Automatic context

Most `C-c A` commands automatically include:
- **Current function**: if cursor is inside a function
- **Active region**: if you have a selection (takes priority over function)
- **Visible files**: all buffers visible in current windows

### Manual context curation: `C-c A @`

Use `@` to build a persistent, project-scoped context list:
- `C-c A @` → "Add" → stores current function/region/file
- `C-c A @` → "Show" → view what's stored
- `C-c A @` → "Clear" → wipe it

**Pattern**: open the files that matter in split windows, then issue your command. All visible buffers are included automatically.

### @ and # completion in buffers

With `ai-code-prompt-filepath-completion-mode` enabled (it is):
- Type `@` anywhere in a comment or in the opencode session buffer → completion list of project files appears → select to insert path
- Type `@path/to/file#` → completion list of symbols from that file appears

This is how you precisely direct the AI: `"Refactor @src/foo.py#MyClass to use the pattern in @src/bar.py#BaseClass"`.

### C-u prefix levels

| Prefix | What's added |
|---|---|
| (none) | function/region scope only |
| `C-u` | + visible files + stored repo context |
| `C-u C-u` | + clipboard on top of above |

---

## Spec-Driven Development (SDD)

SDD inverts the traditional relationship: specifications are the source of truth, code is the generated output. The workflow is: **specify → plan → tasks → implement → test**.

Reference: https://github.com/github/spec-kit/blob/main/spec-driven.md

### The SDD cycle with ai-code

#### Phase 1: Specify

Create a feature spec file first. Use `C-c A K` to create a task file, or manually create `specs/<feature-name>/spec.md`. The spec should answer:
- What problem does this solve? (user stories)
- What are the acceptance criteria? (testable, unambiguous)
- What are the non-functional requirements?
- What is explicitly out of scope?
- Mark unknowns as `[NEEDS CLARIFICATION: ...]` — don't guess

**Emacs workflow**:
```
C-c A K          → create/open task file for this feature
```
Then in the spec buffer, use yasnippet snippets to scaffold structure:
```
M-x yas-insert-snippet → "design" or "architecture-diagram" or "schema"
```

Send the spec to opencode for refinement:
```
Select spec buffer → C-c A SPC
→ "Review this spec for ambiguity, contradictions, and gaps.
   Mark unclear requirements with [NEEDS CLARIFICATION].
   Do not suggest implementation details."
```

#### Phase 2: Plan

With spec stable, generate an implementation plan. Open the spec in one window, a new `specs/<feature>/plan.md` in another:
```
C-c A @          → add spec.md to context
C-c A SPC        → "Generate an implementation plan from @specs/<feature>/spec.md.
                    Include: architecture decisions with rationale, data models,
                    API contracts, file creation order (tests first), and
                    parallelizable tasks marked [P]."
```

Constitutional gates to include in your prompt (from spec-kit Article VII/VIII):
- Maximum 3 projects for initial implementation
- Use framework features directly, no unnecessary wrapping
- Contract tests before implementation
- No speculative features

#### Phase 3: Tasks

Break the plan into executable tasks. Open `plan.md`, send to opencode:
```
C-c A |          → "From this plan, generate tasks.md with:
                    - Specific, atomic tasks
                    - Dependencies noted
                    - Independent tasks marked [P] for parallelization
                    - Test tasks before implementation tasks (TDD order)"
```

Use `C-c A K` to open/track `tasks.md` as your working checklist.

#### Phase 4: Implement (task by task)

For each task, the loop is:
1. Open the relevant source file + test file in split windows
2. `C-c A @` to add both to context if needed
3. `C-c A c` (code change) with the specific task description
4. Review the diff in the buffer (auto-revert shows it)
5. If tests are needed first (TDD): use `C-c A t` instead (see TDD section)

**Key**: keep tasks small. One function, one endpoint, one data model at a time. The AI's context window is finite; precision beats breadth.

#### Phase 5: Verify

```
C-c A b          → build/test with AI follow-up
                   (AI runs tests and fixes failures automatically)
```

Or manually:
```
C-c A t → "0. Run unit-tests"
```

#### SDD + prompt suffix

Set a persistent constraint on all prompts:
```elisp
(setq ai-code-prompt-suffix
      "Follow the spec at specs/<feature>/spec.md strictly.
       Do not implement features not in the spec.
       Tests before implementation.")
(setq ai-code-use-prompt-suffix t)  ; toggle: ^ in menu
```

Toggle with `^` in the transient menu.

---

## Test-Driven Development (TDD)

`C-c A t` opens the TDD cycle menu. Kent Beck's Red-Green-Refactor, AI-assisted.

### Prerequisites

- Have both source file and test file **visible in split windows** (Red/Green stages require a test buffer visible)
- The test file must have "test" in its name

### The stages

#### 0. Run tests
Detects language and runs appropriate runner. Falls back to AI-assisted if no runner found.

#### 1. Red — write failing test

Place cursor in the function you're about to implement (or select a region describing the behavior):
```
C-c A t → "1. Red (Write failing test)"
→ describe the feature: "validate email format, reject missing @, reject missing TLD"
```

ai-code sends a prompt that:
- Writes only the test (not the implementation)
- Follows the project's existing test patterns
- Creates the test file if it doesn't exist

**Then actually run the tests and confirm they fail.** Don't skip this.

#### 2. Green — make the test pass

With failing test confirmed:
```
C-c A t → "2. Green (Make test pass)"
→ "Implement the minimal code to make the failing test pass"
```

The prompt includes current file context. ai-code knows to implement *minimally* — no gold-plating.

If the test output is visible (e.g. in a compilation buffer), select it and use `C-u C-u` with the green stage to include the failure text as clipboard context.

#### 3. Refactor — improve without breaking

```
C-c A t → "3. Refactor (Improve code quality)"
→ opens the full Fowler refactoring catalog
→ pick a technique or "Suggest Refactoring Strategy"
```

The refactoring prompt automatically appends "Ensure all tests still pass after refactoring."

#### 4. Red + Green in one shot

For simpler cases where you trust the AI to do both:
```
C-c A t → "4. Red + Green (One prompt)"
→ describe the feature
```

Sends a combined prompt: write test, implement, run tests, fix if failing. Good for well-understood requirements; prefer Red then Green separately for anything subtle.

### Realistic TDD with integration tests

Per spec-kit Article IX: prefer real databases/services over mocks. When prompting for tests:
```
C-c A t → Red
→ "Write integration tests for the user signup endpoint.
   Use a real test database (not mocks). Tests should:
   - Insert a user and verify persistence
   - Reject duplicate emails with the correct HTTP status
   - Roll back on validation failure"
```

The `ai-code--tdd-test-pattern-instruction` constant automatically appends a reminder to follow the project's existing test file naming/structure patterns.

---

## Everyday Code Actions

### Code change: `C-c A c`

The workhorse. Position cursor in a function (or select a region):
```
C-c A c → "add input validation; raise ValueError if name is empty"
```

With clipboard (e.g. an error message you copied):
```
C-u C-c A c → "fix the bug described in the clipboard"
```

### Implement TODO: `C-c A i`

Write a TODO comment, cursor on it:
```python
# TODO: implement rate limiting — max 100 req/min per IP, sliding window
```
```
C-c A i  → reads the TODO text as context, prompts for any extra instruction
```

### Ask question: `C-c A q`

Cursor in a function:
```
C-c A q → "what's the time complexity of this? can it be made O(n)?"
```

Answer appears in the opencode session buffer. Use `C-c A z` to flip to it.

### Explain: `C-c A x`

No prompt needed — sends the current function/region with "explain this code" framing. Good for onboarding onto unfamiliar codebases.

### Investigate exception: `C-c A e`

Copy a stack trace, then:
```
C-u C-c A e → pastes clipboard (the trace) + current file context
             → "what's causing this and how do I fix it?"
```

### Fix diagnostics: `C-c A f`

With flymake errors in the current buffer:
```
C-c A f → sends all errors in scope to opencode with "fix these"
```

Note: this is labelled "flycheck" in the menu but works with flymake diagnostics too since opencode reads the file directly.

---

## Refactoring (`C-c A r`)

Opens a completing-read list of 70+ refactoring techniques from Fowler's *Refactoring* (2nd ed). Works on:
- The current function (cursor anywhere inside it)
- A selected region

**Tip**: select a region for techniques like Extract Method, Extract Variable. For class-level techniques (Pull Up, Extract Class), just have cursor in the class.

**"Suggest Refactoring Strategy"** is the first option — lets the AI analyze the code and recommend what to do rather than you prescribing it. Good starting point for unfamiliar code.

Parametric techniques (Extract Method, Rename, Move Method) prompt for names inline. Hit enter with the default to let the AI decide.

---

## PR Review Workflow

`C-c A v` generates a diff and starts a review session.

```
C-c A v → "Generate diff between current branch and main"
         → diff appears in a new buffer
         → "Start review? y"
         → opencode reviews the diff and comments
```

Can also review a patch file: open it, `C-c A v`.

Combine with Magit: `C-c g` to open magit, then the AI commands are available directly in Magit popups via the Magit integration we set up.

---

## Prompt File Mastery

`C-c A p` opens `.ai.code.files/.ai.code.prompt.org` — a project-local org file that is your conversation history and prompt scratchpad.

### Why it matters

Every prompt sent via `C-c A` is written here with a timestamp/headline (AI-generated via gptel when `ai-code-use-gptel-headline` is on). You can:
- Review what you asked and what context was sent
- Re-send a previous prompt block: position in a block, `C-c A b`
- Edit a prompt and re-send it (great for iterating on a spec)
- Use yasnippet snippets to insert structured prompts

### Yasnippet snippets

In the prompt file (or any buffer, for `ai-code-prompt-mode` snippets):
```
M-x yas-insert-snippet
```

Key snippets bundled with ai-code:
| Snippet key | Use case |
|---|---|
| `create-tests` | Scaffold test generation prompt |
| `unit-tests` | Unit test prompt template |
| `code-review` | Structured code review |
| `review-logical-errors` | Logic-focused review |
| `debug` | Debug investigation template |
| `refactor-code` | Refactoring prompt |
| `design` | Architecture/design discussion |
| `schema` | Data model generation |
| `architecture-diagram` | Mermaid diagram generation |
| `explain-code` | Explanation request |
| `improve-performance` | Performance analysis |
| `improve-error-handling` | Error handling improvements |
| `security` | Security review |
| `world-class-engineer` | "What would a senior engineer do here?" framing |
| `create-boilerplate` | Bootstrap new file/module |
| `write-code` | General code generation |
| `optimize` | Optimization request |
| `diff-change` | Review a specific diff |
| `docs` | Documentation generation |

Tab-expand them to get a structured prompt, fill in the blanks, then `C-c A b` to send the block.

---

## Multi-Session Patterns

ai-code supports multiple opencode sessions per project.

```
C-c A a   → start a second session (prompts for session name)
C-c A z   → select active session (completing-read across all open sessions)
```

**Pattern: spec session + implementation session**

Session 1 ("spec"): keep the spec and plan open, discuss requirements, iterate on the spec file.
Session 2 ("impl"): do the actual code changes task by task.

Prompts from `C-c A c`, `C-c A t`, etc. go to the currently selected session.

**Pattern: parallel feature branches**

One session per feature branch. Switch between them with `C-c A z`.

**With desktop notifications on** (`C-c A N` to toggle), you can kick off a long agent run, switch to another session or another task entirely, and get a D-Bus notification when the first one finishes.

---

## AI-Assisted Shell Commands

From any shell/dired/vterm buffer:
```
C-c A ! → type ": count lines of python code recursively"
         (the : prefix means "generate a shell command for this")
         → opencode generates the command
         → you review and approve
         → runs in compile buffer
```

Without `:` prefix, runs the current file directly.

---

## Magit Integration

With `ai-code-magit-setup-transients` called (it is), AI commands appear in Magit popups. From `C-c g` (magit-status):
- AI-assisted commit message generation
- Diff review via the diff popup

---

## Practice Drills (Week-by-Week)

### Week 1: Core mechanics

**Day 1–2: Sessions and basic actions**
- Start an opencode session: `C-c A a`
- Navigate to a function, send `C-c A x` (explain it to yourself)
- Make a trivial change with `C-c A c` ("add a docstring")
- Switch to the session buffer with `C-c A z`, read the output

**Day 3–4: Context awareness**
- Open two related files in split windows
- Use `C-c A q` from one file: notice both files appear in the prompt
- Add a function to context with `C-c A @`, then ask a question that spans both
- Type `@` in a comment and use file completion

**Day 5–7: TODO-driven development**
- Write 3 TODO comments in a file describing small tasks
- Use `C-c A i` on each one
- Compare to what you'd have written manually

### Week 2: TDD

**Day 1–3: Red-Green on a small function**
- Pick a pure function to write from scratch
- Open source file + test file in split windows
- `C-c A t` → Red: write the failing test, run it, confirm it fails
- `C-c A t` → Green: implement, run tests
- `C-c A t` → Refactor: pick "Suggest Refactoring Strategy"

**Day 4–5: Integration tests**
- Write an integration test (real DB, no mocks) using `C-c A t` → Red
- Pay attention to the test pattern instruction in the prompt

**Day 6–7: Red+Green combined**
- Use stage 4 (Red+Green) for a well-understood requirement
- Compare output quality to the two-stage approach

### Week 3: Spec-Driven Development

**Day 1–2: Write a spec**
- Pick a small feature (one endpoint, one class)
- `C-c A K` → create task file as `specs/<feature>/spec.md`
- Write the spec manually, then `C-c A SPC` → "review this spec for ambiguity"
- Iterate until no `[NEEDS CLARIFICATION]` markers remain

**Day 3–4: Plan and tasks**
- Generate `plan.md` from the spec
- Generate `tasks.md` from the plan
- Identify which tasks are parallelizable `[P]`

**Day 5–7: Execute task by task**
- Work through `tasks.md` using `C-c A c` and `C-c A t`
- Check off tasks as you go
- Use `C-c A b` to run the full test suite after each task

### Week 4: Advanced patterns

**Day 1–2: Refactoring catalog**
- Find a complex function in your codebase
- `C-c A r` → "Suggest Refactoring Strategy"
- Apply the suggested technique
- Try parametric techniques: Extract Method, Rename

**Day 3–4: Multi-session + notifications**
- Start two sessions for two different concerns
- Enable notifications: `C-c A N`
- Kick off a long agent task in session 1, switch to session 2 and work
- Receive notification when session 1 finishes

**Day 5–7: PR review**
- Before merging a branch: `C-c A v` → generate diff vs main
- Start the review
- Act on the AI's comments with `C-c A c`

---

## Tips and Gotchas

**Opencode theme**: if you see flickering in the vterm buffer, add to `~/.config/opencode/opencode.jsonc`:
```json
"theme": "system"
```

**Auto-revert**: already enabled globally. `auto-revert-interval` is set to 1s so AI edits appear promptly.

**Prompt file location**: `.ai.code.files/.ai.code.prompt.org` at the project root (created on first use). Add to `.gitignore` or commit it — both are valid strategies. Committing makes the prompt history part of the project record.

**Region vs function**: when you have a region selected, it takes priority over function scope. For a targeted refactor, select exactly what you want changed.

**C-u escalation**: start without `C-u`. If the AI asks for more context or produces something off, retry with `C-u` to add file/repo context, then `C-u C-u` to also add clipboard.

**The `|` command** (`C-c A |`): pipes the entire current file through opencode with a prompt. Useful for whole-file transformations, migrations, or large refactors where function-level scope isn't enough.

**Session buffer keybinding**: `C-c A z` is your most-used nav key once sessions are running. It toggles between your code and the AI session.
