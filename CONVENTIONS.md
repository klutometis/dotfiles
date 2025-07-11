# Conventions

## Build Documentation and Scripts

For build instructions and automation:

- **Documentation**: Place detailed build instructions in `var/build/NAME.md`
- **Scripts**: Place the corresponding automated build script in `bin/build/NAME.sh`

For example:
- `var/build/emacs.md` - detailed instructions for building Emacs
- `bin/build/emacs.sh` - automated script that implements those instructions

The script should be a distillation of the documentation in executable form.

## Git Commit Messages

A commit message is a public record of a change that should clearly communicate:
1.  **What** change is being made.
2.  **Why** the change is being made.

Future developers (including your future self) will rely on this information to understand the history of the project. A well-written message provides context that the code alone cannot.

Use the following format for commit messages:

```
One-line summary of the change

- A brief description of the problem being solved.
- Detailed points about what was changed and why this is the best approach.
- Any relevant context, such as links to design documents or issue numbers.
- Mention any shortcomings or breaking changes.
```

### Guidelines

- **Summary Line:**
  - Keep the summary line under 50 characters.
  - Summarize *specifically* what the commit does.
  - Use the imperative mood ("Add feature," not "Added feature").
  - Capitalize the first letter.
  - Do not end the summary line with a period.

- **Body:**
  - Leave a blank line between the summary and the body.
  - Explain the "what" and the "why." Provide context, even for small commits.
  - Use bullet points for detailed explanations. Avoid vague messages like "Fix bug."

- **Before Committing:**
  - Review your commit message to ensure it accurately reflects the final changes.

### Google3 / Critique Specifics

For changes in the Google3 monorepo using Critique, add the following fields to the end of your CL description:

```
R=[list of reviewers]
BUG=[bug ID]
MARKDOWN=true
```
