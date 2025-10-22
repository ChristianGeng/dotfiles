---
description: Summarize current branch changes vs a target branch (default: main) as a short bulleted list with emojis
auto_execution_mode: 3
---

# diff-summary

- [ ] Determine TARGET_BRANCH to diff against:
  - [ ] If env TARGET_BRANCH is set, use it; otherwise default to "main".
  - [ ] If that branch doesn't exist remotely, fall back to the repo default:
        `git remote show origin | sed -n '/HEAD branch/s/.*: //p'`
- [ ] Fetch latest refs: `git fetch origin --prune --quiet`
- [ ] Compute the diff since the branch point with the target branch:
  - [ ] File-level summary:
        `git diff --name-status --find-renames --find-copies --diff-filter=ACDMRTUXB origin/${TARGET_BRANCH}...HEAD`
  - [ ] Patch (minimal context for faster scan):
        `git diff -U0 origin/${TARGET_BRANCH}...HEAD`
  - [ ] Short stats:
        `git diff --shortstat origin/${TARGET_BRANCH}...HEAD`
- [ ] Analyze the diff and output a concise summary (5–8 bullets max) with emojis, grouping by area when helpful. Examples:
  - [ ] ✨ New features/modules: <dirs/files>
  - [ ] 🐛 Bug fixes: <files/areas>
  - [ ] 🧹 Refactors/cleanup: <dirs/files>
  - [ ] 🧪 Tests added/updated: <test paths>
  - [ ] 📦 Dependency changes: <package files>
  - [ ] 📝 Docs/typos: <docs>
- [ ] Include counts (files changed, insertions, deletions) inline when relevant.
- [ ] Output only the bullet list (no raw diff or commands).
