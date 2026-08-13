#!/usr/bin/env bash
# restack-agents.sh — move a never-merged personal branch back onto the head of
# the stack it sits on.
#
# WHY THIS EXISTS
# ---------------
# ec2-instance-setup carries a stack:
#
#   main <- chore/drop-api-test-scaffold (!4) <- demo-instance-setup (!3) <- cgeng/agents
#
# cgeng/agents holds the AI assistant files (AGENTS.md, CLAUDE.md, the .github
# Copilot bridges, the superpowers note) that must never reach main. It is one
# commit on top of demo-instance-setup, and it does NOT follow that branch by
# itself: nothing in git or GitLab restacks a child when its parent moves. Every
# time demo-instance-setup is amended or rebased, this branch is left pointing at
# a commit that is no longer on it.
#
# WHY NOT JUST `git rebase origin/demo-instance-setup`
# ---------------------------------------------------
# Because the parent is REWRITTEN, not appended to. A plain rebase computes the
# merge base between this branch and the new parent, which after a rewrite sits
# far back in history, so it tries to replay every commit since then. The correct
# form is `--onto <new-head> <old-base>`, where <old-base> is the commit this
# branch was actually built on — derivable as the branch's own first parent.
#
# Usage:
#   ./restack-agents.sh                 # restack cgeng/agents onto demo-instance-setup
#   ./restack-agents.sh --dry-run       # report what would happen, touch nothing
#   BRANCH=cgeng/foo ONTO=origin/main ./restack-agents.sh
#   REPO=/path/to/repo ./restack-agents.sh
#
# Safe to re-run: when the branch is already at the head it reports so and exits
# without touching the remote. The push is SHA-leased, so a push that raced this
# one is rejected rather than silently overwritten.
set -uo pipefail

REPO="${REPO:-$HOME/work/research/research/iva/aws/ec2-instance-setup}"
BRANCH="${BRANCH:-cgeng/agents}"
ONTO="${ONTO:-origin/demo-instance-setup}"
DRY=0
[ "${1:-}" = "--dry-run" ] && DRY=1

say()  { echo "== restack-agents: $*"; }
warn() { echo "!! restack-agents: $*" >&2; }
die()  { warn "$*"; exit 1; }

[ -d "$REPO/.git" ] || die "not a git repo: $REPO"
cd "$REPO" || die "cannot cd to $REPO"

git fetch -q origin || die "git fetch failed"
git rev-parse -q --verify "$ONTO" >/dev/null || die "no such ref: $ONTO"
git rev-parse -q --verify "origin/$BRANCH" >/dev/null || die "no such ref: origin/$BRANCH"

HEAD_SHA="$(git rev-parse "$ONTO")"
OLD_TIP="$(git rev-parse "origin/$BRANCH")"
OLD_BASE="$(git rev-parse "origin/$BRANCH^")"   # what the branch was built on
N="$(git rev-list --count "$OLD_BASE..origin/$BRANCH")"

say "$BRANCH is ${OLD_TIP:0:8} ($N commit(s) on ${OLD_BASE:0:8})"
say "$ONTO is ${HEAD_SHA:0:8}"

if [ "$OLD_BASE" = "$HEAD_SHA" ]; then
  say "already at the head — nothing to do"
  exit 0
fi

[ "$DRY" = 1 ] && { say "--dry-run: would rebase --onto ${HEAD_SHA:0:8} ${OLD_BASE:0:8}"; exit 0; }

# Rebase in a throwaway worktree: never disturb whatever is checked out here.
WT="$(mktemp -d)" || die "mktemp failed"
cleanup() { git worktree remove --force "$WT" >/dev/null 2>&1 || true; git worktree prune >/dev/null 2>&1 || true; }
trap cleanup EXIT

git worktree add -q --detach "$WT" "$OLD_TIP" || die "could not create worktree"
( cd "$WT" && git rebase --onto "$HEAD_SHA" "$OLD_BASE" --empty=drop ) \
  || die "rebase hit a conflict — resolve it by hand in $WT (worktree left in place)"
trap - EXIT   # keep the worktree if we got this far and something below fails

NEW_TIP="$(git -C "$WT" rev-parse HEAD)"

# The restacked branch must carry the same TREE-level additions it did before:
# it only re-adds files, so every file it added must still be present.
missing=0
while IFS= read -r f; do
  [ -z "$f" ] && continue
  git -C "$WT" cat-file -e "$NEW_TIP:$f" 2>/dev/null || { warn "missing after restack: $f"; missing=1; }
done < <(git diff --name-only "$OLD_BASE" "$OLD_TIP")
[ "$missing" = 0 ] || die "restacked branch lost files — not pushing"

say "restacked ${OLD_TIP:0:8} -> ${NEW_TIP:0:8} onto ${HEAD_SHA:0:8}"
git push --force-with-lease="$BRANCH:$OLD_TIP" origin "$NEW_TIP:refs/heads/$BRANCH" \
  || die "push rejected — someone else moved $BRANCH; re-run to pick up their version"
say "pushed $BRANCH"
cleanup
