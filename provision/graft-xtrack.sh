#!/usr/bin/env bash
# graft-xtrack.sh — put the experiment-tracking layer on top of any branch.
#
# WHY THIS EXISTS
# ---------------
# cgeng/experiment-tracking holds the xtrack/ measurement layer for
# livekit-plugins-iva. It must never reach main, but it has to be applicable to
# any merge request, so that what a branch does to the plugin's runtime
# behaviour can be measured and pushed to MLflow.
#
# The layer is an ORPHAN branch containing nothing but xtrack/ — a path no other
# branch uses — and it edits no existing file. That is what makes this work:
# with no common ancestor git treats every path as added by one side, so nothing
# is deleted and an add/add conflict is impossible. Grafting is therefore a
# plain merge that cannot conflict, on any branch, with no per-branch
# bookkeeping.
#
# WHY NOT THE restack-agents.sh MODEL
# -----------------------------------
# restack-agents.sh moves a one-commit personal branch onto a moving parent with
# `rebase --onto`. That is right when there is ONE parent. Here the parent is any
# of ~70 branches, so a rebase model would mean tracking a base per branch. An
# orphan merges onto anything, repeatedly, with no state to keep.
#
# WHAT YOU GET
# ------------
# A throwaway worktree on a local-only branch xtrack/<target>. It is never
# pushed, so the target's CI never sees the layer and the merge stays private.
# Run the experiment inside the worktree it prints.
#
# Usage:
#   ./graft-xtrack.sh <target-branch>      # graft and print the worktree path
#   ./graft-xtrack.sh --list               # show existing grafts
#   ./graft-xtrack.sh --clean [target]     # remove one graft, or all of them
#   ./graft-xtrack.sh --force <target>     # recreate an existing graft
#
#   REPO=/path/to/checkout ./graft-xtrack.sh <target>
#   XTRACK_WT_ROOT=/somewhere ./graft-xtrack.sh <target>
set -uo pipefail

REPO="${REPO:-$HOME/work/research/research/iva/tools/livekit-plugins-iva}"
LAYER="${LAYER:-cgeng/experiment-tracking}"
WT_ROOT="${XTRACK_WT_ROOT:-$HOME/work/xtrack-worktrees}"

say()  { echo "== graft-xtrack: $*"; }
warn() { echo "!! graft-xtrack: $*" >&2; }
die()  { warn "$*"; exit 1; }

[ -d "$REPO/.git" ] || die "not a git repo: $REPO"

slug() { printf '%s' "$1" | tr '/' '-'; }

do_list() {
  say "grafts under $WT_ROOT"
  git -C "$REPO" worktree list | grep -F "$WT_ROOT" || echo "  (none)"
  say "local xtrack/* branches"
  git -C "$REPO" for-each-ref --format='  %(refname:short)' refs/heads/xtrack \
    || echo "  (none)"
}

# Remove a graft: the worktree first, then the branch it was checked out on.
# Order matters — git refuses to delete a branch a worktree still holds.
remove_one() {
  local branch="$1" path="$2"
  if [ -n "$path" ] && [ -d "$path" ]; then
    git -C "$REPO" worktree remove --force "$path" \
      || warn "could not remove worktree $path"
  fi
  git -C "$REPO" worktree prune
  if git -C "$REPO" rev-parse -q --verify "refs/heads/$branch" >/dev/null; then
    git -C "$REPO" branch -D "$branch" >/dev/null \
      && say "deleted branch $branch"
  fi
}

do_clean() {
  local target="${1:-}"
  if [ -n "$target" ]; then
    remove_one "xtrack/$target" "$WT_ROOT/$(slug "$target")"
    return
  fi
  # Nothing here is precious: every graft is reproducible from the layer.
  local branch
  while read -r branch; do
    [ -z "$branch" ] && continue
    remove_one "$branch" "$WT_ROOT/$(slug "${branch#xtrack/}")"
  done < <(git -C "$REPO" for-each-ref --format='%(refname:short)' refs/heads/xtrack)
  say "clean"
}

FORCE=0
case "${1:-}" in
  --list)  do_list; exit 0;;
  --clean) shift; do_clean "${1:-}"; exit 0;;
  --force) FORCE=1; shift;;
  -h|--help|"") sed -n '2,38p' "$0"; exit 0;;
esac

TARGET="${1:?target branch required}"
BRANCH="xtrack/$TARGET"
WT="$WT_ROOT/$(slug "$TARGET")"

git -C "$REPO" fetch -q origin || warn "git fetch failed; using local refs"
git -C "$REPO" rev-parse -q --verify "$LAYER" >/dev/null \
  || die "no such layer ref: $LAYER (fetch or create it first)"

# Accept a local branch as readily as a remote one: the point is to measure
# whatever tree you have, including one that was never pushed.
if git -C "$REPO" rev-parse -q --verify "origin/$TARGET" >/dev/null; then
  BASE="origin/$TARGET"
elif git -C "$REPO" rev-parse -q --verify "refs/heads/$TARGET" >/dev/null; then
  BASE="refs/heads/$TARGET"
else
  die "no such branch: $TARGET (tried origin/$TARGET and a local branch)"
fi

if git -C "$REPO" rev-parse -q --verify "refs/heads/$BRANCH" >/dev/null; then
  if [ "$FORCE" = 1 ]; then
    say "--force: removing the existing graft first"
    remove_one "$BRANCH" "$WT"
  else
    die "$BRANCH already exists — use --force to recreate, or --clean $TARGET"
  fi
fi

mkdir -p "$WT_ROOT"
say "grafting $LAYER onto $BASE"
git -C "$REPO" worktree add -q -b "$BRANCH" "$WT" "$BASE" \
  || die "could not create worktree at $WT"

if ! git -C "$WT" merge --allow-unrelated-histories --no-edit -q "$LAYER"; then
  warn "merge conflicted, which should be impossible for a pure-addition layer"
  warn "worktree left at $WT for inspection:"
  git -C "$WT" status --short >&2
  exit 1
fi

# Tree-level postcondition, the idea borrowed from restack-agents.sh: the graft
# only ever ADDS files, so every file the layer carries must be present
# afterwards. A merge that "succeeded" while dropping files is not a success.
missing=0
while IFS= read -r f; do
  [ -z "$f" ] && continue
  git -C "$WT" cat-file -e "HEAD:$f" 2>/dev/null \
    || { warn "missing after graft: $f"; missing=1; }
done < <(git -C "$REPO" ls-tree -r --name-only "$LAYER")
[ "$missing" = 0 ] || die "graft lost files — not handing this worktree back"

# The target's own tip, not the merge commit. This is the SHA under test, and it
# is what xtrack records as git.sha.
UNDER_TEST="$(git -C "$WT" rev-parse HEAD^1)"

say "worktree ready: $WT"
say "branch $BRANCH (local only — never pushed)"
say "measuring ${UNDER_TEST:0:12} ($TARGET)"
cat <<EOF

Next:
  cd $WT
  cp $REPO/.env.local .env.local     # credentials are not in git
  python3.12 xtrack/run.py --case xtrack/cases/conv.toml

Note: a fresh worktree needs its own uv environment on first run. Point
UV_PROJECT_ENVIRONMENT at a shared path if the disk cost matters.

Remove when done:
  $(basename "$0") --clean $TARGET
EOF
