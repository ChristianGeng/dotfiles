#!/usr/bin/env bash
# push-personal.sh — CONTROL-NODE orchestrator for the personal-setup PUSH model.
#
# Why a push model: personal-bootstrap.sh used to clone cgeng-ai-skills from
# GitLab ON the target. That fails on managed clusters (iva-p4d, iva-p5) where
# the target has no working GitLab token and non-interactive git cannot prompt.
# The fix is to PUSH the repos from the control node — which already has working
# checkouts — via rsync, so NO GitLab credentials are ever needed on the target.
#
# This orchestrator:
#   1. rsyncs myfiles/dotfiles and cgeng/cgeng-ai-skills from the control node's
#      ~/work into <remote_work> on the target,
#   2. copies personal-bootstrap.sh to the target's home, and
#   3. runs it there against <remote_work>.
#
# It works for both the token-less clusters (default remote_work ~/work) and the
# demo box (pass remote_work project/cgeng/work) — the layout difference is just
# the remote_work argument.
#
# Usage:
#   push-personal.sh <ssh-host> [remote_work]
#     <ssh-host>     an entry ssh can reach (Host in ~/.ssh/config, or user@host)
#     [remote_work]  checkout root on the target, expanded remotely; default ~/work
#
#   push-personal.sh iva-p5                                  # cluster: -> ~/work
#   push-personal.sh iva-demo-test-cgeng project/cgeng/work  # demo box: -> ~/project/cgeng/work
#
# Pass-through env (forwarded to the remote runner if set): PERSONAL, CLAUDE.
set -euo pipefail

HOST="${1:?usage: push-personal.sh <ssh-host> [remote_work]}"
REMOTE_WORK="${2:-~/work}"

HERE="$(cd "$(dirname "$0")" && pwd)"

# Control-node source checkouts (resolve from $HOME — do not hardcode a path).
SRC_MYFILES="$HOME/work/myfiles/dotfiles"
SRC_SKILLS="$HOME/work/cgeng/cgeng-ai-skills"

RSYNC_EXCLUDES=(
  --exclude .venv
  --exclude build
  --exclude htmlcov
  --exclude .coverage
  --exclude .cache
  --exclude __pycache__
  --exclude '*.egg-info'
)
# NOTE: .git is intentionally NOT excluded — the target keeps a real checkout.

[ -d "$SRC_MYFILES" ] || { echo "!! missing control-node source: $SRC_MYFILES" >&2; exit 1; }
[ -d "$SRC_SKILLS" ]  || { echo "!! missing control-node source: $SRC_SKILLS"  >&2; exit 1; }

echo "== push-personal: target=$HOST  remote_work=$REMOTE_WORK =="

echo
echo "== [1/4] rsync myfiles/dotfiles -> $HOST:$REMOTE_WORK/myfiles/dotfiles/ =="
ssh -o BatchMode=yes "$HOST" "mkdir -p $REMOTE_WORK/myfiles $REMOTE_WORK/cgeng"
rsync -az "${RSYNC_EXCLUDES[@]}" \
  "$SRC_MYFILES/" "$HOST:$REMOTE_WORK/myfiles/dotfiles/"

echo
echo "== [2/4] rsync cgeng/cgeng-ai-skills -> $HOST:$REMOTE_WORK/cgeng/cgeng-ai-skills/ =="
rsync -az "${RSYNC_EXCLUDES[@]}" \
  "$SRC_SKILLS/" "$HOST:$REMOTE_WORK/cgeng/cgeng-ai-skills/"

echo
echo "== [3/4] copy personal-bootstrap.sh -> $HOST:~/personal-bootstrap.sh =="
scp -q "$HERE/personal-bootstrap.sh" "$HOST:~/personal-bootstrap.sh"

echo
echo "== [4/4] run personal-bootstrap.sh on $HOST (WORK=$REMOTE_WORK) =="
# Forward selected env if set in this control-node shell.
REMOTE_ENV=""
[ -n "${PERSONAL:-}" ] && REMOTE_ENV+="PERSONAL=$(printf %q "$PERSONAL") "
[ -n "${CLAUDE:-}" ]   && REMOTE_ENV+="CLAUDE=$(printf %q "$CLAUDE") "
ssh -o BatchMode=yes "$HOST" \
  "${REMOTE_ENV}bash ~/personal-bootstrap.sh $REMOTE_WORK"

echo
echo "== push-personal: done (target=$HOST, remote_work=$REMOTE_WORK) =="
