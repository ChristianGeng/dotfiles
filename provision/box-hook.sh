#!/usr/bin/env bash
# box-hook.sh — the DEMO box's personal-layer POST_HOOK entrypoint.
#
# Invoked by the ec2 provisioning as a POST_HOOK with the ec2 repo's inventory.
# It runs personal.yml, which is now a THIN wrapper that stages the repos and
# then runs the SHARED provision/personal-bootstrap.sh (PERSONAL_TARGET=demo) —
# the same script the iva-p5 clusters run via provision/push-personal.sh. So the
# demo box and the clusters share one implementation; only the entrypoint and
# the WORK root differ. See personal-bootstrap.sh / personal.yml for details.
set -euo pipefail
INV="${1:-inventory}"; HERE="$(cd "$(dirname "$0")" && pwd)"
uv run --no-config ansible-playbook -i "$INV" "$HERE/personal.yml"
# shellcheck disable=SC2015  # optional chained hook; the trailing `|| true` keeps it non-fatal
[ -n "${AUDEERING_HOOK:-}" ] && "$AUDEERING_HOOK" "$INV" || true
