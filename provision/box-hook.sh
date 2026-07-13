#!/usr/bin/env bash
set -euo pipefail
INV="${1:-inventory}"; HERE="$(cd "$(dirname "$0")" && pwd)"
uv run --no-config ansible-playbook -i "$INV" "$HERE/personal.yml"
[ -n "${AUDEERING_HOOK:-}" ] && "$AUDEERING_HOOK" "$INV" || true
