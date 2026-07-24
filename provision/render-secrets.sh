#!/usr/bin/env bash
# render-secrets.sh — CONTROL-NODE pre-render of all secret files from gopass.
#
# The demo box (and other targets) never get gopass, GPG keys, or pass stores:
# the box is internet-facing and recreated daily, so cgeng's GPG key — which
# unlocks all three stores — must never land there. Instead, every secret file
# is rendered HERE on the control node from the gopass stores, and ansible
# copies the rendered files (mode 0600) to the target:
#
#   gopass (control node) -> rendered files -> ansible copy -> box
#
# Renders (all from `gopass show -o`, see the specs for the mappings):
#   ~/.local_configs              scripts/generate-local-configs
#                                 (spec: bash/.local_configs.spec, personal/…)
#   ~/.audeering_configs          scripts/generate-local-configs --profile audeering
#                                 (spec: audeering-dotfiles/audeering-config/
#                                  .audeering_configs.spec, aud/…)
#   ~/.aws/{config,credentials}   audeering-dotfiles/render-aws-config.sh
#   ~/.config/audbackend/minio.cfg  (reads aud/iva/aws/…)
#
# Consumers: the ec2-instance-setup playbook copies these to the demo box, where
# clone-audeering-repos.sh sources ~/.audeering_configs for GITLAB_API_TOKEN;
# audeering-dotfiles/deploy-secrets.yml pushes them to other targets.
#
# WIRING (demo box roll-up): call this from the gitignored ec2 scripts/roll.env
# so every daily roll-up re-renders BEFORE the playbook copies the files:
#
#   "$HOME/work/myfiles/dotfiles/provision/render-secrets.sh" \
#     || echo "WARNING: render-secrets failed; deploying existing files" >&2
#
# Exits non-zero (loudly) if any render or validation fails; it never prints a
# secret value. Requires gopass + the GPG key (decryption) + emacs, i.e. the
# control node.
set -uo pipefail

warn() { echo "!! render-secrets: $*" >&2; }
fail=0

# gopass is a personal install; probe the usual rootless locations.
if ! command -v gopass >/dev/null 2>&1; then
  for d in "$HOME/go/bin" "$HOME/.local/bin"; do
    [ -x "$d/gopass" ] && { export PATH="$d:$PATH"; break; }
  done
fi
command -v gopass >/dev/null 2>&1 || { warn "gopass not found"; exit 1; }

MYFILES="$(cd "$(dirname "$0")/.." && pwd)"
AUD_DOTFILES="${AUDEERING_DOTFILES_DIR:-$HOME/work/cgeng/audeering-dotfiles}"

echo "== render-secrets: rendering from gopass (control node) =="

"$MYFILES/scripts/generate-local-configs" >/dev/null \
  && echo "  ok  ~/.local_configs" \
  || { warn "generate-local-configs (private) failed"; fail=1; }

"$MYFILES/scripts/generate-local-configs" --profile audeering >/dev/null \
  && echo "  ok  ~/.audeering_configs" \
  || { warn "generate-local-configs --profile audeering failed"; fail=1; }

if [ -x "$AUD_DOTFILES/render-aws-config.sh" ]; then
  "$AUD_DOTFILES/render-aws-config.sh" >/dev/null \
    && echo "  ok  ~/.aws/{config,credentials} + minio.cfg" \
    || { warn "render-aws-config.sh failed"; fail=1; }
else
  warn "render-aws-config.sh not found at $AUD_DOTFILES"; fail=1
fi

# Validate what the provisioning depends on — presence, 0600, and that the
# box-critical GITLAB_API_TOKEN rendered non-empty. Never print values.
for f in "$HOME/.local_configs" "$HOME/.audeering_configs" \
         "$HOME/.aws/config" "$HOME/.aws/credentials" \
         "$HOME/.config/audbackend/minio.cfg"; do
  if [ ! -f "$f" ]; then warn "missing: $f"; fail=1; continue; fi
  mode="$(stat -c %a "$f")"
  [ "$mode" = "600" ] || { warn "$f has mode $mode (expected 600)"; fail=1; }
done
grep -qE '^\s*export GITLAB_API_TOKEN=.{10,}' "$HOME/.audeering_configs" \
  || { warn "GITLAB_API_TOKEN did not render into ~/.audeering_configs"; fail=1; }

if [ "$fail" -ne 0 ]; then
  warn "FAILED — targets would receive stale/incomplete secrets"
  exit 1
fi
echo "== render-secrets: done (all rendered + validated) =="
