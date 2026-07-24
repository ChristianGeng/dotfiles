#!/usr/bin/env bash
# deploy-secrets-iva-p5.sh — render secrets on the CONTROL NODE, deploy them to
# the iva-p5 HyperPod login node as REAL files at 0600.
#
# Same model as render-secrets.sh (gopass -> rendered 0600 files -> copy):
# the cluster never gets gopass, GPG keys, or pass stores. iva-p5 is a SHARED,
# team-managed cluster (no sudo; $HOME = /fsx/christian.geng on a shared Lustre
# FS with a 755 home dir), so 0600 on every secret file is security-critical.
#
# Honors the onboarding conventions (research/iva/aws/onboarding README):
#   - cluster cache/env vars (AUDMODEL_CACHE_ROOT, HF_HOME, AUDB_CACHE_ROOT,
#     UV_CACHE_DIR, ...) are owned by /etc/profile.d/iva-envs.sh — the deployed
#     files must NOT re-export them (a guard below enforces this), and no PATH
#     exports either; the .audeering_configs deployed here is the minimal
#     .audeering_configs.iva-p5.template variant;
#   - the HF token lives in a private ~/.config/hf-token (0600) sourced from
#     the shell rc — NEVER `hf auth login` on the cluster (HF_HOME is shared);
#   - non-secret AUGLIB_CACHE_ROOT (not cluster-provided yet) goes into a
#     separate plain env file, ~/.config/iva-p5-env.sh, not the secrets files.
#
# Deploys (pre-existing files/symlinks are first backed up on the target to
# ~/.config/secrets-backup-<timestamp>/, mode 0700; old symlinks are replaced,
# their TARGET files inside the old repo checkouts are left untouched):
#   ~/.local_configs                  standard render (personal API keys)
#   ~/.audeering_configs              iva-p5 variant (work creds, no PATH)
#   ~/.aws/config ~/.aws/credentials  rendered AWS profiles
#   ~/.config/audbackend/minio.cfg    audb/audmodel S3 auth
#   ~/.config/hf-token                export HF_TOKEN=... (0600)
#   ~/.config/iva-p5-env.sh           non-secret AUGLIB_CACHE_ROOT (0644)
# and appends guarded source lines to ~/.bashrc AND ~/.profile (the stock
# .bashrc returns early for non-interactive shells, so login shells à la
# `ssh iva-p5 bash -lc ...` and Slurm prologues need ~/.profile).
#
# AWS safety: the pre-existing credentials may hold a different IAM identity.
# The script records `aws sts get-caller-identity` before and after; if the
# identity CHANGED and the S3 smoke test (`aws s3 ls $CKPT_S3_ROOT/`) FAILS,
# it restores the previous credentials from the backup (still forcing 0600)
# and reports the discrepancy instead of forcing the change.
#
# Never prints a secret value. Idempotent — safe to re-run.

set -uo pipefail

TARGET="${IVA_P5_HOST:-iva-p5}"
MYFILES="$(cd "$(dirname "$0")/.." && pwd)"
AUD_DOTFILES="${AUDEERING_DOTFILES_DIR:-$HOME/work/cgeng/audeering-dotfiles}"
P5_TEMPLATE="$AUD_DOTFILES/audeering-config/.audeering_configs.iva-p5.template"

say()  { echo "== deploy-secrets-iva-p5: $*"; }
warn() { echo "!! deploy-secrets-iva-p5: $*" >&2; }
die()  { warn "$*"; exit 1; }

# gopass is a personal install; probe the usual rootless locations.
if ! command -v gopass >/dev/null 2>&1; then
  for d in "$HOME/go/bin" "$HOME/.local/bin"; do
    [ -x "$d/gopass" ] && { export PATH="$d:$PATH"; break; }
  done
fi
command -v gopass >/dev/null 2>&1 || die "gopass not found (control node only)"
[ -f "$P5_TEMPLATE" ] || die "missing p5 template: $P5_TEMPLATE"

# ---------------------------------------------------------------- 1. render
say "rendering from gopass (control node)"
"$MYFILES/provision/render-secrets.sh" || die "render-secrets.sh failed"

# ------------------------------------------------- 2. stage ($HOME layout)
umask 077
STAGE="$(mktemp -d)" || die "mktemp failed"
trap 'rm -rf "$STAGE"' EXIT
mkdir -p "$STAGE/.aws" "$STAGE/.config/audbackend"

install -m 600 "$HOME/.local_configs"                "$STAGE/.local_configs"
install -m 600 "$HOME/.aws/config"                   "$STAGE/.aws/config"
install -m 600 "$HOME/.aws/credentials"              "$STAGE/.aws/credentials"
install -m 600 "$HOME/.config/audbackend/minio.cfg"  "$STAGE/.config/audbackend/minio.cfg"

# p5 variant of .audeering_configs (minimal; generator chmods 600)
"$MYFILES/scripts/generate-local-configs" --profile audeering \
    --template "$P5_TEMPLATE" --output "$STAGE/.audeering_configs" >/dev/null \
  || die "rendering the iva-p5 .audeering_configs variant failed"

# ~/.config/hf-token per onboarding README (never `hf auth login` on cluster)
hf_token="$(gopass show -o personal/huggingface/api_key 2>/dev/null)"
[ -n "$hf_token" ] || die "gopass entry personal/huggingface/api_key is empty/missing"
printf 'export HF_TOKEN="%s"\n' "$hf_token" > "$STAGE/.config/hf-token"
chmod 600 "$STAGE/.config/hf-token"
unset hf_token

# non-secret personal env — kept OUT of the secrets files on purpose
cat > "$STAGE/.config/iva-p5-env.sh" <<'EOF'
# iva-p5 personal, NON-SECRET environment (deploy-secrets-iva-p5.sh).
# Cluster-provided vars live in /etc/profile.d/iva-envs.sh — do not add
# them here. AUGLIB_CACHE_ROOT is not cluster-provided (yet); candidate
# for iva-envs.sh. Same ${VAR:-default} pattern as the cluster file.
export AUGLIB_CACHE_ROOT="${AUGLIB_CACHE_ROOT:-/fsx/cache/auglib}"
EOF
chmod 644 "$STAGE/.config/iva-p5-env.sh"

# ------------------------------------------------------------- 3. guards
# The sourced shell files must not shadow cluster-provided vars, export PATH,
# or carry unrendered ${PLACEHOLDER}s. Print var names only, never lines.
FORBIDDEN='PATH|FSX_CACHE|NVME_LOCAL|AUDMODEL_CACHE_ROOT|AUDB_CACHE_ROOT|HF_HOME|DATASETS_CACHE_ROOT|UV_CACHE_DIR|CKPT_S3_ROOT|MLFLOW_TRACKING_URI'
for f in "$STAGE/.local_configs" "$STAGE/.audeering_configs"; do
  hits="$(grep -oE "^[[:space:]]*(export[[:space:]]+)?($FORBIDDEN)=" "$f" | tr -d ' =' || true)"
  [ -z "$hits" ] || die "$(basename "$f") would shadow cluster vars/PATH: $hits"
  ph="$(grep -vE '^[[:space:]]*#' "$f" | grep -oE '\$\{[A-Za-z_]+\}' || true)"
  [ -z "$ph" ] || die "$(basename "$f") has unrendered placeholders: $ph"
done
grep -qE '^\s*export GITLAB_API_TOKEN=.{10,}' "$STAGE/.audeering_configs" \
  || die "GITLAB_API_TOKEN did not render into the p5 .audeering_configs"

# ----------------------------------------- 4. pre-deploy AWS identity (p5)
OLD_ARN="$(ssh "$TARGET" 'bash -lc "aws sts get-caller-identity --query Arn --output text"' 2>/dev/null || true)"
say "pre-deploy AWS identity on $TARGET: ${OLD_ARN:-<none/invalid>}"

# ---------------------------------------- 5. backup + drop symlinks (p5)
say "backing up existing files on $TARGET (0700 backup dir)"
BACKUP_DIR="$(ssh "$TARGET" bash -s <<'EOSH'
set -euo pipefail
umask 077
bk="$HOME/.config/secrets-backup-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$bk" && chmod 700 "$bk"
man="$bk/MANIFEST"
: > "$man"
for f in .local_configs .audeering_configs .aws/config .aws/credentials \
         .config/audbackend/minio.cfg .config/hf-token .config/iva-p5-env.sh; do
  p="$HOME/$f"; safe="${f//\//__}"
  if [ -L "$p" ]; then
    echo "$f: symlink -> $(readlink "$p")" >> "$man"
    if [ -e "$p" ]; then cp -L "$p" "$bk/$safe" && chmod 600 "$bk/$safe"; fi
    rm -f "$p"                      # only the $HOME symlink; target stays
  elif [ -e "$p" ]; then
    echo "$f: regular file (mode $(stat -c %a "$p"))" >> "$man"
    cp "$p" "$bk/$safe" && chmod 600 "$bk/$safe"
    rm -f "$p"
  fi
done
chmod 600 "$man"
echo "$bk"
EOSH
)" || die "backup step on $TARGET failed"
say "backup on $TARGET: $BACKUP_DIR"

# --------------------------------------------------------- 6. deploy (p5)
say "copying rendered files to $TARGET (0600, real files)"
tar -C "$STAGE" -cf - . \
  | ssh "$TARGET" 'umask 077; mkdir -p "$HOME/.aws" "$HOME/.config/audbackend";
                   tar -C "$HOME" -xf - --no-same-owner --no-overwrite-dir' \
  || die "file transfer to $TARGET failed"

# ------------------------------------------- 7. shell rc wiring (p5)
say "ensuring ~/.bashrc and ~/.profile source the deployed files"
ssh "$TARGET" bash -s <<'EOSH' || die "rc wiring on target failed"
set -euo pipefail
marker='# --- rendered secrets (deploy-secrets-iva-p5.sh) ---'
# ensure_sourced <rcfile> <path-suffix> <guarded-line>
# Skips if the rc file already sources the path in ANY form (stock .bashrc
# uses '. ~/.local_configs'); otherwise appends the guarded line once,
# under a marker comment.
ensure_sourced() {
  local rc="$1" esc line="$3"
  esc="${2//./\\.}"
  grep -qE "(^|[[:space:]])(\.|source)[[:space:]]+[^[:space:]]*${esc}\"?([[:space:]]|$)" "$rc" 2>/dev/null && return 0
  grep -qxF "$marker" "$rc" 2>/dev/null || printf '\n%s\n' "$marker" >> "$rc"
  printf '%s\n' "$line" >> "$rc"
}
# ~/.bashrc covers interactive shells; ~/.profile covers login shells
# (incl. non-interactive `bash -lc ...`, which .bashrc returns early from).
for rc in "$HOME/.bashrc" "$HOME/.profile"; do
  ensure_sourced "$rc" ".local_configs" \
    '[ -f "$HOME/.local_configs" ] && . "$HOME/.local_configs"'
  ensure_sourced "$rc" ".audeering_configs" \
    '[ -f "$HOME/.audeering_configs" ] && . "$HOME/.audeering_configs"'
  ensure_sourced "$rc" ".config/hf-token" \
    '[ -f "$HOME/.config/hf-token" ] && . "$HOME/.config/hf-token"'
  ensure_sourced "$rc" ".config/iva-p5-env.sh" \
    '[ -f "$HOME/.config/iva-p5-env.sh" ] && . "$HOME/.config/iva-p5-env.sh"'
done
EOSH

# --------------------------------------- 8. verify files + AWS smoke (p5)
say "verifying deployed files on $TARGET"
ssh "$TARGET" bash -s <<'EOSH' || die "deployed-file verification failed"
set -euo pipefail
fail=0
for f in .local_configs .audeering_configs .aws/config .aws/credentials \
         .config/audbackend/minio.cfg .config/hf-token; do
  p="$HOME/$f"
  if [ -L "$p" ] || [ ! -f "$p" ]; then echo "BAD  $f: not a regular file" >&2; fail=1; continue; fi
  mode="$(stat -c %a "$p")"; owner="$(stat -c %U "$p")"
  [ "$mode" = 600 ] || { echo "BAD  $f: mode $mode" >&2; fail=1; }
  [ "$owner" = "$(id -un)" ] || { echo "BAD  $f: owner $owner" >&2; fail=1; }
  echo "ok   $f ($mode $owner)"
done
exit $fail
EOSH

NEW_ARN="$(ssh "$TARGET" 'bash -lc "aws sts get-caller-identity --query Arn --output text"' 2>/dev/null || true)"
say "post-deploy AWS identity on $TARGET: ${NEW_ARN:-<none/invalid>}"
SMOKE_OK=0
ssh "$TARGET" 'bash -lc "aws s3 ls \"\$CKPT_S3_ROOT/\" >/dev/null"' 2>/dev/null && SMOKE_OK=1
if [ "$SMOKE_OK" = 1 ] && [ -n "$NEW_ARN" ]; then
  say "AWS OK: identity $NEW_ARN, S3 smoke test (\$CKPT_S3_ROOT) passed"
elif [ -n "$OLD_ARN" ] && [ "$NEW_ARN" != "$OLD_ARN" ]; then
  warn "AWS identity changed ($OLD_ARN -> ${NEW_ARN:-invalid}) AND smoke test failed"
  warn "restoring previous ~/.aws/{config,credentials} from $BACKUP_DIR (forcing 0600)"
  ssh "$TARGET" "set -e
    bk='$BACKUP_DIR'
    [ -f \"\$bk/.aws__credentials\" ] && install -m 600 \"\$bk/.aws__credentials\" \"\$HOME/.aws/credentials\"
    [ -f \"\$bk/.aws__config\" ] && install -m 600 \"\$bk/.aws__config\" \"\$HOME/.aws/config\"
    true" || warn "restore failed — inspect $BACKUP_DIR on $TARGET manually"
  warn "kept the PREVIOUS AWS credentials (now 0600); rendered creds NOT active"
else
  warn "AWS smoke test failed, but identity is unchanged/was already invalid — rendered files left in place; investigate"
fi

say "done (backup: $BACKUP_DIR on $TARGET)"
