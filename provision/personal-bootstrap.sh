#!/usr/bin/env bash
# personal-bootstrap.sh — REMOTE-SIDE runner for the control-node PUSH model.
#
# ONE idempotent, environment-aware personal-layer deploy for BOTH targets:
#
#   - DEMO box     : WORK=~/project/cgeng/work; normal instance disk (ext4);
#                    HAS a GitLab tunnel (glab API works). Provisioned as the
#                    ec2 POST_HOOK (provision/box-hook.sh -> personal.yml) and/or
#                    manually via provision/push-personal.sh.
#   - iva-p5 etc.  : WORK=~/work; $HOME on /fsx (Lustre, slow for Doom package
#                    loads); NO GitLab tunnel (glab 401s — must be non-fatal);
#                    node-local scratch at $NVME_LOCAL (default /opt/dlami/nvme).
#
# Runs ON the target and assumes the repos are ALREADY staged under $WORK — the
# control node rsyncs them here first (see provision/push-personal.sh). This
# script does NOT clone myfiles or cgeng-ai-skills: that is exactly what fails on
# managed clusters, where the target has no working GitLab token and
# non-interactive git cannot prompt. The only thing still cloned is the public
# doomemacs framework (GitHub, reachable everywhere).
#
# Sets up, all under $HOME with no sudo:
#   - Doom Emacs   (chemacs `emacs` package + `doom` config package + framework)
#   - Claude Code  (cgeng-ai-skills skills + MCP servers, via bootstrap-claude.sh)
# and installs the user-level tools they need (uv, ruff, claude) into ~/.local.
# On DEMO-like (normal-FS) targets it ALSO applies the Claude TMPDIR hygiene that
# used to live only in provision/personal.yml (see the "demo hygiene" step).
#
# Deliberately narrow footprint on a shared box: it stows ONLY the `emacs` and
# `doom` dotfiles packages (NOT bash/git/tmux/etc. — your shell config is left
# alone). The demo POST_HOOK layers a full `stow-deploy.sh` on top separately;
# see provision/personal.yml.
#
# Everything below is designed to CONVERGE on re-run: no step duplicates work or
# errors on a second pass (set -uo pipefail, NO -e — a failing optional step
# WARNS and continues; the deploy is intentionally graceful).
#
# Requirements on the target (true on the managed clusters):
#   - base tools: git, stow, curl, rsync
#   - the repos already staged under $WORK (myfiles/dotfiles, cgeng/cgeng-ai-skills)
#   - an emacs on PATH for `doom sync` (if conda/module-provided and absent from
#     a non-interactive PATH, this script probes common miniconda locations)
#
# Usage:
#   ./personal-bootstrap.sh                       # WORK defaults to ~/work (clusters)
#   ./personal-bootstrap.sh project/cgeng/work    # positional WORK (already expanded by caller)
#   WORK=/scratch/me ./personal-bootstrap.sh      # or via the environment
#   CLAUDE_CONFIG_DIR=~/.claude-work ./personal-bootstrap.sh   # target a specific claude config
#
# Environment knobs (all optional):
#   PERSONAL_TARGET   demo|cluster  — force the profile (default: auto-detect by FS)
#   NVME_LOCAL        node-local scratch base (default: auto /opt/dlami/nvme)
#   GITLAB_TOKEN      alias for GITLAB_API_TOKEN (bridged for bootstrap-claude)
#   GITLAB_HOST       GitLab host   (default: gitlab.audeering.com)
#
# The push orchestrator passes the correct WORK for each host (~/work for the
# clusters, ~/project/cgeng/work for the demo box), so the argument is normally
# an absolute path already expanded by the caller's shell.
set -uo pipefail

WORK="${1:-${WORK:-$HOME/work}}"
MYFILES="$WORK/myfiles/dotfiles"
SKILLS="$WORK/cgeng/cgeng-ai-skills"
DOOM_URL="https://github.com/doomemacs/doomemacs.git"   # public (GitHub)

warn() { echo "!! $*" >&2; }
step() { echo; echo "== $* =="; }

export PATH="$HOME/.local/bin:$PATH"

# Doom needs an emacs binary; on managed clusters it is often conda/module
# provided and absent from a non-interactive shell's PATH — probe for it.
if ! command -v emacs >/dev/null 2>&1; then
  for d in "$HOME"/miniconda*/bin "$HOME"/anaconda*/bin /fsx/"$USER"/miniconda*/bin; do
    [ -x "$d/emacs" ] && { export PATH="$d:$PATH"; break; }
  done
fi

# ---------------------------------------------------------------------------
# Environment detection: network/slow FS vs normal disk, and the target profile.
#
# The SAME signal drives two decisions:
#   1. Doom package tree staging (network FS => stage to node-local scratch), and
#   2. which profile we are (network FS => cluster; normal disk => demo).
# ---------------------------------------------------------------------------
HOME_FSTYPE="$(stat -f -c %T "$HOME" 2>/dev/null || echo unknown)"

# Resolve node-local scratch: honor $NVME_LOCAL, else the cluster onboarding
# default /opt/dlami/nvme if it exists.
NVME_LOCAL="${NVME_LOCAL:-}"
if [ -z "$NVME_LOCAL" ] && [ -d /opt/dlami/nvme ]; then
  NVME_LOCAL="/opt/dlami/nvme"
fi

# Network/slow FS if $HOME sits on a known network filesystem OR node-local
# scratch is present (the defining trait of the managed clusters).
NETWORK_FS="no"
case "$HOME_FSTYPE" in
  lustre|nfs|nfs4|fhgfs|beegfs|gpfs|smb2|smb3|cifs|9p|fuseblk) NETWORK_FS="yes" ;;
esac
[ -n "$NVME_LOCAL" ] && [ -d "$NVME_LOCAL" ] && NETWORK_FS="yes"

# Target profile: explicit override wins, else derive from the FS signal.
PERSONAL_TARGET="${PERSONAL_TARGET:-}"
if [ -z "$PERSONAL_TARGET" ]; then
  if [ "$NETWORK_FS" = "yes" ]; then PERSONAL_TARGET="cluster"; else PERSONAL_TARGET="demo"; fi
fi

echo "== personal-bootstrap: WORK=$WORK  home_fs=$HOME_FSTYPE  network_fs=$NETWORK_FS  target=$PERSONAL_TARGET =="

# ---------------------------------------------------------------------------
step "user-level tools -> ~/.local (install or upgrade; harmless if present)"
# ---------------------------------------------------------------------------
if command -v uv >/dev/null 2>&1; then uv self update || warn "uv self update failed"
else curl -LsSf https://astral.sh/uv/install.sh | sh || warn "uv install failed"; fi
command -v ruff >/dev/null 2>&1 || curl -LsSf https://astral.sh/ruff/install.sh | sh || warn "ruff install failed"
if command -v claude >/dev/null 2>&1; then claude update || warn "claude update failed"
else curl -fsSL https://claude.ai/install.sh | bash || warn "claude install failed"; fi
hash -r

# ---------------------------------------------------------------------------
step "dotfiles: stow ONLY the emacs + doom packages (repos already staged)"
# ---------------------------------------------------------------------------
if [ ! -d "$MYFILES" ]; then
  warn "no dotfiles at $MYFILES (did the control node rsync run?); skipped dotfiles"
elif command -v stow >/dev/null 2>&1 && [ -d "$MYFILES/emacs" ]; then
  # Back up any pre-existing REAL files that would block stow (chemacs seeds
  # ~/.emacs.d/*.el), then stow just these two packages into $HOME. Re-running
  # is a no-op: stow leaves symlinks it already owns untouched.
  ( cd "$MYFILES"
    for pkg in emacs doom; do
      stow --no-folding -n -v -t "$HOME" "$pkg" 2>&1 \
        | sed -n 's/.*existing target is neither a link nor a directory: //p' \
        | while IFS= read -r rel; do
            [ -n "$rel" ] && [ -e "$HOME/$rel" ] && [ ! -L "$HOME/$rel" ] \
              && mv "$HOME/$rel" "$HOME/$rel.bak.$(date +%Y%m%d%H%M%S)"
          done
      stow --no-folding -t "$HOME" "$pkg" || warn "stow $pkg failed"
    done ) || warn "stow (emacs/doom) failed"
else
  warn "stow or the emacs package is missing; skipped dotfiles"
fi

# ---------------------------------------------------------------------------
step "Doom Emacs (env-aware: node-local staging on a network FS, in-place otherwise)"
# ---------------------------------------------------------------------------
export DOOMDIR="${DOOMDIR:-$HOME/.config/doom}"   # stowed config; never relocated

# doom_sync_tree <doom-root>: idempotent install (once) + non-interactive sync.
# `doom sync` can hit interactive straight.el prompts (e.g. "straight.el HEAD on
# master is behind develop — Checkout develop? 1/2"). No TTY here, so feed "2"
# (the recommended answer) to every prompt.
doom_sync_tree() {
  local root="$1"
  if command -v emacs >/dev/null 2>&1 && [ -x "$root/bin/doom" ]; then
    [ -d "$root/.local" ] || "$root/bin/doom" install --force --no-env --no-config \
      || warn "doom install failed ($root)"
    yes 2 | "$root/bin/doom" sync || warn "doom sync failed ($root)"
  else
    warn "no emacs on PATH — skipped Doom sync (load your emacs module, then re-run)"
  fi
}

if [ "$NETWORK_FS" != "yes" ]; then
  # -------- NORMAL DISK (demo): keep .local in place; behavior UNCHANGED. --------
  if [ ! -d "$HOME/doom-emacs/.git" ]; then
    echo "  clone $DOOM_URL -> ~/doom-emacs"
    git clone "$DOOM_URL" "$HOME/doom-emacs" || warn "doomemacs clone failed (continuing)"
  else
    echo "  exists (leaving as-is): ~/doom-emacs"
  fi
  doom_sync_tree "$HOME/doom-emacs"

else
  # ======================================================================
  # NETWORK / SLOW FS (e.g. iva-p5, $HOME on Lustre): stage the Doom package
  # tree onto NODE-LOCAL storage and run/point Emacs THERE.
  #
  # >>> NEEDS LIVE VERIFICATION ON iva-p5. <<<
  # Profiling proved that naive shims (env vars, symlinking only .local, or
  # DOOMLOCALDIR) do NOT move the hot path off the network FS: package autoloads
  # still bake network-FS paths and startup stays slow. The ONLY thing that
  # worked is having the package tree PHYSICALLY on local storage and running
  # `doom sync` THERE so the generated autoloads bake node-local absolute paths.
  #
  # Design (robust + re-stageable for EPHEMERAL node-local scratch):
  #   - Canonical Doom lives on the HOME FS at $DOOM_CANON (framework + a warm
  #     .local package cache). It is never the runtime.
  #   - The runtime lives on node-local scratch at $DOOM_LOCAL. Each run, if that
  #     copy is missing or stale (node was wiped / canonical framework moved), we
  #     rsync canonical -> local and `doom sync` there.
  #   - ~/doom-emacs (the path the chemacs "doom" profile points its
  #     user-emacs-directory at) is a SYMLINK to $DOOM_LOCAL, so a plain `emacs`
  #     automatically runs the node-local tree with no per-launch flags.
  # ======================================================================
  local_base="$NVME_LOCAL"
  { [ -z "$local_base" ] || ! mkdir -p "$local_base/.probe" 2>/dev/null; } \
    && local_base="/dev/shm/$USER"
  rm -rf "${NVME_LOCAL:-/nonexistent}/.probe" 2>/dev/null || true
  mkdir -p "$local_base" 2>/dev/null || warn "cannot create node-local base $local_base"

  DOOM_LOCAL="$local_base/doom"          # node-local runtime (user-emacs-directory)
  DOOM_CANON="$HOME/doom-emacs.canon"    # home-FS canonical (framework + pkg cache)
  echo "  node-local runtime : $DOOM_LOCAL"
  echo "  home-FS canonical  : $DOOM_CANON"

  # 1. Establish the canonical framework on the home FS (clone once, else keep).
  #    Adopt a pre-existing real ~/doom-emacs clone as the canonical copy.
  if [ ! -d "$DOOM_CANON/.git" ]; then
    if [ -d "$HOME/doom-emacs/.git" ] && [ ! -L "$HOME/doom-emacs" ]; then
      echo "  adopting existing ~/doom-emacs as canonical -> $DOOM_CANON"
      mv "$HOME/doom-emacs" "$DOOM_CANON" || warn "could not move ~/doom-emacs to canonical"
    else
      echo "  clone $DOOM_URL -> $DOOM_CANON"
      git clone "$DOOM_URL" "$DOOM_CANON" || warn "doomemacs clone failed (continuing)"
    fi
  else
    echo "  canonical exists (leaving as-is): $DOOM_CANON"
  fi

  # 2. Make ~/doom-emacs a symlink to the node-local runtime (never a real dir).
  #    Preserve any stray real dir rather than deleting it.
  if [ -e "$HOME/doom-emacs" ] && [ ! -L "$HOME/doom-emacs" ]; then
    mv "$HOME/doom-emacs" "$HOME/doom-emacs.bak.$(date +%Y%m%d%H%M%S)" \
      || warn "could not back up stray real ~/doom-emacs"
  fi
  ln -sfn "$DOOM_LOCAL" "$HOME/doom-emacs"

  # 3. (Re-)stage canonical -> node-local when the local copy is missing or the
  #    canonical framework moved. NO --delete: preserve the local .local across
  #    framework-only updates; `doom sync` reconciles afterwards. Idempotent:
  #    when fresh, this whole block is skipped.
  canon_rev="$(git -C "$DOOM_CANON" rev-parse HEAD 2>/dev/null || echo none)"
  stamp="$DOOM_LOCAL/.staged-from-rev"
  if [ ! -x "$DOOM_LOCAL/bin/doom" ] || [ "$(cat "$stamp" 2>/dev/null || echo)" != "$canon_rev" ]; then
    echo "  staging canonical -> node-local (rev=$canon_rev)"
    mkdir -p "$DOOM_LOCAL"
    if command -v rsync >/dev/null 2>&1; then
      rsync -a "$DOOM_CANON/" "$DOOM_LOCAL/" || warn "rsync canonical -> node-local failed"
    else
      cp -a "$DOOM_CANON/." "$DOOM_LOCAL/" || warn "cp canonical -> node-local failed"
    fi
    echo "$canon_rev" > "$stamp" 2>/dev/null || true
  else
    echo "  node-local copy is fresh (rev=$canon_rev) — skipping re-stage"
  fi

  # 4. Install/sync ON node-local so autoloads bake node-local absolute paths.
  doom_sync_tree "$DOOM_LOCAL"

  # 5. Warm the canonical package cache the FIRST time only, so a later node wipe
  #    re-stages packages without re-downloading. Cheap best-effort; skipped once
  #    the cache exists (avoids repeatedly writing a big .local to the slow FS).
  if [ ! -d "$DOOM_CANON/.local" ] && [ -d "$DOOM_LOCAL/.local" ] && command -v rsync >/dev/null 2>&1; then
    echo "  warming canonical package cache (one-time)"
    rsync -a "$DOOM_LOCAL/.local/" "$DOOM_CANON/.local/" || warn "canonical cache warm failed (non-fatal)"
  fi
fi

# Make Doom the default chemacs profile on this box (idempotent overwrite).
printf 'doom' > "$HOME/.emacs-profile"

# ---------------------------------------------------------------------------
step "GitLab env bridge (non-fatal): normalize GITLAB_TOKEN/GITLAB_HOST"
# ---------------------------------------------------------------------------
# The setup doc's Phase 3 speaks of GITLAB_TOKEN/GITLAB_HOST; bootstrap-claude
# reads GITLAB_API_TOKEN and defaults the host to gitlab.audeering.com. Bridge
# the names here so either convention works. This ONLY exports env for the
# bootstrap-claude call below — it never authenticates and never fails: the demo
# box authenticates through its tunnel, and iva-p5 has no tunnel (glab 401s),
# which must NOT abort the deploy.
export GITLAB_HOST="${GITLAB_HOST:-gitlab.audeering.com}"
if [ -z "${GITLAB_API_TOKEN:-}" ] && [ -n "${GITLAB_TOKEN:-}" ]; then
  export GITLAB_API_TOKEN="$GITLAB_TOKEN"
  echo "  bridged GITLAB_TOKEN -> GITLAB_API_TOKEN"
fi
echo "  GITLAB_HOST=$GITLAB_HOST  (glab auth is best-effort / non-fatal)"

# ---------------------------------------------------------------------------
step "Claude Code: skills + MCP servers (cgeng-ai-skills bootstrap)"
# ---------------------------------------------------------------------------
if [ -x "$SKILLS/scripts/bootstrap-claude.sh" ]; then
  "$SKILLS/scripts/bootstrap-claude.sh" || warn "bootstrap-claude failed"
else
  warn "bootstrap-claude.sh not found in $SKILLS; skipped Claude setup"
fi

# ---------------------------------------------------------------------------
# DEMO HYGIENE (normal-FS / demo profile only): the Claude TMPDIR handling that
# used to live in provision/personal.yml. Ported here so the ONE bootstrap fully
# provisions the demo box too. Guarded to the demo profile: on clusters $HOME is
# on a network FS and node-local scratch is the right temp home, so a ~/.tmp
# cron here would be wrong — skipped. Every step below converges on re-run.
# ---------------------------------------------------------------------------
if [ "$PERSONAL_TARGET" = "demo" ]; then
  step "demo hygiene: Claude TMPDIR (dir + settings.json merge + cron + CLAUDE.md)"

  # Claude's Bash tool silently breaks when /tmp fills on the box; give it a
  # per-user temp dir with a retention cron and tell Claude to use it.
  CLAUDE_HOME="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
  TMPDIR_DIR="$HOME/.tmp"
  mkdir -p "$CLAUDE_HOME" "$TMPDIR_DIR"

  # Merge env.TMPDIR into settings.json WITHOUT clobbering bootstrap-claude's
  # permissions/allow list (read existing JSON, deep-merge, write back).
  SETTINGS_JSON="$CLAUDE_HOME/settings.json"
  if command -v python3 >/dev/null 2>&1; then
    TMPDIR_DIR="$TMPDIR_DIR" SETTINGS_JSON="$SETTINGS_JSON" python3 - <<'PY' || warn "settings.json TMPDIR merge failed"
import json, os, pathlib
p = pathlib.Path(os.environ["SETTINGS_JSON"])
s = json.loads(p.read_text() or "{}") if p.exists() else {}
env = s.setdefault("env", {})
env["TMPDIR"] = os.environ["TMPDIR_DIR"]
p.parent.mkdir(parents=True, exist_ok=True)
p.write_text(json.dumps(s, indent=2) + "\n")
print(f"    set env.TMPDIR={env['TMPDIR']} in {p}")
PY
  else
    warn "python3 not found — skipped settings.json TMPDIR merge"
  fi

  # Daily cleanup cron (7-day retention, 03:00). Idempotent: replace our own
  # marked line rather than appending duplicates.
  if command -v crontab >/dev/null 2>&1; then
    CRON_MARK="# cgeng: cleanup Claude TMPDIR"
    CRON_JOB="0 3 * * * find $TMPDIR_DIR -mindepth 1 -mtime +7 -delete  $CRON_MARK"
    # shellcheck disable=SC2015  # echo effectively never fails; warn is a fallback
    { crontab -l 2>/dev/null | grep -vF "$CRON_MARK"; echo "$CRON_JOB"; } \
      | crontab - && echo "    installed Claude TMPDIR cleanup cron" \
      || warn "could not install TMPDIR cleanup cron"
  else
    warn "crontab not found — skipped TMPDIR cleanup cron"
  fi

  # Tell Claude to use $TMPDIR (managed marker block, replaced not duplicated).
  CLAUDE_MD="$CLAUDE_HOME/CLAUDE.md"
  MARK_BEGIN="<!-- BEGIN cgeng tmpdir -->"
  MARK_END="<!-- END cgeng tmpdir -->"
  touch "$CLAUDE_MD"
  tmp_md="$(mktemp)"
  awk -v b="$MARK_BEGIN" -v e="$MARK_END" '
    $0 == b { skip = 1; next } $0 == e { skip = 0; next } !skip { print }
  ' "$CLAUDE_MD" > "$tmp_md"
  {
    cat "$tmp_md"
    printf '%s\n' "$MARK_BEGIN"
    printf '## Filesystem / shell\n'
    # shellcheck disable=SC2016  # literal $TMPDIR is intended in the guidance text
    printf -- '- Use `$TMPDIR` (or `mktemp`) for temporary files, never hardcode\n'
    # shellcheck disable=SC2016
    printf '  `/tmp/...`. `/tmp` on this box fills up and silently breaks the Bash\n'
    # shellcheck disable=SC2016
    printf '  tool (commands exit 1 with no output). `$TMPDIR` is a per-user dir\n'
    printf '  with a daily cleanup cron.\n'
    printf '%s\n' "$MARK_END"
  } > "$CLAUDE_MD"
  rm -f "$tmp_md"
  echo "    wrote TMPDIR guidance block to $CLAUDE_MD"

  # Web-dev formatters/linters for Doom's :lang web (stylelint, js-beautify),
  # installed into the nvm-managed Node prefix. Best-effort: skipped if nvm/npm
  # are absent, and never fatal.
  if [ -s "$HOME/.nvm/nvm.sh" ]; then
    # shellcheck disable=SC1091  # nvm is user-provided at runtime
    if . "$HOME/.nvm/nvm.sh" >/dev/null 2>&1 && command -v npm >/dev/null 2>&1; then
      if command -v js-beautify >/dev/null 2>&1 && command -v stylelint >/dev/null 2>&1; then
        echo "    web formatters already present (stylelint, js-beautify)"
      else
        # shellcheck disable=SC2015  # echo effectively never fails; warn is a fallback
        npm install -g stylelint js-beautify \
          && echo "    installed npm globals: stylelint js-beautify" \
          || warn "npm install of web formatters failed"
      fi
    else
      warn "nvm present but npm unavailable — skipped web formatters"
    fi
  else
    echo "    no nvm — skipped web formatters (Doom :lang web)"
  fi
fi

echo; echo "== personal-bootstrap: done (WORK=$WORK, target=$PERSONAL_TARGET) =="
