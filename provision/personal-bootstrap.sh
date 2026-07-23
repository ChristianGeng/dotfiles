#!/usr/bin/env bash
# personal-bootstrap.sh — REMOTE-SIDE runner for the control-node PUSH model.
#
# Runs ON the target (managed clusters like iva-p4d, iva-p5, or the demo box)
# and assumes the repositories are ALREADY present under $WORK — the control
# node rsyncs them here first (see provision/push-personal.sh). This script does
# NOT clone myfiles or cgeng-ai-skills: that is exactly what fails on managed
# clusters, where the target has no working GitLab token and non-interactive git
# cannot prompt. The only thing still cloned is the public doomemacs framework.
#
# Sets up, all under $HOME with no sudo:
#   - Doom Emacs   (chemacs `emacs` package + `doom` config package + framework)
#   - Claude Code  (cgeng-ai-skills skills + MCP servers)
# and installs the user-level tools they need (uv, ruff, claude) into ~/.local.
#
# Deliberately narrow footprint on a shared box: it stows ONLY the `emacs` and
# `doom` dotfiles packages (NOT bash/git/tmux/etc. — your shell config is left
# alone), and there is NO tunnel/secrets layer (that is demo-box only).
#
# Requirements on the target (true on the managed clusters):
#   - base tools: git, stow, curl
#   - the repos already staged under $WORK (myfiles/dotfiles, cgeng/cgeng-ai-skills)
#   - an emacs on PATH for `doom sync` (if conda/module-provided and absent from
#     a non-interactive PATH, this script probes common miniconda locations)
#
# Usage:
#   ./personal-bootstrap.sh                       # WORK defaults to ~/work (clusters)
#   ./personal-bootstrap.sh project/cgeng/work    # positional WORK (relative to $HOME? no — see below)
#   WORK=/scratch/me ./personal-bootstrap.sh      # or via the environment
#   CLAUDE_CONFIG_DIR=~/.claude-work ./personal-bootstrap.sh   # target a specific claude config
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

step "user-level tools -> ~/.local (install or upgrade; harmless if present)"
if command -v uv >/dev/null 2>&1; then uv self update || warn "uv self update failed"
else curl -LsSf https://astral.sh/uv/install.sh | sh || warn "uv install failed"; fi
command -v ruff >/dev/null 2>&1 || curl -LsSf https://astral.sh/ruff/install.sh | sh || warn "ruff install failed"
if command -v claude >/dev/null 2>&1; then claude update || warn "claude update failed"
else curl -fsSL https://claude.ai/install.sh | bash || warn "claude install failed"; fi
hash -r

step "dotfiles: stow ONLY the emacs + doom packages (repos already staged)"
if [ ! -d "$MYFILES" ]; then
  warn "no dotfiles at $MYFILES (did the control node rsync run?); skipped dotfiles"
elif command -v stow >/dev/null 2>&1 && [ -d "$MYFILES/emacs" ]; then
  # Back up any pre-existing REAL files that would block stow (chemacs seeds
  # ~/.emacs.d/*.el), then stow just these two packages into $HOME.
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

step "Doom Emacs: framework (clone if absent) + non-interactive sync"
if [ ! -d "$HOME/doom-emacs/.git" ]; then
  echo "  clone $DOOM_URL"
  git clone "$DOOM_URL" "$HOME/doom-emacs" || warn "doomemacs clone failed (continuing)"
else
  echo "  exists (leaving as-is): $HOME/doom-emacs"
fi
if command -v emacs >/dev/null 2>&1 && [ -x "$HOME/doom-emacs/bin/doom" ]; then
  export DOOMDIR="${DOOMDIR:-$HOME/.config/doom}"
  if [ ! -d "$HOME/doom-emacs/.local" ]; then
    "$HOME/doom-emacs/bin/doom" install --force --no-env --no-config || warn "doom install failed"
  fi
  # `doom sync` can hit interactive straight.el prompts (e.g. "straight.el HEAD
  # on master is behind develop — Checkout develop? 1/2"). No TTY here, so feed
  # "2" (the recommended answer) to every prompt.
  yes 2 | "$HOME/doom-emacs/bin/doom" sync || warn "doom sync failed"
  printf 'doom' > "$HOME/.emacs-profile"   # make Doom the default chemacs profile
else
  warn "no emacs on PATH — skipped Doom sync (load your emacs module, then re-run)"
fi

step "Claude Code: skills + MCP servers (cgeng-ai-skills bootstrap)"
if [ -x "$SKILLS/scripts/bootstrap-claude.sh" ]; then
  "$SKILLS/scripts/bootstrap-claude.sh" || warn "bootstrap-claude failed"
else
  warn "bootstrap-claude.sh not found in $SKILLS; skipped Claude setup"
fi

echo; echo "== personal-bootstrap: done (WORK=$WORK) =="
