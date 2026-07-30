# provision/ — the personal layer, in one page

One sentence to remember: **the laptop renders secrets and pushes its own
checkouts; every target converges idempotently.** The laptop (control node) is
the single source of truth — it holds the GPG key and the three gopass stores,
and no target ever gets credentials to fetch anything itself.

## The layers

| Layer | Owner | Lives in | You touch it via |
|---|---|---|---|
| Cluster infra (iva-p5 nodes, Slurm, `/etc/profile.d/iva-envs.sh`) | IVA team | [sagemaker-hyperpod-setup](https://gitlab.audeering.com/research/iva/aws/sagemaker-hyperpod-setup) | issues/MRs only — never directly |
| Demo-box infra (Terraform, generic playbook, roll-up/down cron) | shared, operator-neutral | [ec2-instance-setup](https://gitlab.audeering.com/research/iva/aws/ec2-instance-setup) | `roll.env` hooks, MRs |
| Personal layer — secrets, dotfiles, tools, Emacs, Claude | you | this repo (+ audeering-dotfiles for work configs) | **this directory** |

Machine-specific behavior is expressed as *runtime gates inside shared files*
(e.g. the iva-p5 Emacs block in `bash/.bashrc` gates on `$HOME` under `/fsx`
with no worker NVMe), never as per-machine config forks.

## One command per intent

```
make local        # converge this laptop: tools (uv/ruff/claude) + full stow
make secrets      # render ~/.local_configs, ~/.audeering_configs, ~/.aws/* from gopass
make p5           # rsync checkouts to iva-p5 + run personal-bootstrap there
make p5-secrets   # render + ship the secret files to iva-p5 (0600, no stores on target)
make demo HOST=iva-demo-<user>   # same push for a demo box
```

The demo box normally needs none of this by hand: the morning roll-up cron
renders secrets, deploys, and runs the personal layer as its `POST_HOOK`.

## What each target profile does

`personal-bootstrap.sh` auto-detects its profile (override: `PERSONAL_TARGET=`):

| | `local` (laptop) | `demo` (EC2 box) | `cluster` (iva-p5) |
|---|---|---|---|
| detected by | gopass stores present | normal disk | network FS / NVMe |
| tools → `~/.local` | yes | yes | yes |
| dotfiles | **full** `stow-deploy.sh` | emacs+doom narrow (full via POST_HOOK) | emacs+doom narrow only |
| Doom Emacs | **skipped** (hand-managed) | clone + sync in place | staged to node-local scratch |
| Claude bootstrap | only with explicit `CLAUDE_CONFIG_DIR` (laptop uses split configs; `~/.claude` unused) | yes | yes |
| demo TMPDIR hygiene | no | yes | no |

## Secrets flow (all targets)

gopass (laptop, GPG) → `render-secrets.sh` → 0600 files → copied to targets
(ansible task on the demo box, `deploy-secrets-iva-p5.sh` for p5). Targets never
hold gopass, stores, or the GPG key. Rotating a credential = update it in
gopass, re-run `make secrets` + the target's deploy.

## Gotchas worth knowing

- Pushes rsync **into real git checkouts** on the targets, so target-side
  `git status` is decorative. Never `git checkout --` files there — some carry
  intentional local lines (e.g. secrets sourcing in the p5 `.bashrc`).
- `personal-bootstrap.sh` is `set -uo pipefail` *without* `-e` by design:
  optional steps warn and continue; the deploy always converges.
- The laptop can never be misdetected as a demo box: the gopass-store check
  wins before the filesystem heuristic.
