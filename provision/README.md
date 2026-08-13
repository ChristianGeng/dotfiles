# provision/ — the personal layer, in one page

One sentence to remember: **the laptop renders secrets and pushes its own
checkouts; every target converges idempotently.** The laptop (control node) is
the single source of truth — it holds the GPG key and the three gopass stores,
and no target ever gets credentials to fetch anything itself.

## Set up my demo box, start to finish

```bash
# 1. render secrets from gopass, on the laptop
cd ~/work/myfiles/dotfiles/provision
make secrets          # -> ~/.local_configs, ~/.audeering_configs, ~/.aws/*, minio.cfg
make secrets-yaml     # -> ec2-instance-setup/secrets.yaml (what the playbook reads)

# 2. build the generic box (Terraform + the shared playbook)
cd ~/work/research/research/iva/aws/ec2-instance-setup
terraform plan && terraform apply          # ~13 min; ends "Apply complete!"

# 3. layer MY stuff on top — apply does NOT do this (see Gotchas)
cd ~/work/myfiles/dotfiles/provision
make demo HOST=iva-demo-test-cgeng         # tools, dotfiles, Doom, Claude, repo clone

# 4. use it
ssh iva-demo-test-cgeng

# 5. tear it down — this is a paid g6.2xlarge
cd ~/work/research/research/iva/aws/ec2-instance-setup && terraform destroy
```

**Re-running pieces** without rebuilding the box:

```bash
make playbook ARGS="--tags secrets"    # in the ec2 repo: re-push credentials only
make playbook ARGS="--tags tunnel"     # refresh the scripts staged in ~/tunnel-install
make demo HOST=iva-demo-test-cgeng     # re-converge the whole personal layer
```

**Check it worked**, on the box:

```bash
bash -lc 'echo ${#GITLAB_TOKEN} ${#BRAVE_API_KEY}'   # non-zero: secrets.env is sourced
bash -lc 'command -v doom node stylelint'            # all three resolve
claude mcp list                                      # 4 servers, no warnings
ls ~/.tmp ~/.claude/settings.json                    # demo hygiene applied
```

The ssh alias is `iva-demo-test-<user>` — note the `-test-`.

## The layers

| Layer | Owner | Lives in | You touch it via |
|---|---|---|---|
| Cluster infra (iva-p5 nodes, Slurm, `/etc/profile.d/iva-envs.sh`) | IVA team | [sagemaker-hyperpod-setup](https://gitlab.audeering.com/research/iva/aws/sagemaker-hyperpod-setup) | issues/MRs only — never directly |
| Demo-box infra (Terraform, generic playbook) | shared, operator-neutral | [ec2-instance-setup](https://gitlab.audeering.com/research/iva/aws/ec2-instance-setup) | `secrets.yaml`, `roll.env` hooks, MRs |
| Personal layer — secrets, dotfiles, tools, Emacs, Claude, repo clone | you | this repo (+ audeering-dotfiles for work configs) | **this directory** |

Machine-specific behavior is expressed as *runtime gates inside shared files*
(e.g. the iva-p5 Emacs block in `bash/.bashrc` gates on `$HOME` under `/fsx`
with no worker NVMe), never as per-machine config forks.

## One command per intent

```
make local        # converge this laptop: tools (uv/ruff/claude) + full stow
make secrets      # render ~/.local_configs, ~/.audeering_configs, ~/.aws/* from gopass
make secrets-yaml # render ec2-instance-setup/secrets.yaml from gopass
make p5           # rsync checkouts to iva-p5 + run personal-bootstrap there
make p5-secrets   # render + ship the secret files to iva-p5 (0600, no stores on target)
make demo HOST=iva-demo-test-<user>   # same push for a demo box
make restack      # move cgeng/agents back onto the head of the ec2 MR stack
```

## What each target profile does

`personal-bootstrap.sh` auto-detects its profile, but **the demo box must be
forced** — see Gotchas. `make demo` and `personal.yml` both set
`PERSONAL_TARGET=demo` for exactly that reason.

| | `local` (laptop) | `demo` (EC2 box) | `cluster` (iva-p5) |
|---|---|---|---|
| detected by | gopass stores present | *nothing* — must be forced | network FS or node-local NVMe |
| tools → `~/.local` | yes | yes | yes |
| dotfiles | **full** `stow-deploy.sh` | emacs+doom narrow | emacs+doom narrow |
| Doom Emacs | **skipped** (hand-managed) | staged to node-local NVMe | staged to node-local NVMe |
| Claude bootstrap | only with explicit `CLAUDE_CONFIG_DIR` (laptop uses split configs; `~/.claude` unused) | yes | yes |
| demo TMPDIR hygiene + web formatters | no | **yes — the only thing the profile gates** | no |
| clone the audEERING repos | no | yes (via `personal.yml`) | no |

## Secrets flow

Two consumers, one source:

```
gopass (laptop, GPG)
  ├─ render-secrets.sh      -> ~/.local_configs, ~/.audeering_configs, ~/.aws/*, minio.cfg
  │                            (iva-p5 via deploy-secrets-iva-p5.sh; demo-box fallback)
  └─ render-secrets-yaml.sh -> ec2-instance-setup/secrets.yaml
                               -> playbook renders on the box:
                                  ~/.config/iva-demo/secrets.env (0600, sourced from
                                  .bashrc AND .profile), ~/.aws/*, minio.cfg
```

Targets never hold gopass, the stores, or the GPG key. Rotating a credential =
update it in gopass, re-run `make secrets` + `make secrets-yaml`, then the
target's deploy (`make playbook ARGS="--tags secrets"` or `make p5-secrets`).

`secrets.yaml` also carries `paths.demo_env`, pointing at the demo `.env.local`.
The ec2 repo has no default for it: it must not presume which demo you run.

## Gotchas worth knowing

- **`terraform apply` does NOT run your personal layer.** The `POST_HOOK` that
  runs it comes from `ec2-instance-setup/scripts/roll.env`, and that file is
  sourced by `roll-up.sh` only — never by a plain `apply`. So step 3 above is
  required, not optional. (The roll-up cron is not currently installed either.)
- **The demo box looks like a cluster to the FS heuristic.** It has
  `/opt/dlami/nvme`, so auto-detection reports `target=cluster` and silently
  skips the demo-hygiene block. Always force `PERSONAL_TARGET=demo`; the
  Makefile and `personal.yml` do. A consequence: Doom is staged to node-local
  NVMe on the demo box too, and that scratch is **wiped on stop/terminate** —
  harmless, since the canonical copy on the home FS re-stages on the next run.
- **`.bashrc` alone is not enough for anything unattended.** Ubuntu's stock
  `.bashrc` returns early for non-interactive shells, so cron, `ssh host cmd`
  and `bash -lc` never reach it. Everything that must survive that goes in
  `.profile` too — `secrets.env`, `doom` on PATH, and nvm (whose own installer
  writes only to `.bashrc`, which is why `node`/`stylelint` were installed but
  unreachable). Scripts that run unattended source what they need directly.
- **`yes 2 | doom sync` reports failure on success.** `yes` takes a SIGPIPE and
  exits 141, and `pipefail` propagates it. Check `PIPESTATUS[1]` — the wrapper
  here does. Both `doom install` and `doom sync` need the `yes 2` guard: without
  it they block forever on a straight.el prompt with no TTY, and hang rather
  than fail.
- Pushes rsync **into real git checkouts** on the targets, so target-side
  `git status` is decorative. Never `git checkout --` files there — some carry
  intentional local lines (e.g. secrets sourcing in the p5 `.bashrc`).
- `personal-bootstrap.sh` is `set -uo pipefail` *without* `-e` by design:
  optional steps warn and continue; the deploy always converges.
- The laptop can never be misdetected as a demo box: the gopass-store check
  wins before the filesystem heuristic.
