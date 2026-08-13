# provision/Makefile — one word per intent for the personal-layer deploys.
#
# The mental model: the LAPTOP (control node) renders secrets and pushes its
# own checkouts; targets converge idempotently. See README.md in this dir.
#
#   make local        converge this laptop (tools + full dotfiles stow)
#   make secrets      render all secret files here from gopass
#   make secrets-yaml render the ec2-instance-setup secrets.yaml from gopass
#   make p5           push + converge the iva-p5 login node
#   make p5-secrets   render + ship the secret files to iva-p5
#   make demo         push + converge a demo box   (HOST=iva-demo-<user> required)
#   make restack      move cgeng/agents back onto the head of the ec2 MR stack
#
# Everything is safe to re-run. Remote pushes rsync the control node's
# checkouts — the control node is the single source of truth; target-side
# `git status` is informational only.

HERE := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
HOST ?=

.PHONY: help local secrets secrets-yaml p5 p5-secrets demo restack

help:
	@sed -n 's/^#   //p' $(abspath $(lastword $(MAKEFILE_LIST)))

local:
	PERSONAL_TARGET=local $(HERE)personal-bootstrap.sh $(HOME)/work

secrets:
	$(HERE)render-secrets.sh

# The ec2-instance-setup playbook consumes a per-user secrets.yaml contract
# instead of reading this machine's rendered dotfiles. This fills that contract
# from gopass; teammates hand-edit their own copy from secrets.yaml.example.
secrets-yaml:
	$(HERE)render-secrets-yaml.sh

p5:
	$(HERE)push-personal.sh iva-p5

p5-secrets:
	$(HERE)deploy-secrets-iva-p5.sh

# PERSONAL_TARGET=demo is REQUIRED, not optional. The runner auto-detects the
# profile from the filesystem, and the demo box has /opt/dlami/nvme, which the
# detection reads as a cluster — so without this, `make demo` silently produced
# target=cluster and skipped the whole demo-hygiene block (Claude TMPDIR dir,
# settings.json env.TMPDIR, the cleanup cron, the CLAUDE.md guidance, the web
# formatters). personal.yml already forces it for the POST_HOOK path; this makes
# the two paths agree.
demo:
	@test -n "$(HOST)" || { echo "usage: make demo HOST=iva-demo-test-<user>"; exit 1; }
	PERSONAL_TARGET=demo $(HERE)push-personal.sh $(HOST) project/cgeng/work

# cgeng/agents holds the AI assistant files that must never reach main. It sits
# one commit above demo-instance-setup and does NOT follow it: nothing restacks
# a child branch when its parent is rewritten. Run this after any force-push to
# demo-instance-setup. Safe to re-run; a no-op when already at the head.
restack:
	$(HERE)restack-agents.sh
