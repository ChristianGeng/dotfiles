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
#
# Everything is safe to re-run. Remote pushes rsync the control node's
# checkouts — the control node is the single source of truth; target-side
# `git status` is informational only.

HERE := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
HOST ?=

.PHONY: help local secrets secrets-yaml p5 p5-secrets demo

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

demo:
	@test -n "$(HOST)" || { echo "usage: make demo HOST=iva-demo-<user>"; exit 1; }
	$(HERE)push-personal.sh $(HOST) project/cgeng/work
