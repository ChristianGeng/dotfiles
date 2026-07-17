# Org → GitLab Markdown copy/paste helpers — Design

**Date:** 2026-07-17
**Repo:** myfiles/dotfiles
**Status:** Approved (approach A)

## Problem

Material from org-roam notes is regularly copy-pasted by hand into GitLab
issues and MR descriptions. Raw org syntax pasted into GitLab renders badly;
manual reformatting to GitLab-flavored markdown is friction. The existing
ox-gfm setup (already installed and tuned in `config.el` — `org-gfm-headline`
is advised to suppress `<a id="orgXXX">` anchors) does most of the conversion
but leaves two problems:

1. Org-roam `[[id:...][Description]]` links export as dead links.
2. Top-level org headings export as `#` (H1), which renders oversized in
   issue comments.

Pasting happens in two places, so both directions are needed:

- **Browser (GitLab web UI):** conversion must happen at copy time in Emacs —
  the browser only receives whatever is on the clipboard.
- **Emacs markdown buffers** (e.g. daitools description-file workflow):
  conversion can happen at yank time.

## Chosen approach

A derived org-export backend on top of the existing `gfm` backend, plus two
thin interactive commands sharing it. Pure elisp, no external dependencies
(pandoc rejected: extra binary per machine, ignores existing ox-gfm tuning,
divergent org parser).

## Location

`doom/.config/doom/defuns/org-gitlab-defuns.el` — all `defuns/*.el` files are
auto-loaded by `config.el`, so no wiring is needed.

## Components

1. **`cg/org-gitlab--backend`** — created with `org-export-create-backend`,
   `:parent 'gfm`, overriding:
   - **Link transcoder:** for link type `id` (and `roam:` if encountered),
     emit only the link description as plain text; links with no description
     emit nothing but must not error. All other link types defer to the gfm
     transcoder.
   - **`:filter-parse-tree`:** find the minimum headline level in the
     fragment being exported and demote all headlines uniformly so the
     highest level becomes 2 (`##`). Relative structure is preserved.
2. **`cg/org-gitlab--export-string (org-text)`** — the shared core. Exports
   the given string through the backend with `body-only`, no TOC, no
   author/date/timestamps, and `:with-todo-keywords nil` (TODO/DONE keywords
   are stripped — issue text should not carry them).
3. **`cg/org-copy-as-gitlab-markdown`** (interactive, org-mode) — exports the
   active region, or the subtree at point when no region is active, and puts
   the markdown on the kill ring via `kill-new`. Existing clipboard plumbing
   (`select-enable-clipboard`, clipetty OSC 52 in terminals/SSH) propagates
   it to the system clipboard. Reports `"Copied N chars as GitLab markdown"`.
4. **`cg/org-yank-as-gitlab-markdown`** (interactive, markdown buffers) —
   takes `(current-kill 0)`, runs it through the core, inserts the result at
   point.

## Keybindings

Following the existing localleader-prefix pattern in `config.el`:

- org-mode localleader: `y` → `cg/org-copy-as-gitlab-markdown`
  (desc "Copy as GitLab markdown")
- markdown-mode localleader: `y` → `cg/org-yank-as-gitlab-markdown`
  (desc "Yank org as GitLab markdown")

If `y` collides with an existing binding in either map, fall back to `G`.

## Error handling

- Copy command with no region and point before the first heading: signal
  `user-error` with a clear message.
- Export failures surface org-export's own error; nothing is silently copied.
- Yank command on clipboard content org can't meaningfully parse: org-mode
  parses nearly any text, so the result is inserted as-is-converted; no
  special casing.

## Testing

Manual, via a scratch org buffer exercising: `id:` links (with and without
descriptions), nested headings (verify demotion to `##` base), a code block
with a language, a table, a task list, and TODO-keyword headings (verify
stripping). Verify the copy command's output pastes correctly into a GitLab
comment preview, and the yank command round-trips the same content into a
markdown buffer. Also verify OSC 52 copy works over an SSH session.

## Out of scope

- Resolving `id:` links to public URLs (org-roam notes have no GitLab-visible
  location).
- Automatic clipboard-format detection or paste hooks in the browser.
- GitLab-specific extensions beyond GFM (mermaid, math) — org source rarely
  uses them; ox-gfm passes fenced blocks through unchanged anyway.
