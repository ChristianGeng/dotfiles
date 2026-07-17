# Org → GitLab Markdown Copy/Paste Helpers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Two Doom Emacs commands that convert org(-roam) content to GitLab-flavored markdown — one copies region/subtree to the clipboard, one converts the clipboard on yank into a markdown buffer.

**Architecture:** A derived org-export backend (parent: `gfm`, already installed and tuned in this config) overrides the `link` transcoder (drop `id:`/`roam:` links to plain description text) and adds a `:filter-parse-tree` that demotes headings so the highest exported heading is H2. A shared string-export core feeds two thin interactive commands. Everything lives in one auto-loaded defuns file.

**Tech Stack:** Emacs Lisp, org-export (`ox`), `ox-gfm` (from Doom's straight build at `~/doom-emacs/.local/straight/build-*/ox-gfm`), `ert` for batch tests, Doom `map!`/`after!` for keybindings.

**Spec:** `docs/superpowers/specs/2026-07-17-org-gitlab-markdown-design.md`

## Global Constraints

- Implementation file: `doom/.config/doom/defuns/org-gitlab-defuns.el` (auto-loaded by `config.el`'s defuns loop — no wiring).
- Function prefix: `cg/org-gitlab--` for internals, `cg/org-` for interactive commands (matches existing defuns naming).
- No external binaries (no pandoc). No new packages — `ox-gfm` is already in `packages.el`.
- Export options: `body-only`, no TOC, no author/date/timestamps, TODO keywords stripped, tags stripped, ATX headings up to H6 (`:headline-levels 6` — otherwise ox-md turns demoted H4+ into list items).
- `id:`/`roam:` links: emit description as plain text; description-less links emit `""`; must never error.
- Keybindings: org-mode localleader `y` → copy command; markdown-mode localleader `y` → yank command; fall back to `G` only if `y` is already bound in that map.
- Tests run in batch Emacs (not Doom), so the test file stubs `map!`/`after!` before loading the defuns file.

---

### Task 1: Export core — backend, link handling, heading demotion

**Files:**
- Create: `doom/.config/doom/defuns/org-gitlab-defuns.el`
- Test: `doom/.config/doom/tests/org-gitlab-defuns-test.el`

**Interfaces:**
- Produces: `cg/org-gitlab--export-string (org-text)` → returns GitLab-markdown string. Also `cg/org-gitlab--backend` (export backend struct). Task 2 calls only `cg/org-gitlab--export-string`.

- [ ] **Step 1: Write the failing tests**

Create `doom/.config/doom/tests/org-gitlab-defuns-test.el`:

```elisp
;;; org-gitlab-defuns-test.el --- Batch ert tests for org-gitlab-defuns  -*- lexical-binding: t; -*-

;; Run outside Doom: stub Doom macros before loading the defuns file.
(unless (fboundp 'map!) (defmacro map! (&rest _args) nil))
(unless (fboundp 'after!) (defmacro after! (_pkg &rest _body) nil))

(require 'ert)
(require 'ox-gfm)
(load (expand-file-name "../defuns/org-gitlab-defuns.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest cg/org-gitlab-id-link-becomes-plain-text ()
  (should (string-match-p
           "^See My Note for details"
           (cg/org-gitlab--export-string
            "See [[id:abc-123][My Note]] for details"))))

(ert-deftest cg/org-gitlab-bare-id-link-emits-nothing ()
  (let ((md (cg/org-gitlab--export-string "before [[id:abc-123]] after")))
    (should-not (string-match-p "abc-123" md))
    (should (string-match-p "before  after" md))))

(ert-deftest cg/org-gitlab-https-links-survive ()
  (should (string-match-p
           "\\[GitLab\\](https://gitlab\\.com)"
           (cg/org-gitlab--export-string "[[https://gitlab.com][GitLab]]"))))

(ert-deftest cg/org-gitlab-headings-demote-to-h2 ()
  (let ((md (cg/org-gitlab--export-string "* Top\nbody\n** Sub\nmore")))
    (should (string-match-p "^## Top" md))
    (should (string-match-p "^### Sub" md))))

(ert-deftest cg/org-gitlab-deep-headings-stay-atx ()
  ;; 5 levels deep: top demotes to H2, deepest to H6 — must stay a heading,
  ;; not degrade into a list item.
  (let ((md (cg/org-gitlab--export-string
             "* A\n** B\n*** C\n**** D\n***** E")))
    (should (string-match-p "^###### E" md))))

(ert-deftest cg/org-gitlab-todo-keywords-stripped ()
  (let ((md (cg/org-gitlab--export-string "* TODO Fix the thing")))
    (should (string-match-p "^## Fix the thing" md))
    (should-not (string-match-p "TODO" md))))

(ert-deftest cg/org-gitlab-tags-stripped ()
  (should-not (string-match-p
               ":work:"
               (cg/org-gitlab--export-string "* Heading :work:"))))

(ert-deftest cg/org-gitlab-code-block-fenced-with-language ()
  (let ((md (cg/org-gitlab--export-string
             "#+begin_src python\nprint(1)\n#+end_src")))
    (should (string-match-p "```python" md))
    (should (string-match-p "print(1)" md))))

(ert-deftest cg/org-gitlab-no-headings-input-ok ()
  (should (string-match-p
           "just \\*\\*bold\\*\\* text"
           (cg/org-gitlab--export-string "just *bold* text"))))

(provide 'org-gitlab-defuns-test)
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
cd /home/cgeng_local/work/myfiles/dotfiles
emacs --batch \
  -L "$(ls -d ~/doom-emacs/.local/straight/build-*/ox-gfm | head -1)" \
  -l doom/.config/doom/tests/org-gitlab-defuns-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: FAIL — load error, `doom/.config/doom/defuns/org-gitlab-defuns.el` does not exist yet.

- [ ] **Step 3: Write the implementation**

Create `doom/.config/doom/defuns/org-gitlab-defuns.el`:

```elisp
;;; org-gitlab-defuns.el --- Copy/paste org as GitLab markdown  -*- lexical-binding: t; -*-

;; Converts org(-roam) content to GitLab-flavored markdown via a backend
;; derived from ox-gfm. Two entry points:
;;   cg/org-copy-as-gitlab-markdown  — org buffer → clipboard (paste in browser)
;;   cg/org-yank-as-gitlab-markdown  — clipboard org → markdown buffer
;; Spec: docs/superpowers/specs/2026-07-17-org-gitlab-markdown-design.md

(require 'ox)

(defun cg/org-gitlab--link (link contents info)
  "Transcode LINK: id/roam links become plain description text."
  (if (member (org-element-property :type link) '("id" "roam"))
      (or contents "")
    (org-export-with-backend 'gfm link contents info)))

(defun cg/org-gitlab--demote-headings (tree _backend info)
  "Demote headlines in TREE so the highest level becomes 2."
  (let ((min-level nil))
    (org-element-map tree 'headline
      (lambda (hl)
        (let ((level (org-element-property :level hl)))
          (when (or (null min-level) (< level min-level))
            (setq min-level level))))
      info)
    (when min-level
      (let ((delta (- 2 min-level)))
        (unless (zerop delta)
          (org-element-map tree 'headline
            (lambda (hl)
              (org-element-put-property
               hl :level (+ (org-element-property :level hl) delta)))
            info)))))
  tree)

(defvar cg/org-gitlab--backend
  (progn
    (require 'ox-gfm)
    (org-export-create-backend
     :name 'gitlab
     :parent 'gfm
     :transcoders '((link . cg/org-gitlab--link))
     :filters '((:filter-parse-tree . cg/org-gitlab--demote-headings))))
  "GFM-derived export backend for GitLab issues/MRs.")

(defun cg/org-gitlab--export-string (org-text)
  "Export ORG-TEXT (a string of org markup) to GitLab markdown."
  (org-export-string-as
   org-text cg/org-gitlab--backend t
   '(:with-toc nil :with-todo-keywords nil :with-tags nil
     :with-author nil :with-date nil :with-timestamps nil
     :headline-levels 6)))
```

(Keybindings and interactive commands come in Task 2.)

- [ ] **Step 4: Run tests to verify they pass**

Same command as Step 2. Expected: `Ran 9 tests, 9 results as expected`.

If `cg/org-gitlab-deep-headings-stay-atx` fails with `E` rendered as a list
item, the `:headline-levels 6` option is not reaching the exporter — check
the ext-plist argument position in `org-export-string-as`.

- [ ] **Step 5: Commit**

```bash
cd /home/cgeng_local/work/myfiles/dotfiles
git add doom/.config/doom/defuns/org-gitlab-defuns.el doom/.config/doom/tests/org-gitlab-defuns-test.el
git commit -m "doom: add org → GitLab markdown export core (ox-gfm derived backend)"
```

---

### Task 2: Interactive commands + keybindings

**Files:**
- Modify: `doom/.config/doom/defuns/org-gitlab-defuns.el` (append)
- Test: `doom/.config/doom/tests/org-gitlab-defuns-test.el` (append)

**Interfaces:**
- Consumes: `cg/org-gitlab--export-string (org-text)` from Task 1.
- Produces: `cg/org-copy-as-gitlab-markdown` (interactive; region → kill ring, else subtree at point), `cg/org-yank-as-gitlab-markdown` (interactive; converts `(current-kill 0)` and inserts).

- [ ] **Step 1: Write the failing tests**

Append to `doom/.config/doom/tests/org-gitlab-defuns-test.el` (before the `provide` line):

```elisp
(ert-deftest cg/org-gitlab-copy-subtree-at-point ()
  (with-temp-buffer
    (org-mode)
    (insert "* One\nalpha\n* TODO Two\n[[id:xyz][Ref]]\nbeta\n")
    (goto-char (point-min))
    (forward-line 3)                    ; inside subtree "Two"
    (cg/org-copy-as-gitlab-markdown)
    (let ((md (current-kill 0)))
      (should (string-match-p "^## Two" md))
      (should (string-match-p "Ref" md))
      (should-not (string-match-p "TODO" md))
      (should-not (string-match-p "One" md)))))

(ert-deftest cg/org-gitlab-copy-region-wins-over-subtree ()
  (with-temp-buffer
    (org-mode)
    (insert "* One\nalpha\n* Two\nbeta\n")
    (set-mark (point-min))
    (goto-char (point-min))
    (forward-line 2)                    ; region = subtree "One" only
    (activate-mark)
    (let ((transient-mark-mode t))
      (cg/org-copy-as-gitlab-markdown))
    (let ((md (current-kill 0)))
      (should (string-match-p "^## One" md))
      (should-not (string-match-p "Two" md)))))

(ert-deftest cg/org-gitlab-copy-before-first-heading-errors ()
  (with-temp-buffer
    (org-mode)
    (insert "preamble\n* One\n")
    (goto-char (point-min))
    (should-error (cg/org-copy-as-gitlab-markdown) :type 'user-error)))

(ert-deftest cg/org-gitlab-copy-outside-org-errors ()
  (with-temp-buffer
    (fundamental-mode)
    (should-error (cg/org-copy-as-gitlab-markdown) :type 'user-error)))

(ert-deftest cg/org-gitlab-yank-converts-clipboard-org ()
  (with-temp-buffer
    (kill-new "* Note\nsee [[id:abc][That Page]]")
    (cg/org-yank-as-gitlab-markdown)
    (should (string-match-p "^## Note" (buffer-string)))
    (should (string-match-p "That Page" (buffer-string)))
    (should-not (string-match-p "id:abc" (buffer-string)))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Same command as Task 1 Step 2. Expected: the 9 Task-1 tests pass; the 5 new tests FAIL with `void-function cg/org-copy-as-gitlab-markdown` / `cg/org-yank-as-gitlab-markdown`.

- [ ] **Step 3: Write the implementation**

Append to `doom/.config/doom/defuns/org-gitlab-defuns.el`:

```elisp
(defun cg/org-copy-as-gitlab-markdown ()
  "Copy region (or subtree at point) as GitLab-flavored markdown."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((org-text
          (cond
           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           ((org-before-first-heading-p)
            (user-error "No region active and point is before the first heading"))
           (t (save-excursion
                (org-back-to-heading t)
                (let ((beg (point)))
                  (org-end-of-subtree t t)
                  (buffer-substring-no-properties beg (point)))))))
         (md (cg/org-gitlab--export-string org-text)))
    (kill-new md)
    (message "Copied %d chars as GitLab markdown" (length md))))

(defun cg/org-yank-as-gitlab-markdown ()
  "Insert clipboard/kill-ring org content converted to GitLab markdown."
  (interactive)
  (insert (cg/org-gitlab--export-string (current-kill 0))))

(after! org
  (map! :map org-mode-map
        :localleader
        :desc "Copy as GitLab markdown" "y" #'cg/org-copy-as-gitlab-markdown))

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :desc "Yank org as GitLab markdown" "y" #'cg/org-yank-as-gitlab-markdown))
```

Note: the test file's `map!`/`after!` stubs make these forms no-ops in batch;
in Doom they bind normally.

- [ ] **Step 4: Run tests to verify all pass**

Same command as Task 1 Step 2. Expected: `Ran 14 tests, 14 results as expected`.

- [ ] **Step 5: Commit**

```bash
cd /home/cgeng_local/work/myfiles/dotfiles
git add doom/.config/doom/defuns/org-gitlab-defuns.el doom/.config/doom/tests/org-gitlab-defuns-test.el
git commit -m "doom: add copy/yank commands + localleader bindings for GitLab markdown"
```

---

### Task 3: Live verification in Doom

**Files:** none (verification only)

**Interfaces:**
- Consumes: both interactive commands from Task 2.

- [ ] **Step 1: Reload Doom config**

In the running Emacs: `M-x doom/reload` (or restart Emacs). Confirm no load
errors in `*Messages*` and that `defuns/org-gitlab-defuns.el` loaded.

- [ ] **Step 2: Check the keybinding landed (collision check)**

In an org buffer: `SPC m y` should show "Copy as GitLab markdown" in
which-key. If `y` was already bound by Doom's org module, rebind both maps to
`G` per the spec fallback and re-commit.

- [ ] **Step 3: End-to-end copy test**

Open a real org-roam note containing an `id:` link, a nested heading, and a
code block. `SPC m y` on a subtree, then paste into a GitLab issue comment
and hit Preview. Verify: headings start at `##`, id-links show as plain
text, code block is fenced with its language.

- [ ] **Step 4: End-to-end yank test**

Copy raw org text (plain `M-w`), open a markdown buffer, `SPC m y` — the
inserted text must be converted markdown, not org.

- [ ] **Step 5: Verify OSC 52 path (SSH/terminal)**

In a terminal Emacs session (clipetty active), run the copy command and
paste into the local browser. Confirm the markdown arrives.
