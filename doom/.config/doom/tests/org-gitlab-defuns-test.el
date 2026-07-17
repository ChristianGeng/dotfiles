;;; org-gitlab-defuns-test.el --- Batch ert tests for org-gitlab-defuns  -*- lexical-binding: t; -*-

;; Run outside Doom: stub Doom macros before loading the defuns file.
(unless (fboundp 'map!) (defmacro map! (&rest _args) nil))
(unless (fboundp 'after!) (defmacro after! (_pkg &rest _body) nil))

(require 'ert)
(require 'cl-lib)
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
    (should (string-match-p "before after" md))))

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

(ert-deftest cg/org-gitlab-md-to-org-basic ()
  (let ((org (cg/org-gitlab--markdown-to-org
              "## Heading\n\nSee [GitLab](https://gitlab.com) and `code`.")))
    (should (string-match-p "^\\*\\* Heading" org))
    (should (string-match-p "\\[\\[https://gitlab\\.com\\]\\[GitLab\\]\\]" org))
    (should (string-match-p "=code=" org))))

(ert-deftest cg/org-gitlab-md-to-org-strips-custom-id-drawers ()
  (let ((org (cg/org-gitlab--markdown-to-org "## My Section\n\ntext")))
    (should-not (string-match-p ":PROPERTIES:" org))
    (should-not (string-match-p ":CUSTOM_ID:" org))
    (should (string-match-p "^\\*\\* My Section\n\ntext" org))))

(ert-deftest cg/org-gitlab-md-to-org-table ()
  (let ((org (cg/org-gitlab--markdown-to-org
              "| a | b |\n|---|---|\n| 1 | 2 |")))
    (should (string-match-p "^| a | b |" org))
    (should (string-match-p "^| 1 | 2 |" org))))

(ert-deftest cg/org-gitlab-md-to-org-task-checkboxes ()
  (let ((org (cg/org-gitlab--markdown-to-org "- [x] done\n- [ ] open")))
    (should (string-match-p "- \\[X\\] done" org))
    (should (string-match-p "- \\[ \\] open" org))
    (should-not (string-match-p "[☐☒]" org))))

(ert-deftest cg/org-gitlab-md-yank-inserts-org ()
  (with-temp-buffer
    (kill-new "## Note\n\nsome **bold** text")
    (cg/markdown-yank-as-org)
    (should (string-match-p "^\\*\\* Note" (buffer-string)))
    (should (string-match-p "\\*bold\\*" (buffer-string)))))

(ert-deftest cg/org-gitlab-md-to-org-errors-without-pandoc ()
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should-error (cg/org-gitlab--markdown-to-org "text") :type 'user-error)))

(provide 'org-gitlab-defuns-test)
