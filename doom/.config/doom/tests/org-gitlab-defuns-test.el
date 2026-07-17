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

(provide 'org-gitlab-defuns-test)
