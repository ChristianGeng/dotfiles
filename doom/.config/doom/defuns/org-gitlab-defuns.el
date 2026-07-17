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

(defun cg/org-gitlab--headline (headline contents info)
  "Transcode HEADLINE one level deeper, so the top level renders as H2.
org-export normalizes levels so the shallowest headline is level 1
\(`org-export--collect-tree-properties' sets `:headline-offset' to
1 - min-level); bumping the offset by one is therefore all the
demotion needed, regardless of the input's absolute levels."
  (org-export-with-backend
   'gfm headline contents
   (org-combine-plists
    info (list :headline-offset
               (1+ (or (plist-get info :headline-offset) 0))))))

(defvar cg/org-gitlab--backend
  (progn
    (require 'ox-gfm)
    (org-export-create-backend
     :name 'gitlab
     :parent 'gfm
     :transcoders '((link . cg/org-gitlab--link)
                    (headline . cg/org-gitlab--headline))))
  "GFM-derived export backend for GitLab issues/MRs.")

(defun cg/org-gitlab--export-string (org-text)
  "Export ORG-TEXT (a string of org markup) to GitLab markdown."
  (org-export-string-as
   org-text cg/org-gitlab--backend t
   '(:with-toc nil :with-todo-keywords nil :with-tags nil
     :with-author nil :with-date nil :with-timestamps nil
     :headline-levels 6)))
