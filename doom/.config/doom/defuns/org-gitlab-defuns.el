;;; org-gitlab-defuns.el --- Copy/paste org as GitLab markdown  -*- lexical-binding: t; -*-

;; Converts org(-roam) content to GitLab-flavored markdown via a backend
;; derived from ox-gfm. Two entry points:
;;   cg/org-copy-as-gitlab-markdown  — org buffer → clipboard (paste in browser)
;;   cg/org-yank-as-gitlab-markdown  — clipboard org → markdown buffer
;; Spec: docs/superpowers/specs/2026-07-17-org-gitlab-markdown-design.md

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

(defun cg/org-gitlab--plain-text (text info)
  "Transcode plain TEXT via gfm, then drop the backslash before underscores.
ox-md escapes every `_' as `\\_' to avoid emphasis, but GFM does not treat
intraword underscores as emphasis, so the escaping is just noise that makes
identifiers like IVA_SLM_MODEL_PATH unreadable in the raw markdown."
  (replace-regexp-in-string
   "\\\\_" "_" (org-export-with-backend 'gfm text info)))

(defvar cg/org-gitlab--backend nil
  "GFM-derived export backend for GitLab issues/MRs, built on first use.
Lazy so loading this file at startup doesn't pull in org/ox/ox-gfm.")

(defun cg/org-gitlab--ensure-backend ()
  "Return the GitLab export backend, creating it on first call."
  (or cg/org-gitlab--backend
      (progn
        (require 'ox-gfm)
        (setq cg/org-gitlab--backend
              (org-export-create-backend
               :name 'gitlab
               :parent 'gfm
               :transcoders '((link . cg/org-gitlab--link)
                              (headline . cg/org-gitlab--headline)
                              (plain-text . cg/org-gitlab--plain-text)))))))

(defun cg/org-gitlab--export-string (org-text)
  "Export ORG-TEXT (a string of org markup) to GitLab markdown."
  (org-export-string-as
   org-text (cg/org-gitlab--ensure-backend) t
   ;; :with-sub-superscript {} (note: SINGULAR key — ox.el's option is
   ;; `:with-sub-superscript`) — bare a_b / a^b are NOT sub/superscripts
   ;; (only explicit a_{b}), so identifiers like IVA_SLM_MODEL_PATH keep
   ;; their literal underscores instead of turning into <sub> markup.
   ;; :with-special-strings nil — keep "..." literal, not the … entity.
   '(:with-toc nil :with-todo-keywords nil :with-tags nil
     :with-author nil :with-date nil :with-timestamps nil
     :with-sub-superscript {} :with-special-strings nil
     :headline-levels 6)))

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

(defun cg/org-gitlab--markdown-to-org (md-text)
  "Convert MD-TEXT (GitLab/GitHub-flavored markdown) to org via pandoc."
  (unless (executable-find "pandoc")
    (user-error "Cannot convert markdown to org: pandoc not installed"))
  (with-temp-buffer
    (insert md-text)
    (let ((exit-code (call-process-region (point-min) (point-max) "pandoc"
                                          t t nil "-f" "gfm" "-t" "org")))
      (unless (zerop exit-code)
        (user-error "pandoc failed (exit %d): %s" exit-code
                    (string-trim (buffer-string)))))
    ;; pandoc artifacts: CUSTOM_ID drawers from GFM auto heading ids
    ;; (the gfm_auto_identifiers reader extension can't be disabled in
    ;; pandoc 2.9), and unicode checkboxes instead of org's [ ]/[X].
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*:PROPERTIES:\n\\(?:[ \t]*:CUSTOM_ID:.*\n\\)+[ \t]*:END:\n"
            nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "[☐☒]" nil t)
      (replace-match (if (string= (match-string 0) "☒") "[X]" "[ ]")))
    (buffer-string)))

(defun cg/markdown-yank-as-org ()
  "Insert clipboard/kill-ring markdown content converted to org.
For capturing GitLab issue text or local markdown files into org notes."
  (interactive)
  (insert (cg/org-gitlab--markdown-to-org (current-kill 0))))

(after! org
  (map! :map org-mode-map
        :localleader
        :desc "Copy as GitLab markdown" "y" #'cg/org-copy-as-gitlab-markdown
        :desc "Yank markdown as org"    "Y" #'cg/markdown-yank-as-org))
