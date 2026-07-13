;; Projectile-related defuns  -*- lexical-binding: t; -*-

(defun cg/projectile-add-projects-under (root)
  "Register every Git repo under ROOT as a Projectile known project.

A repo is any directory containing a .git entry; recursion stops at each repo,
so vendored trees (node_modules, .venv) and submodules are not descended into.
Interactive: pick a root such as ~/project/cgeng/work/research/iva to pick up
all of demos/ and tools/ in one go.  The list is persisted with
`projectile-save-known-projects'."
  (interactive "DAdd projectile projects under: ")
  (let ((root (file-name-as-directory (expand-file-name root)))
        (queue nil) (added 0))
    (push root queue)
    (while queue
      (let ((dir (pop queue)))
        (cond
         ((file-exists-p (expand-file-name ".git" dir))
          (projectile-add-known-project dir)
          (setq added (1+ added)))
         ((file-accessible-directory-p dir)
          (dolist (entry (directory-files dir t "\\`[^.]" t))
            (when (and (file-directory-p entry)
                       (not (member (file-name-nondirectory entry)
                                    '("node_modules" ".venv" "__pycache__"))))
              (push entry queue)))))))
    (projectile-save-known-projects)
    (message "Projectile: added %d project(s) under %s" added root)))
