;;; credentials.el --- Credential management utilities -*- lexical-binding: t; -*-

;; Functions for managing pass-based credentials and generating local configs

;;;###autoload
(defun cg/update-local-configs ()
  "Update ~/.local_configs from pass store using current specs."
  (interactive)
  (let* ((script-dir (expand-file-name "../../../../scripts" doom-user-dir))
         (gen-script (expand-file-name "generate-local-configs" script-dir)))
    (if (file-executable-p gen-script)
        (progn
          (message "Generating local configs...")
          (shell-command gen-script)
          (message "Local configs updated! Run 'source ~/.local_configs' or restart shell to apply."))
      (user-error "Generate script not found: %s" gen-script))))

;;;###autoload
(defun cg/dry-run-local-configs ()
  "Show what would be generated in ~/.local_configs without doing it."
  (interactive)
  (let* ((script-dir (expand-file-name "../../../../scripts" doom-user-dir))
         (gen-script (expand-file-name "generate-local-configs" script-dir)))
    (if (file-executable-p gen-script)
        (with-output-to-temp-buffer "*Local Configs Preview*"
          (shell-command (concat gen-script " --dry-run") "*Local Configs Preview*")
          (with-current-buffer "*Local Configs Preview*"
            (view-mode 1)))
      (user-error "Generate script not found: %s" gen-script))))

;;;###autoload
(defun cg/edit-credential-specs ()
  "Edit the credential specification file."
  (interactive)
  (let ((spec-file (expand-file-name "../../../../bash/.local_configs.spec" doom-user-dir)))
    (find-file-other-window spec-file)))

;;;###autoload
(defun cg/list-credentials ()
  "List all credentials from the pass store."
  (interactive)
  (if (executable-find "pass")
      (with-output-to-temp-buffer "*Pass Credentials*"
        (let ((credentials (split-string (shell-command-to-string "pass list --flat") "\n" t)))
          (dolist (cred credentials)
            (princ (format "%s\n" cred))))
        (with-current-buffer "*Pass Credentials*"
          (view-mode 1)))
    (user-error "pass not found")))

;;;###autoload
(defun cg/add-credential (path secret)
  "Add a new credential to pass store.
PATH is the path in pass store, SECRET is the secret value."
  (interactive 
   (list (read-string "Path: ")
         (read-passwd "Secret: ")))
  (when (y-or-n-p (format "Add credential at '%s'? " path))
    (let ((process (start-process "pass-insert" "*Pass Output*" "pass" "insert" "-f" path)))
      (process-send-string process secret)
      (process-send-eof process)
      (set-process-filter process 
        (lambda (proc output)
          (when (string-match-p "inserted" output)
            (message "Credential added successfully!")))))))

;;;###autoload
(defun cg/update-credential-from-kill-ring (path)
  "Update or create a credential from the current kill ring value."
  (interactive 
   (list (completing-read "Credential path: " 
                          (split-string (shell-command-to-string "pass list --flat") "\n" t))))
  (let ((secret (current-kill 0)))
    (if (y-or-n-p (format "Update credential '%s' with current kill ring content? " path))
        (let ((process (start-process "pass-insert" "*Pass Output*" "pass" "insert" "-f" path)))
          (process-send-string process secret)
          (process-send-eof process)
          (set-process-filter process 
            (lambda (proc output)
              (when (string-match-p "inserted" output)
                (message "Credential updated! Run cg/update-local-configs to apply."))))))))

;;;###autoload
(defun cg/show-credential (path)
  "Show a credential from the pass store."
  (interactive 
   (list (completing-read "Credential path: " 
                          (split-string (shell-command-to-string "pass list --flat") "\n" t))))
  (if (executable-find "pass")
      (let ((secret (string-trim (shell-command-to-string (concat "pass show " path)))))
        (if (not (string-empty-p secret))
            (message "%s: %s" path secret)
          (user-error "Credential not found: %s" path)))
    (user-error "pass not found")))

;; Add keybindings
(map! :leader
      (:prefix ("c" . "credentials")
       :desc "Update local configs"        "u" #'cg/update-local-configs
       :desc "Preview local configs"       "p" #'cg/dry-run-local-configs
       :desc "Edit credential specs"       "e" #'cg/edit-credential-specs
       :desc "List all credentials"        "l" #'cg/list-credentials
       :desc "Add new credential"          "a" #'cg/add-credential
       :desc "Update from kill ring"       "k" #'cg/update-credential-from-kill-ring
       :desc "Show credential"             "s" #'cg/show-credential))

(provide 'credentials)
;;; credentials.el ends here
