;;; generate-local-configs.el --- Generate local configs from pass store

;; This file is generated/used by generate-local-configs script

(let ((template-file (getenv "TEMPLATE_FILE"))
      (spec-file (getenv "SPEC_FILE"))
      (output-file (getenv "OUTPUT_FILE")))
  
  ;; Read spec file
  (with-temp-buffer
    (insert-file-contents spec-file)
    (goto-char (point-min))
    (setq specs (read (current-buffer))))
  
  ;; Read template
  (with-temp-buffer
    (insert-file-contents template-file)
    (setq template-content (buffer-string)))
  
  ;; Replace variables with values from pass
  (dolist (spec specs)
    (let* ((name (car spec))
           (pass-path (plist-get (cdr spec) :pass))
           (env-var (plist-get (cdr spec) :env))
           (pass-value (string-trim (shell-command-to-string (concat "pass show " pass-path " 2>/dev/null || echo ''")))))
      (when (and pass-value (not (string-empty-p pass-value)))
        (setq template-content 
              (replace-regexp-in-string 
               (concat "\\${" env-var "}") 
               pass-value 
               template-content 
               t)))))
  
  ;; Write output file
  (with-temp-file output-file
    (insert template-content))
  
  (print (concat "Generated: " output-file)))
