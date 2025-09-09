;;; cg-secrets.el --- Secret management utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for managing secrets from pass and auth-source.
;; Provides functions to set environment variables from various secret stores.

;;; Code:

(defun cg/get-secret-from-pass (path)
  "Return first line of pass entry at PATH, or nil if unavailable."
  (when (and path (fboundp 'password-store-get))
    (ignore-errors (password-store-get path))))

(defun cg/get-secret-from-auth (host)
  "Return secret from auth-source for HOST, or nil if unavailable."
  (when (and host (fboundp 'auth-source-pick-first-password))
    (ignore-errors (auth-source-pick-first-password :host host))))

(defun cg/set-env-from-secrets (env-name pass-path auth-host)
  "Set ENV-NAME from pass PASS-PATH or auth-source AUTH-HOST if found.
Falls back to existing ENV-NAME value. Returns the value set (or nil)."
  (let* ((val (or (cg/get-secret-from-pass pass-path)
                  (cg/get-secret-from-auth auth-host)
                  (getenv env-name))))
    (when (and val (> (length val) 0))
      (setenv env-name val))
    (getenv env-name)))

(defun cg/init-api-key (env-name pass-path auth-host)
  "Initialize ENV-NAME using PASS-PATH or AUTH-HOST (compat wrapper)."
  (cg/set-env-from-secrets env-name pass-path auth-host))

(defun cg/pass--read-first-line (path)
  "Read first line from pass entry at PATH, or nil if unavailable."
  (ignore-errors
    (with-temp-buffer
      (let ((status (call-process "pass" nil t nil "show" path)))
        (when (and (integerp status) (= status 0))
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))))))

(defun cg/export-env-from-pass (&optional only-missing)
  "Set env vars in Emacs from pass using `cg/secret-specs'.
With ONLY-MISSING (prefix arg), don't overwrite vars already set.
Requires `cg/secret-specs' to be defined."
  (interactive "P")
  (when (boundp 'cg/secret-specs)
    (dolist (cell cg/secret-specs)
      (let* ((spec  (cdr cell))
             (path  (plist-get spec :pass))
             (envs  (let ((e (plist-get spec :env))) (if (listp e) e (list e))))
             (value (cg/pass--read-first-line path)))
        (when (and value (not (string-empty-p value)))
          (dolist (name envs)
            (when (or (not only-missing) (null (getenv name)))
              (setenv name value))))))))

(provide 'cg-secrets)
;;; cg-secrets.el ends here
