;;; cg-pass.el --- Password store utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for managing passwords with pass (password-store).
;; Provides functions for idempotent password insertion and bulk operations.

;;; Code:

(require 'cl-lib)

;;; Core password store operations

(defun cg/pass--ensure ()
  "Ensure pass is available."
  (or (executable-find "pass")
      (user-error "pass(1) not found. Install and initialize pass + GPG")))

(defun cg/pass--existing-first-line (path)
  "Return first line of existing pass entry PATH, or nil if missing/error."
  (let (out)
    (with-temp-buffer
      (let ((status (call-process "pass" nil t nil "show" path)))
        (when (and (integerp status) (= status 0))
          (goto-char (point-min))
          (when (re-search-forward "\\`\\([^\n\r]+\\)" nil t)
            (setq out (match-string 1))))))
    out))

(defun cg/pass--insert (path secret &optional force)
  "Insert SECRET at PATH via pass. If FORCE, overwrite."
  (let ((pass (cg/pass--ensure)))
    (with-temp-buffer
      (insert secret "\n")
      (let* ((args (append '("insert" "-m") (when force '("-f")) (list path)))
             (status (apply #'call-process-region (point-min) (point-max)
                            pass nil nil nil args)))
        (unless (and (integerp status) (= status 0))
          (user-error "pass insert failed (status %S) for %s" status path))))))

(defun cg/pass-upsert (path secret &optional force)
  "Idempotent insert: if PATH exists and equals SECRET, do nothing.
If different, overwrite when FORCE non-nil; otherwise prompt."
  (let ((existing (cg/pass--existing-first-line path)))
    (cond
     ((and existing (string= existing secret))
      (message "pass: %s already set; skipping" path))
     ((and existing (not force))
      (when (y-or-n-p (format "pass: %s exists and differs. Overwrite? " path))
        (cg/pass--insert path secret t)
        (message "pass: %s updated" path)))
     (t
      (cg/pass--insert path secret force)
      (message "pass: %s inserted" path)))))

;;; Bulk operations

defun cg/pass-bulk-insert-from-file (file &optional force symbol)
  "Load FILE (e.g. ~/.config/doom/my-secrets.el.gpg) and upsert all entries.
FILE must define an alist variable. SYMBOL (default: cg/private-pass-secrets)
is the variable name to read. With FORCE, overwrite without prompting."
  (interactive
   (list (read-file-name "Secrets file: " "~/.config/doom/" nil t nil
                         (lambda (f) (string-match-p "\\.el\\(\\.gpg\\)?\\'" f)))
         current-prefix-arg
         (intern (completing-read "Var symbol: "
                                  '(cg/private-pass-secrets cg/api-keys)
                                  nil t nil nil "cg/private-pass-secrets"))))
  (let ((sym (or symbol 'cg/private-pass-secrets)))
    (unless (file-readable-p file)
      (user-error "Secrets file not readable: %s" file))
    (load file nil t)
    (unless (boundp sym)
      (user-error "Variable %s not defined in %s" sym file))
    (cg/pass-bulk-insert-from-var (symbol-value sym) force)))

defun cg/pass-bulk-insert-from-var (alist &optional force)
  "Upsert all (PATH . SECRET) pairs from ALIST into pass.
With FORCE, overwrite differing entries without prompting."
  (interactive
   (list (let* ((sym (intern (completing-read "Var symbol: "
                                              obarray
                                              (lambda (s)
                                                (and (boundp s)
                                                     (listp (symbol-value s))))
                                              t nil nil "cg/api-keys"))))
           (symbol-value sym))
         current-prefix-arg))
  (unless (and (listp alist)
               (cl-every (lambda (x)
                           (and (consp x)
                                (stringp (car x))
                                (stringp (cdr x))))
                         alist))
    (user-error "Expected an alist of (PATH . SECRET) strings"))
  (dolist (cell alist)
    (cg/pass-upsert (car cell) (cdr cell) force)))

(provide 'cg-pass)
;;; cg-pass.el ends here
