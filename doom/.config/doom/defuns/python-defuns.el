

  (defun cg/ruff-jump-to-file-line-column ()
    "Jump to file:line:column pattern under cursor."
    (interactive)
    (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (match (string-match "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" line)))
      (when match
        (let ((file (match-string 1 line))
              (line-num (string-to-number (match-string 2 line)))
              (col-num (string-to-number (match-string 3 line))))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line-num))
          (move-to-column (1- col-num))))))

  ;; Bind it to a convenient key, for example:
  ;; (global-set-key (kbd "C-c f") 'jump-to-file-line-column)
