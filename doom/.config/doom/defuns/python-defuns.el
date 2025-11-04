

  (defun cg/ruff-jump-to-file-line-column ()
    "Jump to file[:LINE[:COL]] on current line."
    (interactive)
    (let* ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (proj-root (or (ignore-errors (vc-root-dir))
                          (ignore-errors (project-root (project-current)))
                          default-directory))
           ;; Match filename with common path chars, then optional :LINE and optional :COL
           (rx "\\([[:alnum:]_./-]+\\)\\(?::\\([0-9]+\\)\\)?\\(?::\\([0-9]+\\)\\)?")
           file line-num col-num abs-file)
      (save-match-data
        (let ((start 0) found)
          ;; Scan across the line; pick the first candidate that resolves to an existing file
          (while (and (not found) (string-match rx text start))
            (let* ((f (match-string 1 text))
                   (ln (match-string 2 text))
                   (cn (match-string 3 text))
                   (cand (if (file-name-absolute-p f) f (expand-file-name f proj-root)))
                   (real (ignore-errors (file-truename cand))))
              (setq line-num (and ln (string-to-number ln))
                    col-num  (and cn (string-to-number cn)))
              (if (and real (file-exists-p real))
                  (setq file f abs-file real found t)
                (setq start (1+ (match-beginning 1))))))
          (unless found
            (user-error "No file[:line[:col]] found on line, or resolved file does not exist"))))
      (let ((buf (find-file-noselect abs-file)))
        (pop-to-buffer buf)
        (when line-num
          (goto-char (point-min))
          (forward-line (max 0 (1- line-num)))
          (when col-num
            (move-to-column (max 0 (1- col-num))))))))

  ;; Bind it to a convenient key, for example:
  ;; (global-set-key (kbd "C-c f") 'cg/ruff-jump-to-file-line-column)
