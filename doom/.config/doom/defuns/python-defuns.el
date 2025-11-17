

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



  (defun cg/autoflake--exec ()
    (or (executable-find "autoflake")
        (user-error "Command 'autoflake' not found in PATH. Install it (e.g., `uv tool install autoflake`).")))

  (defun cg/autoflake--apply-to-region (beg end &optional extra-args)
    "Run autoflake on region [BEG,END] and replace it with the result."
    (let* ((exe (cg/autoflake--exec))
           (args (append (list "--remove-unused-variables" "-") extra-args))
           (outbuf (get-buffer-create "*autoflake-output*"))
           (errbuf (get-buffer-create "*autoflake*")))
      (with-current-buffer outbuf (erase-buffer))
      (with-current-buffer errbuf (erase-buffer))
      (let ((exit (apply #'call-process-region beg end exe nil (list outbuf errbuf) nil args)))
        (if (eq exit 0)
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char beg)
                (delete-region beg end)
                (insert (with-current-buffer outbuf (buffer-string)))))
          (display-buffer errbuf)
          (user-error "autoflake failed (exit %s)" exit)))))

  (defun cg/autoflake--apply-to-buffer ()
    "Run autoflake on the whole buffer and replace its contents."
    (save-restriction
      (widen)
      (cg/autoflake--apply-to-region (point-min) (point-max))))

  (defun cg/python-remove-unused-variables (&optional arg)
    "Remove unused variables using autoflake.

- If a region is active, run on that region and replace it.
- Otherwise, run on the entire current buffer (in memory).
- With a prefix ARG (C-u), run in-place (-i) on a file you choose.
  If the file is visited by a buffer, that buffer is reverted."
    (interactive "P")
    (if arg
        ;; In-place on a chosen file
        (let* ((exe (cg/autoflake--exec))
               (target (expand-file-name
                        (read-file-name "Autoflake in-place on file: " nil (buffer-file-name) t)))
               (exit (process-file exe nil nil nil "--remove-unused-variables" "-i" target)))
          (if (eq exit 0)
              (let ((buf (get-file-buffer target)))
                (when buf (with-current-buffer buf (revert-buffer t t t)))
                (message "autoflake: updated %s" target))
            (user-error "autoflake failed (exit %s)" exit)))
      ;; Region or whole buffer (no in-place write)
      (if (use-region-p)
          (cg/autoflake--apply-to-region (region-beginning) (region-end))
        (cg/autoflake--apply-to-buffer))))
