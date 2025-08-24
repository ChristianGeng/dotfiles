;; Functions (load all files in defuns-dir)
;;(add-to-list 'load-path user-emacs-directory)
(setq user-emacs-default-dir "~/emacs-conf/")
(setq defuns-dir (expand-file-name "defuns" user-emacs-default-dir))
(dolist (file (directory-files defuns-dir t "^[^.#].*el$"))
  (when (file-regular-p file)
    (load (file-name-sans-extension file))))

(setq org-directory "~/Dropbox/org")

(setq bookmark-default-file "~/.config/doom/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(evil-define-key 'normal ibuffer-mode-map
  (kbd "f c") 'ibuffer-filter-by-content
  (kbd "f d") 'ibuffer-filter-by-directory
  (kbd "f f") 'ibuffer-filter-by-filename
  (kbd "f m") 'ibuffer-filter-by-mode
  (kbd "f n") 'ibuffer-filter-by-name
  (kbd "f x") 'ibuffer-filter-disable
  (kbd "g h") 'ibuffer-do-kill-lines
  (kbd "g H") 'ibuffer-update)

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
;; sudo apt install sxiv
;; sudo apt install mpv
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "okular")))
(setq dired-guess-shell-alist-user '(("\\.png\'" "sxiv")))

(setq delete-by-moving-to-trash nil
      trash-directory "~/.local/share/Trash/files/")

;; Core function: not interactive, works on any region
(defun cg/semantic-fill-region (start end)
  "Apply semantic fill to region from START to END."
  (let ((fill-column 80))
    (fill-region start end)))

;; Interactive wrapper: acts on region if active, otherwise whole buffer
(defun cg/semantic-fill-dwim ()
  "Semantic fill: region if active, else whole buffer."
  (interactive)
  (if (use-region-p)
      (cg/semantic-fill-region (region-beginning) (region-end))
    (cg/semantic-fill-region (point-min) (point-max))))

;; Explicitly for whole buffer
(defun cg/semantic-fill-buffer ()
  "Semantic fill for the entire buffer."
  (interactive)
  (cg/semantic-fill-region (point-min) (point-max)))

;; Explicitly for region (errors if no region)
(defun cg/semantic-fill-region-interactive (start end)
  "Semantic fill for active region."
  (interactive "r")
  (cg/semantic-fill-region start end))

(defun cg/naive-semantic-line-breaks-region (start end)
  "Insert line breaks after sentence-ending punctuation followed by a capital letter."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\([\\.\\?!]\\)[ \t]+\\([A-Z]\\)" end t)
      (replace-match (concat "\\1\n\\2")))))


;; Interactive wrapper: region if active, else whole buffer
(defun cg/naive-semantic-line-breaks-dwim ()
  "Semantic line break: region if active, else whole buffer."
  (interactive)
  (if (use-region-p)
      (cg/naive-semantic-line-breaks-region (region-beginning) (region-end))
    (cg/naive-semantic-line-breaks-region (point-min) (point-max))))

(defun cg/break-command-args-region (start end)
  "Break shell command in region from START to END into multiple lines.
Each command line option (starting with '-') and its argument(s) will go on
their own line, prefixed with a backslash for shell line continuation.

Non-option arguments appearing after the command are also split onto their
own lines.

Continuation lines are indented with 4 spaces.

Example input:

  python test_memory_conversation.py --worker-url http://localhost:8001 --scenario landmarks extraArg

Example output:

  python test_memory_conversation.py \\
      --worker-url http://localhost:8001 \\
      --scenario landmarks \\
      extraArg

If called interactively with no active region, operates on entire buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq start (point-min)
          end (point-max)))
  (save-excursion
    (let* ((cmd-line (buffer-substring-no-properties start end))
           (tokens (split-string cmd-line "[ \t\n]+" t))
           (indent-str "    ")  ;; fixed 4 space indent
           (inhibit-read-only t))
      (delete-region start end)
      (goto-char start)
      ;; Insert the initial command (first token)
      (when tokens
        (insert (pop tokens)))
      ;; Process all remaining tokens
      (while tokens
        (let ((tok (pop tokens)))
          (if (or (string-prefix-p "-" tok) (string-prefix-p "--" tok))
              ;; If option, insert backslash + newline + fixed indent + option
              (insert (format " \\\n%s%s" indent-str tok))
            ;; else argument: insert space + token
            (insert (format " %s" tok))))))))

;; DWIM interactive wrapper
(defun cg/break-command-args-dwim ()
  "Break command args in region if active, else entire buffer."
  (interactive)
  (if (use-region-p)
      (cg/break-command-args-region (region-beginning) (region-end))
    (cg/break-command-args-region (point-min) (point-max))))

(setq imenu-list-focus-after-activation t)

(map! :leader
      (:prefix ("t" . "Toggle")
       :desc "Toggle imenu shown in a sidebar" "i" #'imenu-list-smart-toggle))

(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "/media/cgeng/TOSHIBA EXT/mp3"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      (:prefix ("e" . "EMMS audio player")
       :desc "Go to emms playlist"      "a" #'emms-playlist-mode-go
       :desc "Emms pause track"         "x" #'emms-pause
       :desc "Emms stop track"          "s" #'emms-stop
       :desc "Emms play previous track" "p" #'emms-previous
       :desc "Emms play next track"     "n" #'emms-next))

(setq display-line-numbers-type t)
(map! :leader
      :desc "Comment or uncomment lines"      "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines))

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(setq minimap-window-location 'right)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle minimap-mode" "m" #'minimap-mode))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle menu bar" "M" #'menu-bar-mode))

 (map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle menu bar" "T" #'tool-bar-mode))

(set-face-attribute 'mode-line nil :font "Ubuntu Mono-13")
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

;; should be moved to dedicated function
;; (add-to-list 'load-path "~/.config/doom/lisp/")
;; (require 'cg-utils)  ;; or (load "cg-utils.el")

(defun cg/toggle-mouse-and-line-numbers ()
  "Toggle xterm-mouse-mode and line numbers together.
When mouse mode is disabled, also disable line numbers for easier copy-paste."
  (interactive)
  (if xterm-mouse-mode
      (progn
        (xterm-mouse-mode -1)
        (display-line-numbers-mode -1)
        (message "xterm-mouse-mode OFF, line numbers OFF"))
    (xterm-mouse-mode 1)
    (display-line-numbers-mode 1)
    (message "xterm-mouse-mode ON, line numbers ON")))

(unless (display-graphic-p)
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Toggle xterm-mouse-mode" "M" #'cg/toggle-mouse-and-line-numbers )))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree"  "d n" #'neotree-dir)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file"      "=" #'(lambda () (interactive) (find-file "~/.config/doom/start.org"))
       ;; :desc "Edit agenda file"      "a" #'(lambda () (interactive) (find-file "~/nc/Org/agenda.org"))
       :desc "Edit doom config.org"  "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el"     "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))
(map! :leader
      (:prefix ("= e" . "open eshell files")
       :desc "Edit eshell aliases"   "a" #'(lambda () (interactive) (find-file "~/.config/doom/eshell/aliases"))
       :desc "Edit eshell profile"   "p" #'(lambda () (interactive) (find-file "~/.config/doom/eshell/profile"))))

(after! org
    (setq org-agenda-files
        (list
         (joindirs org-directory "agenda.org")
         )
        ))
(setq
   ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
   org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight bold)
     (?B :foreground "#98be65" :weight bold)
     (?C :foreground "#c678dd" :weight bold))
   org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "customtag"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks marked with customtag:")))

          (agenda "")
          (alltodo "")))))

(setq org-journal-dir (joindirs org-directory "journal")
      org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%A, %-d. %B %Y"
      org-journal-file-format "%Y-%m-%d.org")

(setq org-preview-latex-default-process 'dvisvgm)
(after! org
  (map! :map org-mode-map
        :localleader
        (:prefix ("v" . "view/toggle")
         :desc "Toggle LaTeX fragments" "l" #'org-toggle-latex-fragment
         :desc "Toggle inline images"   "i" #'org-toggle-inline-images)))

(after! org
  (when (display-graphic-p)
    (setq org-roam-directory (expand-file-name "roam" org-directory)
          org-roam-graph-viewer "/usr/bin/google-chrome")))

(map! :leader
      (:prefix ("n r" . "org-roam")
       :desc "Completion at point" "c" #'completion-at-point
       :desc "Find node"           "f" #'org-roam-node-find
       :desc "Show graph"          "g" #'org-roam-graph
       :desc "Insert node"         "i" #'org-roam-node-insert
       :desc "Capture to node"     "n" #'org-roam-capture
       :desc "Toggle roam buffer"  "r" #'org-roam-buffer-toggle))

(use-package! ox-gfm
  :after org)

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun dt/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'dt/insert-auto-tangle-tag)

(map! :leader
      :desc "Switch to perspective NAME"       "DEL" #'persp-switch
      :desc "Switch to buffer in perspective"  "," #'persp-switch-to-buffer
      :desc "Switch to next perspective"       "]" #'persp-next
      :desc "Switch to previous perspective"   "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
      :desc "Remove perspective by name"       "-" #'persp-remove-by-name)

(map! :leader
      :desc "Projectile run shell" "p S" #'projectile-run-shell
      :desc "Open eshell here"     "p E" #'eshell
      :desc "Open term here"       "p T" (cmd! (let ((default-directory (projectile-project-root))) (term (getenv "SHELL")))))

(after! lsp-mode
  (setq lsp-pyright-python-executable-cmd "python") ;; or path to your venv's python
  (setq lsp-pyright-typechecking-mode "basic"))      ;; optional, for type checking level

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(map! :leader
      (:prefix ("r" . "registers")
       :desc "Copy to register" "c" #'copy-to-register
       :desc "Frameset to register" "f" #'frameset-to-register
       :desc "Insert contents of register" "i" #'insert-register
       :desc "Jump to register" "j" #'jump-to-register
       :desc "List registers" "l" #'list-registers
       :desc "Number to register" "n" #'number-to-register
       :desc "Interactively choose a register" "r" #'consult-register
       :desc "View a register" "v" #'view-register
       :desc "Window configuration to register" "w" #'window-configuration-to-register
       :desc "Increment register" "+" #'increment-register
       :desc "Point to register" "SPC" #'point-to-register))

(setq shell-file-name "/bin/bash"
      vterm-max-scrollback 5000)
(setq eshell-rc-script "~/.config/doom/eshell/profile"
      eshell-aliases-file "~/.config/doom/eshell/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))
(map! :leader
      :desc "Eshell"                 "e s" #'eshell
      :desc "Eshell popup toggle"    "e t" #'+eshell/toggle
      :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Vterm popup toggle"     "v t" #'+vterm/toggle)

(defun cg/consult-dwim-input (orig-fn &rest args)
  "Advice to use region, Evil search word, or word at point as initial input."
  (let* ((region (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))))
         (evil-search (when (and (boundp 'evil-ex-search-pattern)
                                 evil-ex-search-pattern)
                        (car evil-ex-search-pattern)))
         (word (thing-at-point 'word t))
         (input (or region evil-search word)))
    (apply orig-fn (append (butlast args) (list input)))))

 (advice-add 'consult-line :around #'cg/consult-dwim-input)
 (advice-add 'consult-ripgrep :around #'cg/consult-dwim-input)

(dolist (fn '(consult-line consult-ripgrep consult-grep consult-find))
  (advice-add fn :around #'cg/consult-dwim-input))

(setq initial-buffer-choice "~/.config/doom/start.org")

(define-minor-mode start-mode
  "Provide functions for custom start page."
  :lighter " start"
  :keymap (let ((map (make-sparse-keymap)))
          ;;(define-key map (kbd "M-z") 'eshell)
            (evil-define-key 'normal start-mode-map
              (kbd "1") '(lambda () (interactive) (find-file "~/.config/doom/config.org"))
              (kbd "2") '(lambda () (interactive) (find-file "~/.config/doom/init.el"))
              (kbd "3") '(lambda () (interactive) (find-file "~/.config/doom/packages.el"))
              (kbd "4") '(lambda () (interactive) (find-file "~/.config/doom/eshell/aliases"))
              (kbd "5") '(lambda () (interactive) (find-file "~/.config/doom/eshell/profile")))
          map))

(add-hook 'start-mode-hook 'read-only-mode) ;; make start.org read-only; use 'SPC t r' to toggle off read-only.
(provide 'start-mode)

(setq doom-theme 'doom-henna)
(map! :leader
      :desc "Load new theme" "h t" #'consult-theme)

(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>"  #'winner-undo))

(map! :leader
      :desc "Zap to char"    "z" #'zap-to-char
      :desc "Zap up to char" "Z" #'zap-up-to-char)

(map! :leader
      :desc "Find file at point"
      "f ." #'find-file-at-point)

(getenv "OPENAI_API_KEY")
;; or
(password-store-get "code/openai_api_key")

;;; ========== pass bulk insert core (idempotent) ==========
(defun cg/pass--ensure ()
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

;;; ========== bulk from encrypted file ==========
(defun cg/pass-bulk-insert-from-file (file &optional force symbol)
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

;;; ========== bulk from variable (defvar cg/api-keys ...) ==========
(defun cg/pass-bulk-insert-from-var (alist &optional force)
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

(setq cg/secret-specs
  '((anthropic-aud
     :pass "code/anthropic_api_key_aud"
     :env  ("ANTHROPIC_API_KEY"))     ; optionally also "ANTHROPIC_API_KEY"
    (anthropic-personal
     :pass "code/anthropic_api_key_personal"
     :env  ("ANTHROPIC_API_KEY_PERSONAL"))
    (xai
     :pass "code/xai_api_key"
     :env  ("XAI_API_KEY"))
    (perplexity
     :pass "code/perplexity_api_key"
     :env  ("PPLX_API_KEY"))
    (openai-personal
     :pass "code/openai_api_key"
     :env  "OPENAI_API_KEY"))
  "Specs for secrets. No secret values here.
:pass = path in pass. :env = string or list of env var names to export.")

(defun cg/pass--read-first-line (path)
  (ignore-errors
    (with-temp-buffer
      (let ((status (call-process "pass" nil t nil "show" path)))
        (when (and (integerp status) (= status 0))
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))))))

(defun cg/export-env-from-pass (&optional only-missing)
  "Set env vars in Emacs from pass using `cg/secret-specs'.
With ONLY-MISSING (prefix arg), don't overwrite vars already set."
  (interactive "P")
  (dolist (cell cg/secret-specs)
    (let* ((spec  (cdr cell))
           (path  (plist-get spec :pass))
           (envs  (let ((e (plist-get spec :env))) (if (listp e) e (list e))))
           (value (cg/pass--read-first-line path)))
      (when (and value (not (string-empty-p value)))
        (dolist (name envs)
          (when (or (not only-missing) (null (getenv name)))
            (setenv name value)))))))

(defun cg/write-pass-export-script (file)
  "Write a script exporting env vars by reading pass at shell init time."
  (interactive "FWrite export script: ")
  (let ((lines (list "#!/usr/bin/env bash"
                     "set -euo pipefail" "")))
    (dolist (cell cg/secret-specs)
      (let* ((spec (cdr cell))
             (path (plist-get spec :pass))
             (envs (let ((e (plist-get spec :env))) (if (listp e) e (list e)))))
        (dolist (name envs)
          (push (format "export %s=\"$(pass show %s | head -n1)\"" name path)
                lines))))
    (with-temp-file file
      (insert (mapconcat #'identity (nreverse lines) "\n")))
    (set-file-modes file #o600)
    (message "Wrote %s (mode 600). Add 'source %s' to your shell rc." file file)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-c TAB" . copilot-accept-completion-by-line))
  :config
  ;; Customize copilot behavior
  (setq copilot-indent-offset-warning-disable t)

  ;; Show copilot suggestions with distinctive styling
  (set-face-attribute 'copilot-overlay-face nil
                      :foreground "#6272a4"
                      :background nil
                      :slant 'italic)

  ;; Auto-enable in specific modes
  (add-hook 'python-mode-hook 'copilot-mode)
  (add-hook 'js-mode-hook 'copilot-mode)
  (add-hook 'typescript-mode-hook 'copilot-mode)
  (add-hook 'rust-mode-hook 'copilot-mode)
  (add-hook 'go-mode-hook 'copilot-mode)
  (add-hook 'emacs-lisp-mode-hook 'copilot-mode)

  ;; Custom keybindings for copilot control
  ;; (map! :leader
  ;;       (:prefix ("c" . "copilot")
  ;;        :desc "Toggle Copilot" "t" #'copilot-mode
  ;;        :desc "Accept completion" "a" #'copilot-accept-completion
  ;;        :desc "Next completion" "n" #'copilot-next-completion
  ;;        :desc "Previous completion" "p" #'copilot-previous-completion
  ;;        :desc "Clear completion" "c" #'copilot-clear-overlay
  ;;        :desc "Login" "l" #'copilot-login
  ;;        :desc "Diagnose" "d" #'copilot-diagnose)))

;; Secrets helpers for AI tools
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

(use-package! aidermacs
  :config
  ;; Initialize API keys immediately when aidermacs is loaded
  (cg/set-env-from-secrets "OPENAI_API_KEY"     "code/openai_api_key"     "openai.com")
  (cg/set-env-from-secrets "ANTHROPIC_API_KEY"  "code/anthropic_api_key_personal"  "anthropic.com")
  (cg/set-env-from-secrets "XAI_API_KEY"        "code/xai_api_key"        "x.ai")
  (cg/set-env-from-secrets "PPLX_API_KEY"       "code/perplexity_api_key" "perplexity.ai")
  
  ;; Customize aidermacs behavior
  (setq aidermacs-model "gpt-4o"  ; or "claude-3-5-sonnet-20241022"
        aidermacs-auto-commit nil  ; Don't auto-commit changes
        aidermacs-show-diffs t)    ; Always show diffs

  ;; Also set up keys before any aidermacs command
  (advice-add 'aidermacs-start :before
              (lambda (&rest _)
                (cg/set-env-from-secrets "OPENAI_API_KEY"     "code/openai_api_key"     "openai.com")
                (cg/set-env-from-secrets "ANTHROPIC_API_KEY"  "code/anthropic_api_key_personal"  "anthropic.com")
                (cg/set-env-from-secrets "XAI_API_KEY"        "code/xai_api_key"        "x.ai")
                (cg/set-env-from-secrets "PPLX_API_KEY"       "code/perplexity_api_key" "perplexity.ai")))
  
  (advice-add 'aidermacs-send-prompt :before
              (lambda (&rest _)
                (cg/set-env-from-secrets "OPENAI_API_KEY"     "code/openai_api_key"     "openai.com")
                (cg/set-env-from-secrets "ANTHROPIC_API_KEY"  "code/anthropic_api_key_personal"  "anthropic.com")
                (cg/set-env-from-secrets "XAI_API_KEY"        "code/xai_api_key"        "x.ai")
                (cg/set-env-from-secrets "PPLX_API_KEY"       "code/perplexity_api_key" "perplexity.ai"))))

(defvar cg/ai-global-rules
  "You are an expert software developer assistant. Follow these global rules:

1. CODING STANDARDS:
   - Write clean, readable, and maintainable code
   - Follow language-specific best practices and idioms
   - Use meaningful variable and function names
   - Add comments for complex logic only
   - Prefer composition over inheritance
   - Write self-documenting code

2. SECURITY:
   - Never expose API keys or sensitive data
   - Validate all inputs
   - Use secure coding practices
   - Consider potential security vulnerabilities

3. PERFORMANCE:
   - Write efficient algorithms
   - Avoid premature optimization
   - Consider memory usage and time complexity
   - Use appropriate data structures

4. TESTING:
   - Suggest testable code structure
   - Include error handling
   - Consider edge cases
   - Write defensive code

5. DOCUMENTATION:
   - Keep documentation concise but clear
   - Update documentation when changing code
   - Use consistent formatting"
  "Global AI rules applied to all AI interactions.")


(defvar cg/ai-project-rules nil
  "Buffer to store project-specific AI rules loaded from .aiderrules file.")
(defun cg/load-project-ai-rules ()
  "Load AI rules from .aiderrules file in project root."
  (let ((rules-file (expand-file-name ".aiderrules" (project-root (project-current)))))
    (when (file-exists-p rules-file)
      (setq cg/ai-project-rules
            (with-temp-buffer
              (insert-file-contents rules-file)
              (buffer-string)))
      (message "Loaded project AI rules from %s" rules-file))))

(defun cg/get-combined-ai-rules ()
  "Combine global and project-specific AI rules."
  (concat cg/ai-global-rules
          (when cg/ai-project-rules
            (concat "\n\nPROJECT-SPECIFIC RULES:\n" cg/ai-project-rules))))

(defun cg/create-aiderrules-template ()
  "Create a template .aiderrules file in project root."
  (interactive)
  (let* ((project-root (project-root (project-current)))
         (rules-file (expand-file-name ".aiderrules" project-root))
         (template-content "# Project-specific AI rules for this codebase
# This file defines how AI assistants should behave in this project

## Framework/Technology Stack
- Language: [e.g., Python, JavaScript, Rust]
- Framework: [e.g., React, Django, Actix]
- Architecture: [e.g., MVC, microservices, monolith]

## Code Style Preferences
- Indentation: [e.g., 2 spaces, 4 spaces, tabs]
- Line length: [e.g., 80, 100, 120 characters]
- Naming convention: [e.g., camelCase, snake_case, PascalCase]

## Project-Specific Guidelines
- Use our custom error handling pattern
- Follow our API response format
- Implement proper logging using our logger
- Add type hints/annotations where applicable
- Follow our testing patterns and file structure

## Dependencies and Libraries
- Prefer [specific libraries] for [specific tasks]
- Avoid [specific libraries] due to [reasons]
- Use our internal utilities instead of [alternatives]

## File Organization
- Follow our directory structure conventions
- Use consistent file naming patterns
- Group related functionality appropriately

## Additional Instructions
- Always consider backward compatibility
- Optimize for readability over cleverness
- Include proper error messages
- Consider internationalization where applicable"))
    (if (file-exists-p rules-file)
        (message ".aiderrules already exists in %s" project-root)
      (with-temp-file rules-file
        (insert template-content))
      (find-file rules-file)
      (message "Created .aiderrules template in %s" project-root))))

;; Auto-load project rules when switching projects
(add-hook 'project-switch-hook #'cg/load-project-ai-rules)

;; Load rules when opening files in a new project
(add-hook 'find-file-hook
          (lambda ()
            (when (and (project-current) (not cg/ai-project-rules))
              (cg/load-project-ai-rules))))

(defun cg/ai-send-with-rules (content prompt-type)
  "Send content to AI with appropriate rules prepended."
  (let ((full-prompt (concat (cg/get-combined-ai-rules)
                           "\n\n=== TASK ===\n"
                           prompt-type
                           "\n\n=== CODE ===\n"
                           content)))
    (with-current-buffer (get-buffer-create "*AI Assistant*")
      (erase-buffer)
      (insert full-prompt)
      (gptel-mode)
      (goto-char (point-max))
      (gptel-send))))

(defun cg/ai-code-review ()
  "Send current buffer to GPTel for code review with rules."
  (interactive)
  (cg/ai-send-with-rules
   (buffer-string)
   "Please review this code for:
- Code quality and best practices
- Potential bugs or issues
- Performance improvements
- Security considerations
- Adherence to the specified rules and conventions"))

(defun cg/ai-explain-code ()
  "Explain selected code or function at point using GPTel with rules."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (when code
      (cg/ai-send-with-rules
       code
       "Please explain this code in detail, considering the project context and rules."))))

(defun cg/ai-refactor-with-aider ()
  "Start aidermacs and suggest refactoring for current file with rules."
  (interactive)
  (cg/load-project-ai-rules)  ; Ensure rules are loaded
  (aidermacs-start)
  (sleep-for 2)  ; Wait for aider to start
  (let ((prompt (concat (cg/get-combined-ai-rules)
                       "\n\nPlease review and suggest refactoring improvements for "
                       (buffer-file-name)
                       ". Focus on code quality, maintainability, and adherence to the specified rules.")))
    (aidermacs-send-prompt prompt)))

(defun cg/ai-generate-code ()
  "Generate code based on user prompt with project rules."
  (interactive)
  (let ((user-prompt (read-string "Describe what code you need: ")))
    (cg/ai-send-with-rules
     (format "Current file: %s\nContext: %s"
             (or (buffer-file-name) "New file")
             (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               "No specific context"))
     (concat "Generate code based on this request: " user-prompt))))

(defun cg/ai-fix-code ()
  "Fix code issues in current selection or buffer."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (cg/ai-send-with-rules
     code
     "Please identify and fix any issues in this code. Provide the corrected version with explanations.")))

(defun cg/ai-optimize-code ()
  "Optimize selected code or buffer for performance."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (cg/ai-send-with-rules
     code
     "Please optimize this code for better performance while maintaining readability and following the specified rules.")))

(defun cg/ai-add-tests ()
  "Generate tests for current function or class."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (when code
      (cg/ai-send-with-rules
       code
       "Please generate comprehensive tests for this code. Include unit tests, edge cases, and error scenarios."))))

(defun cg/ai-add-documentation ()
  "Generate documentation for current function or class."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (when code
      (cg/ai-send-with-rules
       code
       "Please generate appropriate documentation for this code. Include docstrings, parameter descriptions, and usage examples."))))

(defun cg/toggle-all-ai-tools ()
  "Toggle all AI tools on/off."
  (interactive)
  (copilot-mode 'toggle)
  (message "AI tools toggled: Copilot %s"
           (if copilot-mode "ON" "OFF")))

(map! :leader
      (:prefix ("A" . "AI / LLM")
       ;; Copilot subgroup
       (:prefix ("c" . "Copilot")
        :desc "Toggle Copilot" "t" #'copilot-mode
        :desc "Accept completion" "a" #'copilot-accept-completion
        :desc "Next completion" "n" #'copilot-next-completion
        :desc "Previous completion" "p" #'copilot-previous-completion
        :desc "Clear completion" "c" #'copilot-clear-overlay
        :desc "Login" "l" #'copilot-login
        :desc "Diagnose" "d" #'copilot-diagnose)
       ;; Aider subgroup
       (:prefix ("a" . "Aider")
        :desc "Start Aider" "s" #'aidermacs-start
        :desc "Stop Aider" "q" #'aidermacs-stop
        :desc "Send region" "r" #'aidermacs-send-region
        :desc "Send buffer" "b" #'aidermacs-send-buffer
        :desc "Send prompt" "p" #'aidermacs-send-prompt
        :desc "Show status" "S" #'aidermacs-status
        :desc "Clear context" "c" #'aidermacs-clear-context
        :desc "Add file" "f" #'aidermacs-add-file
        :desc "Remove file" "R" #'aidermacs-remove-file)
       ;; GPTel subgroup
       (:prefix ("g" . "GPTel")
        :desc "New Chat" "n" #'gptel)
       ;; Actions subgroup
       (:prefix ("x" . "AI Actions")
        :desc "Code Review" "r" #'cg/ai-code-review
        :desc "Explain Code" "e" #'cg/ai-explain-code
        :desc "Generate Code" "g" #'cg/ai-generate-code
        :desc "Fix Code" "f" #'cg/ai-fix-code
        :desc "Optimize Code" "o" #'cg/ai-optimize-code
        :desc "Add Tests" "t" #'cg/ai-add-tests
        :desc "Add Documentation" "d" #'cg/ai-add-documentation
        :desc "Refactor with Aider" "R" #'cg/ai-refactor-with-aider
        :desc "Toggle All AI" "T" #'cg/toggle-all-ai-tools)
       ;; Settings subgroup
       (:prefix ("s" . "AI Settings/Rules")
        :desc "Create .aiderrules" "r" #'cg/create-aiderrules-template
        :desc "Reload Rules" "R" #'cg/load-project-ai-rules
        :desc "Edit Global Rules" "g" (lambda () (interactive)
                                        (with-current-buffer (get-buffer-create "*AI Global Rules*")
                                          (erase-buffer)
                                          (insert cg/ai-global-rules)
                                          (markdown-mode)
                                          (switch-to-buffer (current-buffer)))))))

;; Make completions faster
(setq copilot-max-char -1)  ; No character limit for completions

;; Better integration with company-mode (if you use it)
(after! company
  (setq company-idle-delay 0.1)  ; Faster company popup
  ;; Ensure copilot doesn't conflict with company
  (add-hook 'copilot-mode-hook
            (lambda ()
              (setq-local company-idle-delay (if copilot-mode 0.5 0.1)))))

;; Auto-save before sending to AI tools
(defadvice aidermacs-send-buffer (before save-buffer-first activate)
  "Save buffer before sending to aider."
  (when (buffer-modified-p)
    (save-buffer)))

;; Auto-start copilot in programming modes
(add-hook 'doom-first-buffer-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (copilot-mode 1))))

;; Display AI status in modeline (optional)
(defun cg/ai-status-indicator ()
  "Show AI tools status in modeline."
  (concat
   (when (and (boundp 'copilot-mode) copilot-mode) " ⚡")
   (when (get-buffer "*aidermacs*") " 🤖")
   (when (and (boundp 'gptel-mode) gptel-mode) " 💬")))

;; Add to modeline (uncomment if desired)
;; (add-to-list 'mode-line-misc-info '(:eval (cg/ai-status-indicator)))
