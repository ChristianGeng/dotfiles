;;; name.el --- summary -*- lexical-binding: t -*-

;; Author: christian
;; Maintainer: christian
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:


;;; Found this here:
;;; https://xenodium.com/emacs-quote-wrap-all-in-region/
(defun cg/toggle-quote-comma-wrap-all-in-region (beg end)
  "Toggle wrapping all items in region with double quotes + cimma. Use: Python LISTS"
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "no region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"," item)))
                              (split-string (buffer-substring beg end)))
                      " ")))
    (delete-region beg end)
    (insert replacement)))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; see https://www.emacswiki.org/emacs/UnfillParagraph
(defun cg/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
;; (define-key global-map "\M-Q" 'unfill-paragraph)


(defun cg/generate-openssl-password ()
  "Automatically generate a 15 character password using OpenSSL for the Account capture template."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string "openssl rand -base64 15")))

(defun cg/insert-openssl-password ()
  "Insert an OpenSSL password from `cg/generate-openssl-password' as a string."
  (interactive)
  (insert (cg/generate-openssl-password)))



(defun cg/my-directory-files (d)
  "Get List of files in directory"
  (let* ((path (file-name-as-directory (expand-file-name d)))
         (command (concat "ls -A1d " path "*")))
    (split-string (shell-command-to-string command) "\n" t)))


;;;###autoload
(defun toggle-menubar ()
"Toggle menubar visibility.
If toolbar is invisible, turn it on.  Otherwise turn it off."
  (interactive)
  (if (eq menu-bar-mode t)
      (menu-bar-mode -1)
          (menu-bar-mode t)

))

(defun toggle-toolbar ()
  "Toggle toolbar visibility.
If toolbar is invisible, turn it on.  Otherwise turn it off."
  (interactive)
  (if (eq tool-bar-mode t)
      (tool-bar-mode -1)
          (tool-bar-mode t)
))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


;; create a shell with name in specific directory,
;; see https://stackoverflow.com/questions/4880058/open-shell-in-emacs-with-a-given-working-directory
(defun shell-dir (name dir)
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))


(defun joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  (if (not dirs)
      root
    (apply 'joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root."
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

;;; see http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
;;; stammen aus der s-bibliothek
(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

;;; see https://emacs.stackexchange.com/questions/36299/move-file-associated-with-open-buffer-to-a-specific-folder
(defun move-buffer-file ()
  "Move visited file to another directory, for which you're prompted.
Directory defaults to the value of `my-archive-dir'."
  (interactive)
  (let ((old  (or (buffer-file-name)  (user-error "Not visiting a file")))
        (dir  (read-directory-name "Move to: " (s-trim (shell-command-to-string "pwd")))))
    (write-file (expand-file-name (file-name-nondirectory old) dir) t)
    (delete-file old)))

(defun close-all-buffers ()
  "Close all open buffers."
(interactive)
  (mapc 'kill-buffer (buffer-list)))

;; rename a currently open buffer
;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;; better; rename-current-buffer-file
(defun cg/rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file filename new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

;; interesting: http://web-mode.org/

;; lines words characters
 (defun cg/wc (&optional start end)
   "Prints number of lines, words and characters in region or whole buffer."
   (interactive)
   (let ((n 0)
         (start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max))))
     (save-excursion
       (goto-char start)
       (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
     (message "%3d %3d %3d" (count-lines start end) n (- end start))))

;;; print elements of list,. see https://www.gnu.org/software/emacs/manual/html_node/eintr/print_002delements_002dof_002dlist.html
(defun printlist (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))


(defun messagelist (list)
  "Print each element of LIST on a line of its own."
  (while list
    (message (car list))
    (setq list (cdr list))))


;; insert current date, use calendar, see for more details:
;; https://www.emacswiki.org/emacs/InsertingTodaysDate
(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))


 (defun timestamp ()
   (interactive)
   (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

 (defun date (arg)
   (interactive "P")
   (insert (if arg
               (format-time-string "%Y-%m-%d")
             (format-time-string "%Y-%m-%d"))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;;; see https://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region

;; https://www.emacswiki.org/emacs/ChangingEncodings

    (defun recode-region (start end &optional coding-system)
      "Replace the region with a recoded text."
      (interactive "r\n\zCoding System (utf-8): ")
      (setq coding-system (or coding-system 'utf-8))
      (let ((buffer-read-only nil)
	    (text (buffer-substring start end)))
        (delete-region start end)
        (insert (decode-coding-string (string-make-unibyte text) coding-system))))


;;; see this one: https://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(require 'sgml-mode)
(defun reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

;;; this one is the better one!
(defun nxml-pretty-format ()
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
        (nxml-mode)
        (indent-region begin end)))





;; https://www.emacswiki.org/emacs/EndOfLineTips
(defun unix-file ()
  "Change the current buffer to Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'unix t))

(defun dos-file ()
  "Change the current buffer to DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'dos t))

(defun mac-file ()
  "Change the current buffer to Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'mac t))

;; lumps several function into a big one for etengo thing
(defun format-properly (b e)
  "Run `indent-region', `untabify' and `delete-trailing-whitespace' in sequence."
  (interactive "*r")
  (when (use-region-p)
    (save-restriction
      (narrow-to-region b e)
      (replace-newlines-with-semicolon)
      (just-one-space-in-region b e)
      (kill-space-before-semicolon)
      )))

;; (global-set-key [f8] 'format-properly)


;;;;;;;;;;;;;;;;;;
;;; see https://stackoverflow.com/questions/5194294/how-to-remove-all-newlines-from-selected-region-in-emacs
(defun cg/remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))


;; see https://emacs.stackexchange.com/questions/18161/how-to-replace-multiple-newlines-for-single-one-in-whole-file
(defun cg/remove-extra-blank-lines ()
    "replace multiple blank lines with a single one"
    (interactive)
    (setq orig (point))
    (goto-char (point-min))
    (while (re-search-forward "^\n+" nil t)
      (replace-match "\n")
      (forward-char 1))
     (goto-char orig))


;;;;;;;;;;;;;;;;;;
(defun cg/replace-newlines-with-semicolon ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match ";" nil t))))



  ;;(global-set-key [f8] 'remove-newlines-in-region)

;;;;;;;;;;;;;;;;;;
(defun cg/kill-space-before-semicolon ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward " ;" nil t) (replace-match "; " nil t))))

(defun cg/just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun cg/whitespace-to-underscore (beg end)
  "BEG Replace all backward slashes in the region with forward slashes.  END.  "
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward " " nil t)
        (replace-match "_"))))
  )


(defun cg/forwardize-slashes (beg end)
  "BEG Replace all backward slashes in the region with forward slashes.  END.  "
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward "\\" nil t)
        (replace-match "/"))))
  )

(defun cg/backwardize-slashes (beg end)
  "Replace all forward slashes in the region with backward slashes."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward "/" nil t)
        (replace-match "\\"))))
  )


;; from cursor, get a region delimited by parentheses
;; https://stackoverflow.com/questions/5194417/how-to-mark-the-text-between-the-parentheses-in-emacs
;; then use mark-sexp
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;; https://www.emacswiki.org/emacs/BackwardKillLine
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))


(defun tpon ()
 "Activate the touchpad"
 (interactive)
 (shell-command "source cglib; tpon"))

(defun tpoff ()
 "Deactivate the touchpad"
 (interactive)
 (shell-command "source cglib; tpoff"))


(defun cg-pyclean-projectitle ()
  "clean python project"
  (interactive)
  ;; (shell-command "source cglib; tpoff")
  (defvar activeproject (projectile-project-root)
    "variable containing the active project")
  (message activeproject)
  (shell-command (concat "find " activeproject " -type f -name '*.py[co]' -delete"  ))
  (shell-command (concat "find " activeproject " -type f -name '__pycache__' -delete"  ))
  (shell-command (concat "find " activeproject " -name '.ipynb_checkpoints'  -exec rm -r '{}' \\;"  ))
  )

(defun cg-projectile-clean-cfiles ()
  "Clean  projects c files DANGEROUS in c projects!!"
  (interactive)
    (defvar activeproject (projectile-project-root)
      "variable containing the active project")
      (shell-command (concat "find " activeproject " -type f -name '*.c' -delete"  ))
    )


(defun lock-screen ()
 "Lock screen using screensaver"
 (interactive)
 (shell-command ". cglib; lock"))

(defun pull-google-cal()
 "update the google calendar ChristianGeng from google using script"
 (interactive)
 (shell-command "source ~/.shell_scripts; bash getgooglecal.sh &>/dev/null"))


;; shamelessly adopted from here
;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; this uses the absolute path
(defun cg/copy-current-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

 ;; (buffer-file-name)


(defun cg/copy-file-only-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename))
    (message filename)))


(defun cg/fpath ()
  "Gets the name of the file the current buffer is based on."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))


;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html
(defun cg/basename ()
  "Gets the name of the file the current buffer is based on."
  (interactive)
  (insert (file-name-nondirectory(buffer-file-name (window-buffer (minibuffer-selected-window))))))


;; see https://emacs.stackexchange.com/questions/60661/how-to-duplicate-a-file-in-dired
(defun cg/dired-duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((this  (dired-get-filename t))
         (ctr   1)
         (new   (format "%s[%d]" this ctr)))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s[%d]" this ctr)))
     (dired-copy-file this new nil))
  (revert-buffer))

(defun cg/remove-newlines-in-region ()
  "Remove all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))


(defun cg-remove-consecutive-newlines-in-region ()
  "Remove all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n\n" nil t) (replace-match "\n" nil t))))


(defun markdown-convert-buffer-to-org ()
    "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f markdown -t org -o %s"
                                     (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(defun insert-curly-braces (&optional arg)
  "Enclose following ARG sexps in parentheses.
Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `()' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

This command assumes point is not in a string or comment."
  (interactive "P")
  (insert-pair arg ?\{ ?\}))


(defun play-remote-audio (remote-file &optional no-play audacity)
  "Copy REMOTE-FILE from remote host and return local path.
With NO-PLAY non-nil, don't play the file automatically.
With AUDACITY non-nil, open the file in Audacity.

Returns the local file path as a string.

Example usage:
  (play-remote-audio \"user@host:file.wav\")  ; Copy and play
  (play-remote-audio \"user@host:file.wav\" t) ; Just copy
  (play-remote-audio \"user@host:file.wav\" t 'audacity) ; Open in Audacity"
  (interactive "sRemote file (user@host:/path/to/file.wav): \nP")
  (let* ((temp-dir (make-temp-file "emacs-audio-" t))
         (filename (file-name-nondirectory remote-file))
         (local-file (expand-file-name filename temp-dir))
         (play-command "mpv --no-terminal --no-resume-playback"))

    (message "Copying %s to %s..." remote-file local-file)
    (if (zerop (call-process "scp" nil "*scp-output*" t remote-file local-file))
        (progn
          (kill-new local-file)  ; Copy to kill ring
          (message "File ready at: %s" local-file)

          ;; Open in Audacity if requested
          (when audacity
            (if (executable-find "audacity")
                (start-process "audacity" nil "audacity" local-file)
              (message "Audacity not found in PATH")))

          ;; Auto-play unless disabled
          (when (and (not no-play) (not audacity))
            (start-process "audio-player" nil "sh" "-c"
                          (format "%s %s" play-command
                                 (shell-quote-argument local-file))))

          local-file)  ; Return the local file path
      (error "Failed to copy file from remote host"))))


(provide 'cglispfuncs)
;;; cglispfuncs.el ends here
