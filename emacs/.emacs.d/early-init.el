;;; early-init.el --- -*- lexical-binding: t; -*-

;; iva-p5 login node: $HOME lives on FSx Lustre, which is slow for the many
;; small random reads/writes Emacs caches do.  Redirect Doom's cache dir
;; (native-comp eln, org-roam.db via `user-emacs-directory', copilot cache,
;; etc.) to the node-local EBS root *before* chemacs loads Doom's early-init,
;; where `doom-cache-dir' and the eln path are derived.
;;
;; Gate: $HOME under /fsx and no worker-local NVMe (/opt/dlami/nvme) means we
;; are on the login node.  /var/tmp is safe persistent storage there: no
;; tmpfiles/tmpreaper age policy (the "30d" line in
;; /usr/lib/tmpfiles.d/tmp.conf is commented out; checked 2026-07).
;; Do NOT use /tmp (may become a small tmpfs) or $XDG_RUNTIME_DIR (wiped when
;; lingering is off).
(when (and (string-prefix-p "/fsx/" (expand-file-name "~"))
           (not (file-directory-p "/opt/dlami/nvme")))
  (let ((cache (concat "/var/tmp/" (user-login-name) "/doom-cache/")))
    ;; 0700 on /var/tmp/$USER: caches (org-roam.db, savehist) can embed file
    ;; contents and this is a shared node.
    (with-file-modes #o700
      (make-directory (concat cache "eln/") t))
    ;; Doom's later (defvar doom-cache-dir ...) keeps this value.  With Doom
    ;; profiles off, everything cache-ish keys off it, including the
    ;; native-comp eln dir and the `user-emacs-directory' redirect that
    ;; packages like org-roam use for their default file locations.
    (setq doom-cache-dir cache)
    ;; Redirect native-comp output right away; Doom later add-to-lists the
    ;; same "<cache>/eln/" path, which dedupes against this entry.
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache (concat cache "eln/")))))

(require 'chemacs
         (expand-file-name "chemacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))
(chemacs-load-user-early-init)
