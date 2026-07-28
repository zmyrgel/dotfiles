;;; init-session.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Emacs initializations related to sessions

;;; Code:

(defun my/recenter-buffer ()
  "Recenter the view on buffer."
  (when buffer-file-name
    (ignore-errors (recenter))))

(setopt save-place-file (locate-user-emacs-file "places"))
(setopt save-place-limit 600)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'save-place-after-find-file-hook #'my/recenter-buffer)

(setopt recentf-save-file (locate-user-emacs-file "recentf"))
(setopt recentf-max-saved-items 300)
(setopt recentf-max-menu-items 15)
(setopt recentf-auto-cleanup (if (daemonp) 300 'never))
(setopt recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"
                              (locate-user-emacs-file "elpa")
                              (expand-file-name "~/quicklisp/dists")))
(add-hook 'after-init-hook 'recentf-mode)

(setopt bookmark-default-file (locate-user-emacs-file "bookmarks"))
(setopt bookmark-save-flag 1)

(setopt history-length 100)
(setopt history-delete-duplicates t)

(defun my/strip-kill-ring ()
  "Strip all but strings from kill-ring."
  (mapcar #'substring-no-properties
          (cl-remove-if-not #'stringp kill-ring)))

(setopt savehist-file (locate-user-emacs-file "savehist"))
(setopt savehist-save-minibuffer-history t)
(setopt savehist-additional-variables
        '(kill-ring
          register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring))
(setopt savehist-autosave-interval 60)
(add-hook 'after-init-hook 'savehist-mode)
(add-hook 'savehist-save-hook #'my/strip-kill-ring)

(add-hook 'kill-emacs-hook 'write-abbrev-file)
(setopt abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
(setopt save-abbrevs t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(setopt view-read-only t)
(setopt large-file-warning-threshold 50000000) ;; 50mb

(setopt make-backup-files t)
(setopt backup-directory-alist `((".*" . ,temporary-file-directory)))
(setopt backup-by-copying t)

;; todo: revert-without-query regexp
;; todo: small-temporary-file-directory ? use tmpfs ?

(setopt mode-require-final-newline t)
(setopt require-final-newline t)

(provide 'init-session)

;; init-session.el ends here
