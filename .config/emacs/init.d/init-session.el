;;; init-session.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Emacs initializations related to sessions

;;; Code:

(setq save-place-file (locate-user-emacs-file "places"))
(add-hook 'after-init-hook 'save-place-mode)

(setq recentf-save-file (locate-user-emacs-file "recentf"))
(setq recentf-max-saved-items 300)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"
                            (locate-user-emacs-file "elpa")
                            (expand-file-name "~/quicklisp/dists")))
(add-hook 'after-init-hook 'recentf-mode)

(setq bookmark-default-file (locate-user-emacs-file "bookmarks"))
(setq bookmark-save-flag 1)

(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 50)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
        register-alist
        mark-ring global-mark-ring
        search-ring regexp-search-ring))
(setq savehist-autosave-interval 60)
(add-hook 'after-init-hook 'savehist-mode)

(add-hook 'kill-emacs-hook 'write-abbrev-file)
(setq abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
(setq save-abbrevs t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq view-read-only t)
(setq large-file-warning-threshold 50000000) ;; 50mb

(setq make-backup-files t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq backup-by-copying t)

;; todo: revert-without-query regexp
;; todo: small-temporary-file-directory ? use tmpfs ?

(setq mode-require-final-newline t)
(setq require-final-newline t)

(setq remember-data-file "~/Documents/notes"
      remember-notes-initial-major-mode 'org-mode
      remember-notes-auto-save-visited-file-name t
      remember-in-new-frame t)

(provide 'init-session)

;; init-session.el ends here
