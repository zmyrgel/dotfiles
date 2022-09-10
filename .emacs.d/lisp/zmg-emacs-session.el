;;; ------------------------------
;;; Session
;;; ------------------------------

(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "places"))
  (save-place-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude "\\elpa")
  :hook (after-init-hook . recentf-mode))

(use-package bookmark
  :config
  (setq bookmark-default-file (locate-user-emacs-file "bookmarks"))
  (setq bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 50)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (setq savehist-autosave-interval 60)
  :hook (after-init-hook . savehist-mode))

(use-package abbrev
  :hook (kill-emacs-hook . write-abbrev-file)
  :config
  (setq abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
  (setq save-abbrevs t)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

(use-package files
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  :config
  (setq view-read-only t)
  (setq large-file-warning-threshold 50000000) ;; 50mb
  (setq backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setq make-backup-files t)
  (setq backup-by-copying t)
  (setq mode-require-final-newline t)
  (setq require-final-newline t))

(provide 'zmg-emacs-session)
