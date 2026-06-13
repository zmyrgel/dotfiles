;;; init-files.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - init file-related settings

;;; Code:

;; Useful key bindings { C-0 w } to copy link
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-isearch-filenames t)
(setq dired-omit-verbose nil)
(setq dired-omit-lines directory-files-no-dot-files-regexp)
(setq dired-ls-F-marks-symlinks t)
(setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")
(setq dired-guess-shell-alist-user
      '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "cvlc")))
(setq dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                 "tar"))

(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)
(setq dired-movement-style 'bounded)
;; {E} 'dired-do-open'

(define-key ctl-x-map (kbd "C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)

(when (executable-find "xwallpaper")
  (setq wallpaper-command "xwallpaper")
  (setq wallpaper-command-args '("--maximize" "%f")))

;; TRAMP stuff
(setq tramp-use-scp-direct-remote-copying t)
(setq tramp-file-name-with-method
      (when-let* ((become-cmd (or (executable-find "doas")
                                  (executable-find "sudo"))))
        (substring become-cmd -4)))

;; EMMS
(ensure-packages-present 'emms)
(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native))

(provide 'init-files)

;; init-files.el ends here
