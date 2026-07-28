;;; init-files.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - init file-related settings

;;; Code:

;; ffap
(setopt ffap-machine-p-known 'reject)

;; Useful key bindings { C-0 w } to copy link
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setopt dired-dwim-target t)
(setopt dired-recursive-copies 'always)
(setopt dired-recursive-deletes 'always)
(setopt dired-isearch-filenames t)
(setopt dired-omit-verbose nil)
(setopt dired-omit-lines directory-files-no-dot-files-regexp)
(setopt dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")

(setopt dired-ls-F-marks-symlinks t)
(setopt dired-guess-shell-alist-user
        '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "cvlc")))
(setopt dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                   "tar"))

(setopt dired-isearch-filenames 'dwim)
(setopt dired-create-destination-dirs 'ask)
(setopt dired-vc-rename-file t)
(setopt dired-movement-style 'bounded)
;; {E} 'dired-do-open'

;; Assemble a list of files you want to operate on with either find-dired, find-name-dired or find-grep-dired.
;; Mark all files in the resulting Dired buffer using t.
;; Use Q to start a query-replace-regexp session on the marked files.
;; To accept all replacements in each file, hit !.

;; TODO: how to sync these to other dired etc.?
;; TODO: add toggle option, default to human readable
(setopt find-ls-option '("-exec ls -ldh {} +" . "-ldh"))
;;(setopt find-ls-option '("-ls" . "-dilsb"))

(keymap-set ctl-x-map "C-j" 'dired-jump)
(keymap-set ctl-x-4-map "C-j" 'dired-jump-other-window)

;; shell-command-guess-functions

(when (executable-find "xwallpaper")
  (setopt wallpaper-command "xwallpaper")
  (setopt wallpaper-command-args '("--maximize" "%f")))

;; TRAMP stuff
;; { C-x x @ for tramp-revert-buffer-with-sudo }
(setopt tramp-use-scp-direct-remote-copying t)
(setopt tramp-file-name-with-method
        (when-let* ((become-cmd (or (executable-find "doas")
                                    (executable-find "sudo"))))
          (substring become-cmd -4)))

(provide 'init-files)

;; init-files.el ends here
