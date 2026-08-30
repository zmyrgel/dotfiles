;;; init-files.el --- File and directory management setup -*- lexical-binding: t -*-

;; Copyright (c) 2022-2026 Timo Myyrä <timo.myyra@bittivirhe.fi>

;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;; URL: https://github.com/zmyrgel/dotfiles
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; - init file-related settings

;;; Code:

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(setopt view-read-only t)
(setopt large-file-warning-threshold 50000000) ;; 50mb

;;; backup
(setopt make-backup-files t)
(setopt backup-by-copying t)
;;(setopt backup-directory-alist `((".*" . ,temporary-file-directory)))
(let ((backup-dir (expand-file-name "backup" user-emacs-directory)))
  (make-directory backup-dir t)
  (setopt backup-directory-alist
          `((".*" . ,backup-dir)))
  (setopt auto-save-file-name-transforms
          `((".*" ,backup-dir t))))

(setopt delete-old-versions t)

;; todo: revert-without-query regexp
;; todo: small-temporary-file-directory ? use tmpfs ?

(setopt mode-require-final-newline t)
(setopt require-final-newline t)

;;; ffap
(setopt ffap-machine-p-known 'reject)

;; { M-x ff-find-other-file }
;; { M-x find-sibling-file }

;;; dired

;; Useful key bindings { C-0 w } to copy link
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setopt dired-create-destination-dirs 'ask)
(setopt dired-clean-confirm-killing-deleted-buffers nil)
(setopt dired-dwim-target t)
(setopt dired-isearch-filenames 'dwim)
(setopt dired-isearch-filenames t)
(setopt dired-ls-F-marks-symlinks t)
(setopt dired-movement-style 'bounded-files)
(setopt dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")
(setopt dired-omit-lines directory-files-no-dot-files-regexp)
(setopt dired-omit-verbose nil)
(setopt dired-recursive-copies 'always)
(setopt dired-recursive-deletes 'top)
(setopt dired-vc-rename-file t)
(setopt dired-free-space 'first)
(setopt dired-auto-revert-buffer 'dired-directory-changed-p)
(setopt dired-do-revert-buffer t)
(setopt dired-mouse-drag-files nil)

(defvar my/generic-open-cmd
  (cond ((memq system-type '(cygwin windows-nt ms-dos))
         "start")
        ((eq system-type 'darwin)
         "open")
        ((memq system-type '(gnu gnu/linux gnu/kfreebsd
                                 berkeley-unix))
         "xdg-open"))
  "Generic command to run applications.")

(setopt dired-guess-shell-alist-user
        `(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" ,my/generic-open-cmd)))
(setopt dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                   "tar"))

;; {E} 'dired-do-open'

;; Assemble a list of files you want to operate on with either find-dired, find-name-dired or find-grep-dired.
;; Mark all files in the resulting Dired buffer using t.
;; Use Q to start a query-replace-regexp session on the marked files.
;; To accept all replacements in each file, hit !.

;; TODO: how to sync these to other dired etc.?
;; TODO: add toggle option, default to human readable
(setopt find-ls-option '("-exec ls -ldh {} +" . "-ldh"))
;;(setopt find-ls-option '("-ls" . "-dgils"))

(keymap-set ctl-x-map "C-j" 'dired-jump)
(keymap-set ctl-x-4-map "C-j" 'dired-jump-other-window)

;; shell-command-guess-functions

;;; tramp stuff
;; { C-x x @ for tramp-revert-buffer-with-sudo }
(setopt tramp-use-scp-direct-remote-copying t)
(setopt tramp-file-name-with-method
        (when-let* ((become-cmd (or (executable-find "doas")
                                    (executable-find "sudo"))))
          (substring become-cmd -4)))

(provide 'init-files)

;;; init-files.el ends here
