;;; init-session.el --- Session management -*- lexical-binding: t -*-

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

;;; Code:

(defun my/recenter-buffer ()
  "Recenter the view on buffer."
  (when buffer-file-name
    (ignore-errors (recenter))))

;;; saveplace
(setopt save-place-file (locate-user-emacs-file "places"))
(setopt save-place-limit 600)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'save-place-after-find-file-hook #'my/recenter-buffer)

;;; recentf
(setopt recentf-save-file (locate-user-emacs-file "recentf"))
(setopt recentf-max-saved-items 300)
(setopt recentf-max-menu-items 15)
(setopt recentf-auto-cleanup (if (daemonp) 300 'never))
(setopt recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"
                              (locate-user-emacs-file "elpa")
                              (expand-file-name "~/quicklisp/dists")))
(add-hook 'after-init-hook 'recentf-mode)

;;; bookmark
(setopt bookmark-default-file (locate-user-emacs-file "bookmarks"))
(setopt bookmark-save-flag 1)

(setopt history-length 100)
(setopt history-delete-duplicates t)

(defun my/strip-kill-ring ()
  "Strip all but strings from kill-ring."
  (mapcar #'substring-no-properties
          (cl-remove-if-not #'stringp kill-ring)))

;;; savehist
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

;;; abbrev
(add-hook 'kill-emacs-hook 'write-abbrev-file)
(setopt abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
(setopt save-abbrevs t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(provide 'init-session)

;;; init-session.el ends here
