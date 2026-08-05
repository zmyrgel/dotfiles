;;; init-shell.el --- Shell management -*- lexical-binding: t -*-

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

;; Generic shell utilities

;;; Code:

;; FIXME: rely on system config, use man command flags instead of env?
;; TODO: really need to check dir exists?
(let ((home-man (expand-file-name "~/share/man"))
      (man-path (split-string (or (getenv "MANPATH") "") ":")))
  (when (file-directory-p home-man)
    (add-to-list 'man-path home-man)
    (setenv "MANPATH" (string-join man-path ":"))))

(setopt shell-kill-buffer-on-exit t)

(defun my/sh-mode-hook ()
  (setq-local indent-tabs-mode t)
  (setq-local sh-basic-offset 8))

(add-hook 'sh-mode-hook 'my/sh-mode-hook)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; comint
(setopt comint-scroll-to-bottom-on-input t)
(setopt comint-scroll-to-bottom-on-output t)
(setopt comint-scroll-show-maximum-output t)
(setopt comint-completion-autolist t)
(setopt comint-input-ignoredups t)
(setopt comint-completion-addsuffix t)
(setopt comint-prompt-read-only t)

(with-eval-after-load 'comint-mode 
  (keymap-substitute comint-mode-map 'kill-region 'comint-kill-region)
  (keymap-substitute comint-mode-map 'kill-whole-line 'comint-kill-whole-line))

;;; eshell
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
	 (height (/ (window-total-height) 3))
	 (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(keymap-global-set "C-!" 'eshell-here)

;; smart shell
(require 'em-smart)
(setopt eshell-where-to-jump 'begin)
(setopt eshell-review-quick-commands nil)
(setopt eshell-smart-space-goes-to-end t)
(setopt eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
(setopt eshell-save-history-on-exit t)
(setopt eshell-scroll-show-maximum-output t)
(setopt eshell-scroll-to-bottom-on-output t)
(setopt eshell-cmpl-autolist t)
(setopt eshell-cmpl-cycle-completions nil)
(setopt eshell-cmpl-cycle-cutoff-length 2)
(setopt eshell-cmpl-ignore-case t)
(setopt eshell-cp-overwrite-files nil)
(setopt eshell-default-target-is-dot t)
(setopt eshell-destroy-buffer-when-process-dies t)
(setopt eshell-hist-ignoredups t)
(setopt eshell-list-files-after-cd t)
(setopt eshell-review-quick-commands t)
(setopt eshell-save-history-on-exit t)
(setopt eshell-scroll-show-maximum-output nil)
(setopt eshell-visual-subcommands '(("git" "log" "diff" "show" "branch")))
(setopt eshell-visual-options '(("git" "--help" "--paginate")))
(setopt eshell-hist-ignoredups t)
(setopt eshell-history-append t)
(setopt eshell-history-isearch 'dwim)

(defun eshell/x ()
  "Closes the Emacs Shell session and gets rid of the window as well."
  (delete-window)
  (eshell/exit))

(provide 'init-shell)

;;; init-shell.el ends here
