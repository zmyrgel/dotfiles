;;; init-visual.el --- Settings related to UI and visual presentation -*- lexical-binding: t -*-

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

(defun pulse-line ()
  "Pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line))

;;(add-hook 'window-state-change-hook #'pulse-line)

(global-font-lock-mode t)

(setopt font-lock-maximum-decoration t)

(setopt window-combination-resize t)

(setopt show-paren-style 'parenthesis)
(setopt show-paren-when-point-in-periphery t)
(setopt show-paren-when-point-inside-paren nil)
(add-hook 'after-init-hook 'show-paren-mode)

(blink-cursor-mode -1)

;; help
(setopt help-window-select t)

;; hide minor mode bindings by default to focus on major ones.
(setopt describe-bindings-outline-rules
        '((match-regexp . "Key translations\\|Minor Mode Bindings")))

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | { C-x 4 C-f } | Find-file other-window       |
;; | { C-x 4 d }   | Dired other-window           |
;; | { C-x 4 C-o } | Display buffer other-window  |
;; | { C-x 4 b }   | Set buffer in other-window   |
;; | { C-x 4 0 }   | Kill buffer and window       |
;; | { C-x 4 p }   | Run project cmd in window    |

(keymap-global-set "C-c w" 'winner-undo)
(keymap-global-set "C-c W" 'winner-redo)
(add-hook 'after-init-hook 'winner-mode)

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(keymap-global-set "C-x 1" #'toggle-delete-other-windows)

;; default emacs configurations

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | { M-- M-l }  | Change case of preceding word|
;; | { C-M-f/b }  | Move by sexp                 |
;; | { C-M-d/u }  | Move into/out of lists       |
(add-hook 'after-init-hook 'auto-compression-mode)

(setopt kill-region-dwim 'emacs-word)

(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "C-h h" nil)
(keymap-global-set "M-SPC" 'cycle-spacing)
(keymap-global-set "C-c C-j" 'join-line)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "C-x M-k" 'kill-buffer-other-window)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-x (" 'kmacro-start-macro-or-insert-counter)
(keymap-global-set "C-x )" 'kmacro-end-or-call-macro)

(let ((map (make-sparse-keymap)))
  (keymap-set map "s" #'eshell)
  (keymap-set map "r" #'rgrep)
  (keymap-set map "m" #'gnus)
  (keymap-global-set "C-z" map))

(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-j" 'duplicate-dwim)
(keymap-global-set "M-g r" 'recentf)
(keymap-global-set "M-s g" 'grep)
(keymap-global-set "M-s f" 'find-name-dired)

(keymap-global-set "C-x w t" 'window-layout-transpose)
(keymap-global-set "C-x w r" 'rotate-windows)
(keymap-global-set "C-x w f h" 'window-layout-flip-leftright)
(keymap-global-set "C-x w f v" 'window-layout-flip-topdown)

(defun my/backward-kill-word-or-region ()
  "Kill region or word based on selection."
  (interactive)
  (call-interactively (if (region-active-p)
                          'kill-region
                        'backward-kill-word)))

(defun emacs-reload-configuration ()
  "Reload emacs configuration."
  (interactive)
  (load-file (locate-user-emacs-file "init.el")))

(defun kill-buffer-other-window ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1))

(setopt case-fold-search t)
(setopt load-prefer-newer t)
(setopt apropos-do-all t)
(setopt ad-redefinition-action 'accept)

;; bump this a bit from default 64kb
(setq read-process-output-max 524288) ; 512kb

(setq-default show-trailing-whitespace nil)
(setq-default require-final-newline t)
(setq-default cursor-type 'box)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 72)
(setq-default tab-always-indent 'complete)
(setq-default bidi-paragraph-direction 'left-to-right)

(setq redisplay-skip-fontification-on-input t)
(setq bidi-inhibit-bpa t)
(setq use-hard-newlines nil)
(setopt sentence-end-double-space nil)
(setopt sentence-end-without-period nil)
(setopt colon-double-space nil)

(setopt message-log-max 5000)

(setopt initial-scratch-message "")
(setopt inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "tmy")

(setopt visible-bell t)
(setopt window-min-height 3)

(setopt select-active-regions t)

;; disable dialog boxes
(setopt use-file-dialog nil)
(setopt use-dialog-box nil)

(defun my/help-mode-hook ()
  (setq truncate-lines t))

(add-hook 'help-mode-hook #'my/help-mode-hook)

(defvar *my-fixed-font* "Julia Mono")
(defvar *my-variable-font* "Input Serif")

(defun my/adjust-font-for-screen (frame)
  "Adjusts the font height based on the screen resolution."
  (if (> (x-display-pixel-width) 1600)
      (set-face-attribute 'default frame :height 130)
    (set-face-attribute 'default frame :height 110)))

(add-hook 'after-make-frame-functions 'my/adjust-font-for-screen)

(defun my/set-frame-fonts ()
  "My hook to setup frame fonts, useful for daemon mode."
  ;; Set Default font if present
  (when (find-font (font-spec :name *my-fixed-font*))
    (set-face-attribute 'default nil :family *my-fixed-font*)
    (set-face-attribute 'fixed-pitch nil :family *my-fixed-font*)
    (set-face-attribute 'tooltip nil :family *my-fixed-font*))
  (when (find-font (font-spec :name *my-variable-font*))
    (set-face-attribute 'variable-pitch nil :family *my-variable-font*)))

(add-hook 'server-after-make-frame-hook #'my/set-frame-fonts)

(unless (daemonp)
  (my/set-frame-fonts))

(setopt use-short-answers t)

;; Don't prompt if killing buffer with process attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; enabled disabled features
(dolist (s '(narrow-to-region
             upcase-region
             downcase-region
             dired-find-alternative-file
             overwrite-mode))
  (put s 'disabled nil))

;;; simple
(setopt set-mark-command-repeat-pop t)
(setopt next-line-add-newlines nil)
(setopt kill-do-not-save-duplicates t)
(setopt backward-delete-char-untabify-method nil)
(setopt yank-pop-change-selection t)
(setopt save-interprogram-paste-before-kill t)
(add-hook 'after-init-hook 'size-indication-mode)
(add-hook 'after-init-hook 'line-number-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(when (executable-find "xwallpaper")
  (setopt wallpaper-command "xwallpaper")
  (setopt wallpaper-command-args '("--maximize" "%f")))

;;; theme settings

(setopt modus-themes-disable-other-themes t)
(load-theme 'modus-operandi t)

(setopt image-use-external-converter t)

(provide 'init-visual)

;;; init-visual.el ends here
