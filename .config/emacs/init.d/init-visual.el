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

(defun kill-buffer-other-window ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1))

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
(setopt inhibit-startup-echo-area-message user-login-name)

(setopt visible-bell t)
(setopt window-min-height 3)

(setopt select-active-regions t)

;; disable dialog boxes
(setopt use-file-dialog nil)
(setopt use-dialog-box nil)

(defun my/truncate-lines-hook ()
  (setq-local trruncate-lines t))

(add-hook 'help-mode-hook #'my/truncate-lines-hook)

(defvar *my-fixed-font* "Input Mono")
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

(when (executable-find "xwallpaper")
  (setopt wallpaper-command "xwallpaper")
  (setopt wallpaper-command-args '("--maximize" "%f")))

;;; theme settings

(setopt modus-themes-disable-other-themes t)
(load-theme 'modus-operandi t)

(setopt image-use-external-converter t)

(provide 'init-visual)

;;; init-visual.el ends here
