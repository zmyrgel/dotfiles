;;; init-general.el --- General setup of Emacs -*- lexical-binding: t -*-

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

;; General emacs configuration

;;; Code:

(defun password-lookup (&rest keys)
  "Lookup password from auth-sources filtered by given KEYS."
  (when-let* ((result (apply #'auth-source-search keys)))
    (funcall (plist-get (car result) :secret))))

(dolist (p '("~/bin" "~/.local/bin" "~/workspace/bin" "~/opt/bin"))
  (add-to-list 'exec-path (expand-file-name p)))

(add-hook 'after-init-hook 'delete-selection-mode)

(setopt mode-line-collapse-minor-modes
        '(abbrev-mode flyspell-mode flyspell-prog-mode eldoc-mode
                      subword-mode auto-revert-mode whitespace-mode
                      completion-preview-mode))

(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent))

;;; global keybindings
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

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | C-s C-w   | Search char or word at point |
;; | M-s .     | Similar, but broader match   |
;; | M-s o     | Run `occur' on regexp        |
;; | M-s h r   | Highlight regexp             |
;; | M-s h u   | Undo the highlight           |
;; | C-s M-r   | Toggle regexp search         |
;; | M-%       | Run `query-replace'          |
;; | C-M-%     | `query-replace-regexp'       |

(setopt isearch-lazy-highlight t)
(setopt isearch-lazy-count t)
(setopt isearch-lax-whitespace t)
(setopt search-whitespace-regexp ".*?")
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format " (%s/%s)")
(setopt isearch-yank-on-move 'shift)
(setopt isearch-allow-scroll 'unlimited)
(setopt query-replace-highlight t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-highlight t) ;; not documented?

;; mouse options
(setopt mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
(setopt mouse-drag-copy-region nil)
(setopt make-pointer-invisible t)
(setopt mouse-yank-at-point t)
(setopt mouse-wheel-progressive-speed t)
(setopt mouse-wheel-follow-mouse t)
(add-hook 'after-init-hook 'context-menu-mode)
(add-hook 'after-init-hook 'mouse-wheel-mode)
(add-hook 'after-init-hook 'pixel-scroll-precision-mode)

(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(global-so-long-mode 1)

(setopt repeat-on-final-keystroke t)
(setopt set-mark-command-repeat-pop t)
(add-hook 'after-init-hook 'repeat-mode)

;; bump undo limits a bit
(setopt undo-limit (* 13 160000))
(setopt undo-strong-limit (* 13 240000))
(setopt undo-outer-limit (* 13 24000000))

;;; ------------------------------
;;; Buffer management
;;; ------------------------------

;; M-x rename-visited-file

;;; uniquify
(setopt uniquify-buffer-name-style 'post-forward-angle-brackets)
(setopt uniquify-separator ":")
(setopt uniquify-after-kill-buffer-p t)
(setopt uniquify-ignore-buffers-re "^\\*")

;;; ibuffer
(keymap-global-set "C-x C-b" 'ibuffer)
(setopt ibuffer-default-sorting-mode 'major-mode)
(setopt ibuffer-expert t)
(setopt ibuffer-human-readable-size t)
(setopt ibuffer-default-shrink-to-minimum-size t)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

;;; Buffer-menu
(setopt Buffer-menu-group-by '(Buffer-menu-group-by-root))

;;; buffer switching
(setopt switch-to-prev-buffer-skip-regexp nil)
(keymap-set ctl-x-x-map "p" #'switch-to-prev-buffer)
(keymap-set ctl-x-x-map "n" #'switch-to-next-buffer)

;;; delete pair
(setopt delete-pair-blink-delay 0)
(setopt delete-pair-push-mark t)
(keymap-global-set "M-s d" #'delete-pair)

;;; registers
;; buffers to registers, C-x r j m
;; {C-u 99 C-x e} run macro for 99 times
;; {C-u 0 C-x e} run macro until bell
(set-register ?m '(buffer . "*Messages*"))

;;; man pages
(setopt Man-prefer-synchronous-call t)
(setopt Man-support-remote-systems t)

;;; remember
(setopt remember-data-file (expand-file-name "~/Documents/notes"))
(setopt remember-notes-initial-major-mode 'org-mode)

;;; proced
(setopt proced-enable-color-flag t)
(setopt proced-tree-flag t)
(setopt proced-auto-update-flag nil)
(setopt proced-auto-update-interval 5)
(setopt proced-filter 'user)
(setopt proced-descend t)
(setopt proced-show-remote-processes t)

(provide 'init-general)

;;; init-general.el ends here
