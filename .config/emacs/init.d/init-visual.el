;;; init-visual.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Init visual settings

;;; Code:

(global-font-lock-mode t)

(setq font-lock-maximum-decoration t)

(setq window-combination-resize t)

(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren nil)
(add-hook 'after-init-hook 'show-paren-mode)

(blink-cursor-mode -1)

;; help
(setq help-window-select t)

;; hide minor mode bindings by default to focus on major ones.
(setq describe-bindings-outline-rules
      '((match-regexp . "Key translations\\|Minor Mode Bindings")))

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | C-x 4 C-f | Find-file other-window       |
;; | C-x 4 d   | Dired other-window           |
;; | C-x 4 C-o | Display buffer other-window  |
;; | C-x 4 b   | Set buffer in other-window   |
;; | C-x 4 0   | Kill buffer and window       |
;; | C-x 4 p   | Run project cmd in window    |

(keymap-global-set "C-c w" 'winner-undo)
(keymap-global-set "C-c W" 'winner-redo)
(winner-mode)

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

(setq kill-region-dwim 'emacs-word)

(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "C-h h" nil)
(keymap-global-set "M-SPC" 'cycle-spacing)
;(keymap-global-set "C-w" 'my/backward-kill-word-or-region)
(keymap-global-set "C-c C-j" 'join-line)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "C-x M-k" 'kill-buffer-other-window)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-z" nil)
(keymap-global-set "C-x (" 'kmacro-start-macro-or-insert-counter)
(keymap-global-set "C-x )" 'kmacro-end-or-call-macro)
(keymap-global-set "C-z s" 'eshell)
(keymap-global-set "C-z r" 'rgrep)

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

(setq case-fold-search t)
(setq load-prefer-newer t)
(setq apropos-do-all t)
(setq ad-redefinition-action 'accept)

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
(setq sentence-end-double-space nil)
(setq sentence-end-without-period nil)
(setq colon-double-space nil)
(setq use-hard-newlines nil)

(setq message-log-max 5000)

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(setq visible-bell t)
(setq window-min-height 3)

(setq select-active-regions t)

;; disable dialog boxes
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(add-hook 'help-mode-hook (lambda () (setq truncate-lines t)))

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

(setq use-short-answers t)

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

;; simple
(setq set-mark-command-repeat-pop t)
(setq next-line-add-newlines nil)
(setq kill-do-not-save-duplicates t)
(setq backward-delete-char-untabify-method nil)
(setq yank-pop-change-selection t)
(setq save-interprogram-paste-before-kill t)
(add-hook 'after-init-hook 'size-indication-mode)
(add-hook 'after-init-hook 'line-number-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(ensure-packages-present 'easy-kill)
(define-key global-map [remap kill-ring-save] #'easy-kill)
(define-key global-map [remap mark-sexp] #'easy-mark)

;;; theme settings

(setq modus-themes-disable-other-themes t)
(load-theme 'modus-operandi t)

(setq image-use-external-converter t)

(provide 'init-visual)

;; init-visual.el ends here
