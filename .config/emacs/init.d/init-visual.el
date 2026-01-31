;;; init-visual.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Init visual settings

;;; Code:

(global-font-lock-mode t)

(setq font-lock-maximum-decoration t)

(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren nil)
(add-hook 'after-init-hook 'show-paren-mode)

(blink-cursor-mode -1)

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | C-x 4 C-f | Find-file other-window       |
;; | C-x 4 d   | Dired other-window           |
;; | C-x 4 C-o | Display buffer other-window  |
;; | C-x 4 b   | Set buffer in other-window   |
;; | C-x 4 0   | Kill buffer and window       |
;; | C-x 4 p   | Run project cmd in window    |

;;  :commands winner-undo
(global-set-key (kbd "C-c w") 'winner-undo)
(global-set-key (kbd "C-c W") 'winner-redo)
(winner-mode)

;; default emacs configurations

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | { M-- M-l }  | Change case of preceding word|
;; | { C-M-f/b }  | Move by sexp                 |
;; | { C-M-d/u }  | Move into/out of lists       |
(add-hook 'after-init-hook 'auto-compression-mode)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-h h") nil)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-w") 'my/backward-kill-word-or-region)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-x M-k") 'kill-buffer-other-window)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x (") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-x )") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-z s") 'eshell)
(global-set-key (kbd "C-z r") 'rgrep)
(global-set-key (kbd "C-x C-c") nil) ;; save-buffers-kill-terminal

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-j") 'duplicate-dwim)
(global-set-key (kbd "M-g r") 'recentf)
(global-set-key (kbd "M-s g") 'grep)
(global-set-key (kbd "M-s f") 'find-name-dired)

(global-set-key (kbd "C-x w t") 'transpose-window-layout)
(global-set-key (kbd "C-x w r") 'rotate-windows)
(global-set-key (kbd "C-x w f h") 'flip-window-layout-horizontally)
(global-set-key (kbd "C-x w f v") 'flip-window-layout-vertically)

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

(defun become (&optional user)
  "Use TRAMP to open the current file or directory buffer with
different user account. By default the user is set to `root'."
  (interactive (list (if current-prefix-arg
                         (completing-read "User: " (system-users))
                       "root")))
  (let ((create-become-path
         (lambda (path)
           (let* ((remote-path (file-remote-p path))
                  (cmd (or (executable-find "doas" remote-path)
                           (executable-find "sudo" remote-path)))
                  (become-cmd (substring cmd -4))
                  (become-user (if user user "root")))
             (if (not remote-path)
                 (concat "/" become-cmd ":" become-user "@localhost:" path)
               (let ((file-parts (tramp-dissect-file-name path)))
                 (concat "/"
                         (tramp-file-name-method file-parts)
                         ":"
                         (tramp-file-name-user file-parts)
                         "@"
                         (tramp-file-name-host file-parts)
                         "|"
                         become-cmd
                         ":"
                         become-user
                         "@"
                         (tramp-file-name-host file-parts)
                         ":"
                         (tramp-file-name-localname file-parts))))))))
    (cond ((eq major-mode 'dired-mode)
           (dired (funcall create-become-path default-directory)))
          (buffer-file-name
           (find-alternate-file (funcall create-become-path buffer-file-name))))))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))
(menu-bar-mode -1)

(setq case-fold-search t)
(setq load-prefer-newer t)
(setq apropos-do-all t)
(setq ad-redefinition-action 'accept)

;; XXX: does this help with LSP stuff?
(setq read-process-output-max (* 1024 1024)) ; 1mb

(setq-default show-trailing-whitespace nil)
(setq-default require-final-newline t)
(setq-default cursor-type 'box)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 72)
(setq-default tab-always-indent 'complete)
(setq-default bidi-paragraph-direction 'left-to-right)

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

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'help-mode-hook (lambda () (setq truncate-lines t)))

(defvar *my-fixed-font* "Input Mono")
(defvar *my-variable-font* "Input Serif")

(defun my/set-frame-fonts ()
  "My hook to setup frame fonts, useful for daemon mode."
  (let ((my-fixed-font "Input Mono")
        (my-variable-font "Input Serif"))
    ;; Set Default font if present
    (when (find-font (font-spec :name *my-fixed-font*))
      (set-face-attribute 'default nil :family *my-fixed-font* :height 110)
      (set-face-attribute 'variable-pitch nil :family *my-variable-font*)
      (set-face-attribute 'fixed-pitch nil :family *my-fixed-font*)
      (set-face-attribute 'tooltip nil :family *my-fixed-font*))))

(add-hook 'server-after-make-frame-hook #'my/set-frame-fonts)

(unless (daemonp)
  (my/set-frame-fonts))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))

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
(setq save-interprogram-paste-before-kill nil)
(add-hook 'after-init-hook 'size-indication-mode)
(add-hook 'after-init-hook 'line-number-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(ensure-packages-present 'easy-kill)
(global-set-key [remap kill-ring-save] #'easy-kill)
(global-set-key [remap mark-sexp] #'easy-mark)

;; theme settings

(setq modus-themes-custom-auto-reload nil
      modus-themes-to-toggle '(modus-operandi modus-vivendi)
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui t
      modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-completions '((t . (extrabold)))
      modus-themes-prompts '(extrabold)
      modus-themes-headings
      '((agenda-structure . (variable-pitch light 2.2))
        (agenda-date . (variable-pitch regular 1.3))
        (t . (regular 1.15))))

(load-theme 'modus-vivendi t)

(setq image-use-external-converter t)

(provide 'init-visual)

;; init-visual.el ends here
