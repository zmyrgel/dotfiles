;;; ------------------------------
;;; Visual settings
;;; ------------------------------

(use-package font-core
  :config (global-font-lock-mode t))

(use-package font-lock
  :config (setq font-lock-maximum-decoration t))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

(use-package frame
  :config (blink-cursor-mode -1))

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | C-x 4 C-f | Find-file other-window       |
;; | C-x 4 d   | Dired other-window           |
;; | C-x 4 C-o | Display buffer other-window  |
;; | C-x 4 b   | Set buffer in other-window   |
;; | C-x 4 0   | Kill buffer and window       |
;; | C-x 4 p   | Run project cmd in window    |

(use-package winner
  :commands winner-undo
  :bind
  (("C-c w" . winner-undo)
   ("C-c W" . winner-redo))
  :config
  (winner-mode))

;; default emacs configurations

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | M-- M-l   | Change case of preceding word|
;; | C-M-f/b   | Move by sexp                 |
;; | C-M-d/u   | Move into/out of lists       |
(use-package emacs
  :hook ((after-init-hook . auto-compression-mode))
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim)
         ("C-h h" . nil)
         ("M-SPC" . cycle-spacing)
         ("C-w" . my/backward-kill-word-or-region)
         ("C-c C-j" . join-line)
         ("M-z" . zap-up-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-x C-z" . nil)
         ("C-z" . nil)
         ("C-z s" . eshell)
         ("C-z r" . rgrep))
  :config
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

  ;; FIXME: remote tramp uses multihop
  ;;/ssh:homer@powerplant|sudo:powerplant:/root/stuff.txt
  ;; /ssh:user@foo.example.fi|sudo:root@foo.example.fi:/path/to/file
  ;; FIXME: make this work for dired buffers too for remote admin tasks
  (defun become ()
    "Use TRAMP to open the current buffer with elevated privileges."
    (interactive)
    (when buffer-file-name
      (let* ((cmd (or (executable-find "doas")
                      (executable-find "sudo")))
             (method (substring cmd -4)))
        (find-alternate-file
         (concat "/" method ":root@localhost:" buffer-file-name)))))

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

  ;; keep a little more history to see whats going on
  (setq message-log-max 16384)

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

  (add-hook 'help-mode-hook (lambda () (setq truncate-lines t)))

  ;; ;; Set Default font if present
  (when (find-font (font-spec :name "Input Mono Narrow"))
    (set-face-attribute 'default nil :family "Input Mono" :height 120)
    (set-face-attribute 'variable-pitch nil :family "Input Serif")
    (set-face-attribute 'fixed-pitch nil :family "Input Mono Narrow")
    (set-face-attribute 'tooltip nil :family "Input Mono Narrow"))

  (defalias 'yes-or-no-p 'y-or-n-p)

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
    (put s 'disabled nil)))

(use-package simple
  :config
  (setq set-mark-command-repeat-pop t)
  (setq next-line-add-newlines nil)
  (setq kill-do-not-save-duplicates t)
  (setq backward-delete-char-untabify-method nil)
  (setq kill-ring-max 100)
  (setq yank-pop-change-selection t)
  (setq save-interprogram-paste-before-kill t)
  :hook ((after-init-hook . size-indication-mode)
         (after-init-hook . line-number-mode)
         (after-init-hook . column-number-mode)
         (text-mode-hook . auto-fill-mode)
         (before-save-hook . delete-trailing-whitespace)))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package diminish
  :ensure t
  :after use-package)

;; theme settings
(use-package emacs
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-mixed-fonts t)
  (setq moduls-themes-headings
        '((1 . (background overline variable-pitch 1.5))
          (2 . (overline rainbow 1.3))
          (3 . (overline 1.1))
          (t . (monochrome))))
  (setq moduls-themes-headings nil)
  (setq modus-themes-fringes 'intense)
  (setq modus-themes-org-blocks nil)
  (setq modus-themes-mode-line '(borderless accented))
  (setq modus-themes-diffs nil)
  (setq modus-themes-completions '((matches . (extrabold background))
                                   (selection . (semibold accented))
                                   (popup . (accented))))
  (setq modus-themes-completions nil)
  (setq modus-themes-prompts nil)
  (setq modus-themes-hl-line '(accented intense))
  (setq modus-themes-subtle-line-numbers nil)
  (setq modus-themes-markup nil)
  (setq modus-themes-paren-match '(bold))
  (setq modus-themes-syntax '(yellow-comments green-strings))
  (setq modus-themes-links '(bold italic))
  (setq modus-themes-region '(accented))
  (setq modus-themes-mail-citations '(intense))
  (load-theme 'modus-vivendi t))

(provide 'zmg-emacs-visual)
