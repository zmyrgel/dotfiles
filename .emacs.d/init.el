;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2021-01-11 22:59:57 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 26.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - fix warnings on this file
;;; - elisp-hook
;;; - check desktop.el

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defconst elisp-dir (expand-file-name "elisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; avoid re-initializing packages
(unless package--initialized (package-initialize))

;; load of use-package to handle rest of package initialization.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-hook-name-suffix nil)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;; ------------------------------
;;; General
;;; ------------------------------

(use-package gnutls
  :config
  ;; silence gnutls warnings
  (setq gnutls-min-prime-bits nil
        gnutls-verify-error nil)
  ;; workaround for emacs bug, fixed in 26.3+
  (when (version<= emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; https://bugs.debian.org/766397
(when (version<= emacs-version "27")
  (setq tls-program '("gnutls-cli --x509cafile %t -p %p %h")))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

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

(use-package isearch
  :config
  (setq isearch-highlight t)
  (setq isearch-lazy-highlight t)
  (setq isearch-lazy-count t)
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq search-whitespace-regexp ".*?")
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(setq query-replace-highlight t)

;; mouse options
(use-package mouse
  :config
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  (setq mouse-drag-copy-region nil)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  :hook (after-init-hook . mouse-wheel-mode))

(use-package flyspell
  :commands (ispell-change-dictionary
             ispell-word
             flyspell-buffer
             flyspell-mode
             flyspell-region)

  ;;:hook text-mode-hook
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB"))

(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; (use-package whitespace
;;   :diminish
;;   :config (global-whitespace-mode))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))

;;; ------------------------------
;;; Text editing
;;; ------------------------------

;; | Key chord           | Description       |
;; |---------------------+-------------------|
;; | find-dired          | find + pattern    |
;; | find-name-dired     | find + name       |
;; | find-grep-dired     | find + grep       |
;; | find-lisp-find-dired| use emacs regexp  |

(use-package grep
  :config
  (when (version<= "27" emacs-version)
    (setq grep-find-use-xargs 'exec-plus)))

;; Check if I could use default binding of  C-x C-o instead of extra package.
;; (delete-blank-lines)
;; flush-lines, keep-lines -> useful when looking for example log file
;; On blank line, delete all surrounding blank lines, leaving just one.
;; On isolated blank line, delete that one.
;; On nonblank line, delete any immediately following blank lines.
(use-package hungry-delete
  :disabled
  :ensure t
  :diminish
  :config (global-hungry-delete-mode))

;; (use-package smartparens
;;   :diminish
;;   :ensure t
;;   :init (require 'smartparens-config)
;;   :config
;;   (smartparens-global-mode 1)
;;   (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
;;   (add-to-list 'sp-lisp-modes 'sly-mode)
;;   (sp-local-pair #'sly-mrepl-mode "'" nil :actions nil))

(use-package electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs '((8216 . 8217) (8220 . 8221) (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  ;;(setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  :hook ((after-init-hook . electric-indent-mode)
         ;;(after-init-hook . electric-pair-mode -1)
         ;;(after-init-hook . electric-quote-mode -1)
	 ))

(defun th/pdf-view-revert-buffer-maybe (file)
  (let ((buf (find-buffer-visiting file)))
    (when buf
  (with-current-buffer buf
    (when (derived-mode-p 'pdf-view-mode)
      (pdf-view-revert-buffer nil t))))))
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
    #'th/pdf-view-revert-buffer-maybe)

;; | Key chord      | Description     |
;; |----------------+-----------------|
;; | C-c [          | add cite        |
;; | C-c =          | show toc        |
(use-package tex
  :defer t
  :ensure auctex
  :hook ((latex-mode-hook . auto-fill-mode)
         (latex-mode-hook . reftex-mode)
         (tex-mode-hook . (lambda ()
                            (setq ispell-parser 'tex))))
  :init
  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
        (output-pdf "pdf-tools")
          (output-html "xdg-open")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-insert-braces nil)
  (setq TeX-electric-escape t)
  (setq TeX-electric-macro t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent))

;; doc-view / doc-view-presentation
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-links-minor-mode)
  (pdf-view-mode . pdf-isearch-minor-mode)
  (pdf-view-mode . pdf-outline-minor-mode)
  (pdf-view-mode . pdf-history-minor-mode)
  :config
  (setq pdf-view-display-size 'fit-page)
  (pdf-tools-install :no-query))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun my-nov-setup-hook ()
    (face-remap-add-relative 'variable-pitch :family "DejaVu Serif Book"
                             :height 1.0)
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$\\|\\.yaml$"
  :magic ("---" . yaml-mode))

(use-package nxml-mode
  :mode (("\\.plist\\'" . nxml-mode)
         ("\\.rss\\'"   . nxml-mode)
         ("\\.svg\\'"   . nxml-mode)
         ("\\.xml\\'"   . nxml-mode)
         ("\\.xsd\\'"   . nxml-mode)
         ("\\.xslt\\'"  . nxml-mode)
         ("\\.pom$"     . nxml-mode))
  :config
  (defun bf-pretty-print-xml-region (begin end)
  "Function formats XML elements in region between BEGIN and END."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

  ;; Any file start with xml will be treat as nxml-mode
  (add-to-list 'magic-mode-alist '("<\\?xml" . nxml-mode))
  ;; Use nxml-mode instead of sgml, xml or html mode.
  (mapc
   (lambda (pair)
     (if (or (eq (cdr pair) 'xml-mode)
             (eq (cdr pair) 'sgml-mode))
         (setcdr pair 'nxml-mode)))
   auto-mode-alist))

(use-package ansible-vault
  ;;; TODO: add vault-identity support
  :ensure t)

(use-package typescript-mode
  :ensure t
  :after flymake-eslint
  :hook ((typescript-mode-hook . eglot-ensure)
         (typescript-mode-hook . flymake-eslint-enable)))

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode))

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

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

;; (dolist (command '(scroll-up-command scroll-down-command
;;                                      recenter-top-bottom other-window))
;;   (advice-add command :after #'pulse-line))

;; default emacs configurations

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | M-- M-l   | Change case of preceding word|
;; | C-M-f/b   | Move by sexp                 |
;; | C-M-d/u   | Move into/out of lists       |
(use-package emacs
  :hook ((after-init-hook . auto-compression-mode)
         (focus-out-hook . garbage-collect))
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim)
         ("C-h h" . nil)
         ("M-SPC" . cycle-spacing)
         ("C-x C-k" . kill-region)
         ("C-w" . my/backward-kill-word-or-region)
         ("C-c C-j" . join-line)
         ("M-z" . zap-up-to-char)
         ("C-x k" . kill-this-buffer)
         ("M-o" . other-window)
         ("C-x C-z" . nil)
         ("C-z" . nil)
         ("C-z s" . eshell)
         ("C-z r" . rgrep)
         ("C-z m" . gnus)
         ("C-z b" . bookmark-bmenu-list))
  :config
  (defun my/backward-kill-word-or-region ()
    "Kill region or word based on selection."
    (interactive)
    (call-interactively (if (region-active-p)
                            'kill-region
                          'backward-kill-word)))

  (defun become ()
    "Use TRAMP to open the current buffer with elevated privileges."
    (interactive)
    (when buffer-file-name
      (let ((method (or (executable-find "doas")
                        (executable-find "sudo"))))
        (find-alternate-file
         (concat "/" method ":root@localhost:" buffer-file-name)))))

  ;; Tranlate-map C-x -> C-t, M-x -> M-t
  ;; apparently the best method
  ;; (define-key key-translation-map [?\C-x] [?\C-u])
  ;; (define-key key-translation-map [?\C-u] [?\C-x])

  ;; (global-set-key [?\C-.] 'execute-extended-command)
  ;; (global-set-key [?\C-,] (lookup-key global-map [?\C-x]))
  ;; (global-set-key [?\C-'] 'hippie-expand)

  ;; (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  ;; (global-set-key (kbd "\C-c\C-m") 'execute-extended-command) ; {from effective emacs}

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (menu-bar-mode t)

  (setq case-fold-search t)
  (setq load-prefer-newer t)

  (setq-default show-trailing-whitespace t)
  (setq-default require-final-newline t)
  (setq-default cursor-type 'box)
  (setq-default truncate-lines t)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 72)
  (setq-default tab-always-indent 'complete)

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

  ;; Set Default font if present
  (when (find-font (font-spec :name "Input Mono Narrow-12"))
    (set-face-attribute 'default nil :family "Input Mono Narrow" :height 120)
    (set-face-attribute 'variable-pitch nil :family "Input Serif")
    (set-face-attribute 'fixed-pitch nil :family "Input Mono Narrow")
    (set-face-attribute 'tooltip nil :family "Input Mono Narrow"))

  ;; Graphical Emacs seems to freeze when handling clipboard, so
  ;; decrease the selection timeout so it won't wait for so long.
  ;; https://omecha.info/blog/org-capture-freezes-emacs.html
  ;; XXX: why is this needed?
  (when (eq system-type 'berkeley-unix)
    (setq x-selection-timeout 10))

  (setq save-interprogram-paste-before-kill t)

  (setq select-enable-clipboard t)

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; enabled disabled features
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'overwrite-mode 'disabled t))

(use-package simple
  :config
  (setq set-mark-command-repeat-pop t)
  (setq next-line-add-newlines nil)
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

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config (load-theme 'gruvbox-dark-soft t nil))

;; Nice theme but doesn't set proper faces in org-mode
;; (use-package material-theme
;;   :ensure t
;;   :config (load-theme 'material t nil))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-eighties t nil))

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-fringes 'intense ; {nil,'subtle,'intense}
        modus-themes-mode-line '3d ; {nil,'3d,'moody}
        modus-themes-syntax nil ; Lots of options---continue reading the manual
        modus-themes-intense-hl-line nil
        modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
        modus-themes-links 'neutral-underline ; Lots of options---continue reading the manual
        modus-themes-prompts 'intense ; {nil,'subtle,'intense}
        modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}
        modus-themes-region 'bg-only-no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs nil ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks 'rainbow ; {nil,'grayscale,'rainbow}
        modus-themes-headings ; Lots of options---continue reading the manual
        '((1 . section)
          (2 . section-no-bold)
          (3 . rainbow-line)
          (t . rainbow-line-no-bold))
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t)
  :config
  (modus-themes-load-vivendi))

;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(use-package diary-lib
  :hook ((diary-list-entries-hook . diary-include-other-diary-files)
         (diary-list-entries-hook . diary-sort-entries)
         (diary-list-entries-hook . diary-mark-included-diary-files))
  :config
  (setq diary-display-function 'diary-fancy-display)
  (setq diary-number-of-entries 7))

(use-package calendar
  :hook (calendar-today-visible-hook . calendar-mark-today)
  :init (setq calendar-date-style 'european)
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-day-name-array
        ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
         "torstai" "perjantai" "lauantai"])
  (setq calendar-month-name-array
        ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
         "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
         "lokakuu" "marraskuu" "joulukuu"])

  (setq calendar-mark-holidays-flag t)
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-mark-diary-entries-flag t)
  (setq diary-show-holidays-flag t)
  (setq diary-file (expand-file-name "diary" user-emacs-directory)))

(use-package solar
  :config
  (setq calendar-latitude 60.29414
        calendar-longitude 25.04099))

;; time utilities
(use-package time-stamp
  :config
  (setq time-stamp-active t)
  (setq time-stamp-line-limit 10)
  (setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)"))

(use-package time
  :config
  (display-time-mode -1)
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date nil)
  (setq display-time-format nil)
  (setq display-time-use-mail-icon t))

(use-package suomalainen-kalenteri
  :ensure t)

;;; ------------------------------
;;; Session
;;; ------------------------------

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "places" user-emacs-directory))
  (save-place-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-max-saved-items 50)
  :hook (after-init-hook . recentf-mode))

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq history-length 30000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (setq savehist-autosave-interval 60)
  :hook (after-init-hook . savehist-mode))

(use-package abbrev
  :hook (kill-emacs-hook . write-abbrev-file)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (setq save-abbrevs t)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

(use-package files
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  :config
  (setq view-read-only t)
  (setq large-file-warning-threshold 50000000) ;; 50mb
  (setq backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setq make-backup-files t)
  (setq backup-by-copying t)
  (setq mode-require-final-newline t)
  (setq require-final-newline t))

;;; ------------------------------
;;; Shell settings
;;; ------------------------------

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (eq system-type 'berkeley-unix)
    (setq exec-path-from-shell-arguments '("-l")))
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH"
          "JAVA_HOME" "GOPATH"
          "GERBIL_HOME" "CVSROOT"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package sh-script
  :config (defun my/sh-mode-hook ()
            (set (make-local-variable 'indent-tabs-mode) t))
  :hook (sh-mode-hook . my/sh-mode-hook))

(use-package shell
  :hook (shell-mode-hook . ansi-color-for-comint-mode-on))

(use-package comint
  :config
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq comint-completion-addsuffix t)
  (setq comint-prompt-read-only t))

(use-package eshell
  :config
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  (setq eshell-save-history-on-exit t)
  (setq eshell-scroll-show-maximum-output t)
  (setq eshell-scroll-to-bottom-on-output t))

;;; customization for term, ansi-term
;; disable cua and transient mark modes in term-char-mode
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'transient-mark-mode) t))
(ad-activate 'term-line-mode)

(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)

(use-package ssh-tunnels
  :ensure t
  :config
  (setq ssh-tunnels-configurations
        '((:name "core-dev-db" :type "SH" :login "core-dev-db")
          (:name "core-prod-db" :type "SH" :login "core-prod-db")
          (:name "kube-prod" :type "SH" :login "kube-prod")
          (:name "kube-test" :type "SH" :login "kube-test"))))

;;; ------------------------------
;;; Org-mode
;;; ------------------------------

;; (defun zmg/org-file-sync ()
;;   "Sync org-files to/from remote server."
;;   (async-shell-command  (concat "rsync -v " org-directory ))
;;   )

(use-package org
  ;;:ensure org-plus-contrib ;; for confluence export
  :demand t
  :config
  (setq org-directory "~/Org")
  (setq org-default-notes-file "~/Org/notes.org")
  (setq org-agenda-files '("~/Org"))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'note)
  (setq org-startup-indented t)
  (setq org-special-ctrl-a/e t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                            (sequence "|" "CANCELED(c@/!)")
                            (sequence "STALLED(s@/!)" "|")
                            (sequence "PENDING(p@/!)" "|")))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?B)
  (setq org-tag-alist ;; use these or set file tags?
        '(("work" . ?w)
          ("emacs" . ?e)
          ("school" . ?s)
          ("thesis" . ?t)
          ("mail" . ?m)))
  ;; allow shell execution
  (setq org-babel-load-languages
        '((emacs . t)
          (shell . t)))
  (setq org-confirm-babel-evaluate t)
  (setq org-log-done 'note)
  (setq org-log-note-clock-out t)
  (setq org-read-date-prefer-future t)
  (setq org-adapt-indentation nil)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 3)
  :hook ((org-mode-hook . flyspell-mode)
         (org-mode-hook . variable-pitch-mode)
         (org-mode-hook . visual-line-mode)
         (message-mode-hook . turn-on-orgtbl)))

(use-package ol
:config
(setq org-link-keep-stored-after-insertion t)
:bind (("C-c l" . org-store-link)
       :map org-mode-map
       ("C-c L" . org-toggle-link-display)
       ("C-c C-y" . org-insert-last-stored-link)))

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("s" "School work" entry (file+headline "school.org" "School work")
           "* TODO %?\n  %i\n  %a")
          ("w" "Work tasks" entry (file+headline "work.org" "Work tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Notes" entry (file+datetree "notes.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  (setq org-capture-templates-contexts
        '(("r" ((in-mode . "gnus-article-mode")
                (in-mode . "gnus-summary-mode")))))
  :bind ("C-c c" . org-capture))

(use-package org-agenda
  :after org
  :config
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time t)
  (setq org-agenda-include-diary t)
  ;;(setq org-agenda-start-with-follow-mode t)
  (setq org-agenda-follow-indirect t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 7)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  :bind (("C-c a" . org-agenda)))

(use-package org-src
  :after org
  :config
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0))

(use-package ox
  :after org
  :config
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 3)
  (setq org-export-dispatch-use-expert-ui nil))

(use-package ox-latex
  :after ox
  :config
  (add-to-list 'org-latex-classes
               '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               t))

;;; ------------------------------
;;; Buffer management
;;; ------------------------------

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-separator ":")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package ibuffer
  :init (defalias 'list-buffers 'ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-expert t)
  :hook (ibuffer-mode-hook . ibuffer-auto-mode))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :config
  ;; sort buffer list by repositories
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  ;; show file vc status in buffer list
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;; Don't prompt if killing buffer with process attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; -----------------------------
;;; IRC
;;; ------------------------------

(use-package rcirc
  :config
  (setq rcirc-server-alist
        '(("irc.freenode.net"
           :channels ("#openbsd" "#lisp"))))
  (setq rcirc-default-nick "zmyrgel")
  (setq rcirc-default-user-name "zmyrgel")
  (setq rcirc-default-full-name "Curious Minds Want To Know")

  (let ((nickserv-pass (secrets-get-secret "default" "freenode-pass")))
    (when nickserv-pass
      (setq rcirc-authinfo
            `(("freenode" nickserv "zmyrgel" ,nickserv-pass)))))

  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (setq rcirc-time-format "%Y-%m-%d %H:%M ")
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)))

(use-package erc
  :hook ((erc-mode-hook . erc-services-mode)
         (erc-mode-hook . erc-autojoin-mode)
         (erc-mode-hook . erc-match-mode)
         (erc-mode-hook . erc-track-mode)
         (erc-mode-hook . erc-fill-mode)
         (erc-mode-hook . erc-ring-mode)
         (erc-mode-hook . erc-netsplit-mode)
         (erc-mode-hook . erc-timestamp-mode)
         (erc-mode-hook . erc-spelling-mode)
         (erc-mode-hook . erc-notify-mode)
         (erc-mode-hook . erc-pcomplete-mode)
         (erc-mode-hook . erc-log-mode)
         (erc-insert-post-hook . erc-save-buffer-in-logs)
         (erc-insert-post-hook . erc-truncate-buffer))
  :config

  (setq erc-modules (append erc-modules '(services notify spelling log)))
  (erc-update-modules)

  (setq erc-prompt-for-password nil)
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit nil)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-auto-query 'window-noselect)
  (setq erc-keywords '("zmyrgel" "tmy"))

  (setq erc-track-enable-keybindings t)
  (setq erc-track-remove-disconnected-buffers t)
  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-timestamp-format "[%R-%m/%d]")
  (setq erc-hide-timestamps nil)

  (pcomplete-erc-setup)

  (setq erc-pcomplete-order-nickname-completions t)
  (setq erc-log-channels-directory (expand-file-name ".irclogs" "~"))
  (setq erc-log-insert-log-on-open nil)
  (setq erc-log-file-coding-system 'utf-8-unix)
  (setq erc-save-buffer-on-part t)
  (setq erc-max-buffer-size 20000)
  (setq erc-truncate-buffer-on-save t)
  (defvar erc-insert-post-hook nil))

;;; ------------------------------
;;; Email settings
;;; ------------------------------

(setq user-mail-address "timo.myyra@bittivirhe.fi")
(setq user-full-name "Timo Myyrä")

(use-package smtpmail
  :config
  (setq smtpmail-default-smtp-server "smtp.fastmail.com")
  (setq smtpmail-smtp-server         "smtp.fastmail.com")
  (setq smtpmail-local-domain        "bittivirhe.fi")
  (setq smtpmail-smtp-service        465)
  (setq smtpmail-stream-type         'ssl)
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-send-mail-function 'smtpmail-send-it))

(use-package message
  :config
  (setq mail-user-agent 'message-user-agent)
  (setq message-mail-user-agent nil)    ; default is `gnus'
  (setq compose-mail-user-agent-warnings nil)
  (setq message-citation-line-format "%f [%Y-%m-%d, %R %z]:\n")
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (setq message-default-charset 'utf-8)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))
  :hook ((message-setup-hook . message-sort-headers)))

;; gnus
(use-package gnus
  :config
  (setq gnus-treat-hide-citation t)
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-cited-lines-visible '(0 . 5))
  (setq gnus-always-read-dribble-file t)
  (setq mm-inline-large-images 'resize)
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq mm-text-html-renderer 'shr)

  (setq gnus-select-method '(nntp "news.gmane.io"))
  (setq gnus-secondary-select-methods
        '((nnimap "work-gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "993")
                  (nnimap-stream ssl))
          (nnimap "imap-metro"
                  (nnimap-address "imap.metropolia.fi")
                  (nnimap-server-port "993")
                  (nnimap-stream tls))
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnimap-stream tls))))
  :bind ("C-c m" . gnus))

(use-package gnus-art
  :config
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^Subject:" "^To:" "^Cc:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers))

(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

(use-package nnmail
  :config
  (setq nnmail-expiry-wait 30))

(use-package gnus-agent
  :after gnus
  :config
  (setq gnus-agent-expire-days 30))

(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode-hook . gnus-dired-mode))

;;; ------------------------------
;;; Web Browsing settings
;;; ------------------------------

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)

  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml"
          "https://news.ycombinator.com/rss"
          "http://www.tedunangst.com/flak/rss"
          "https://undeadly.org/cgi?action=rss"
          "https://www.phoronix.com/rss.php"
          "http://planetsysadmin.com/atom.xml"
          ("http://oremacs.com/atom.xml" emacs)
          ("http://emacsblog.org/feed/" emacs)
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("https://www.masteringemacs.org/feed" emacs)
          "https://scripter.co/posts/atom.xml"
          ("https://oneofus.la/have-emacs-will-hack/feed.xml" emacs)
          ("https://updates.orgmode.org/feed/changes" emacs org)
          ("https://www.reddit.com/r/emacs.rss" emacs reddit)
          ("https://www.reddit.com/r/orgmode.rss" reddit emacs org)
          ("https://xkcd.com/atom.xml" xkcd)
          ("https://planet.lisp.org/rss20.xml" lisp)
          "https://lobste.rs/t/emacs.lisp.security.ask.ai.openbsd.programming.rss")))


;; | M-s M-w | eww-search-words       |
(use-package eww
  :commands (eww
             eww-browse-url
             eww-search-words
             eww-open-in-new-buffer
             eww-open-file
             prot/eww-visit-history)
  :config
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "Downloads" "~"))
  (setq eww-suggest-uris
        '(eww-links-at-point thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (expand-file-name "eww-bookmarks" user-emacs-directory))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/ogg\\|pdf\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  :bind (:map eww-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("B" . eww-back-url)
              ("N" . eww-next-url)
              ("P" . eww-previous-url)))

(use-package browse-url
  :after eww
  :config
  (setq browse-url-new-window-flag nil)
  (setq browse-url-firefox-new-window-is-tab t)
  (setq browse-url-browser-function 'eww-browse-url))

;;; ------------------------------
;;; Completion
;;; ------------------------------

(use-package icomplete-vertical
  :ensure t
  :config
  (setq icomplete-vertical-prospects-height 10))

(use-package icomplete
  :after (minibuffer icomplete-vertical)
  :demand t
  :config
  (setq icomplete-delay-completions-threshold 100)
  (setq icomplete-max-delay-chars 2)
  (setq icomplete-compute-delay 0.2)
  (setq icomplete-prospects-height 1)
  (setq icomplete-in-buffer t)
  (setq icomplete-separator " | ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-tidy-shadowed-file-names t)
  (setq icomplete-show-matches-on-no-input nil)
  (setq icomplete-hide-common-prefix nil)
  (setq completion-flex-nospace nil)
  (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<return>" . icomplete-force-complete-and-exit)
              ("M-t" . icomplete-force-complete)
              ("C-j" . exit-minibuffer)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-s" . icomplete-forward-completions)
              ("C-r" . icomplete-backward-completions)
              ("<C-backspace>" . icomplete-fido-backward-updir)
              ("DEL" . icomplete-fido-backward-updir)
              ("C-v" . icomplete-vertical-toggle)))

(use-package minibuffer
  :config
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq minibuffer-beginning-of-buffer-movement t)
  (setq completions-format 'vertical)
  (setq completion-show-help nil)
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows 'grow-only)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  :bind (:map completion-list-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . next-completion)
              ("b" . previous-completion)))

(use-package imenu
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-max-item-length 100)
  (setq imenu-space-replacement ".")
  (setq imenu-level-separator ":"))

(use-package dabbrev
  :config
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search 'case-fold-search)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t)
  :bind (("C-," . dabbrev-expand)
         ("C-." . dabbrev-completion)))

(use-package hippie-expand
  :bind ("C-'" . hippie-expand))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode-hook . company-mode)
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))

;;; ------------------------------
;;; File and directory management
;;; ------------------------------

(use-package dired
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :hook ((dired-mode-hook . hl-line-mode)
         (dired-mode-hook . dired-hide-details-mode))
  :config
  (require 'dired-x)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-isearch-filenames t)
  (setq dired-ls-F-marks-symlinks t)
  ;; Don't pass --dired flag to ls on BSD
  (when (eq system-type 'berkeley-unix)
    (setq dired-use-ls-dired nil))

  (setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")
  (setq dired-guess-shell-alist-user
        '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "cvlc"
           "\\.rar$" "unrar e")))
  (setq dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                   "tar")))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (unless (version<= emacs-version "27")
    (setq dired-create-destination-dirs 'ask)
    (setq dired-vc-rename-file t)))

(use-package bongo
  :ensure t
  :defer t
  :hook (bongo-player-started-hook . bongo-no-autoplay-video)
  :bind (("<C-XF86AudioPlay>" . bongo-pause/resume)
         ("<C-XF86AudioNext>" . bongo-next)
         ("<C-XF86AudioPrev>" . bongo-previous)
         ("<M-XF86AudioPlay>" . bongo-show)
         ("C-z B" . bongo)
         :map bongo-playlist-mode-map
         ("n" . bongo-next-object)
         ("p" . bongo-previous-object)
         ("R" . bongo-rename-line)
         ("j" . bongo-dired-line)
         ("J" . dired-jump)
         ("I" . bongo-insert-special))
  :config
  (setq bongo-default-directory (expand-file-name "Music" "~"))
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(vlc mpv))
  (setq bongo-vlc-program-name "cvlc")

  (defun bongo-no-autoplay-video ()
    "don't autoplay next track if playing video"
    (with-bongo-playlist-buffer
     (when (bongo-video-file-name-p
            (bongo-player-get bongo-player 'file-name))
       (setq bongo-next-action 'bongo-stop))))

  (setq bongo-custom-backend-matchers
        `((vlc
           (local-file "file:" "http:" "ftp:")
           "ogg" "flac" "mp3" "m4a" "mka" "wav" "wma"
           "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "m4v"
           "mkv" "flv" "mov" "asf" "wmv" "rm" "rmvb" "ts"))))

;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

(use-package vc
  :config
  (setq vc-suppress-confirm t)
  (setq vc-command-messages t))

;; (use-package vc-got
;;   :load-path "/home/tmy/.emacs.d/elisp/vc-got/"
;;   :init (push 'Got vc-handled-backends))

(use-package compile
  :config
  (setq compilation-save-buffers-predicate nil)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (setq compilation-window-height 12))

;; or use smerge-ediff to resolve conflicts
(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package diff-mode
  :config
  (setq diff-font-lock-prettify nil))

(use-package diff
  :config (setq diff-switches '("-u")))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  :hook (ediff-after-quit-hook-internal-hook . winner-undo))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package prog-mode
  :config
  (defun my/prog-mode-hook ()
    "Hook to run when entering generic prog-mode."
    (set (make-local-variable 'which-func-unknown) "TOP LEVEL")
    (set (make-local-variable 'whitespace-line-column) 80)
    (set (make-local-variable 'whitespace-style) '(face lines-tail))
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                   1 font-lock-warning-face prepend))))
  :hook ((prog-mode-hook . electric-pair-mode)
         (prog-mode-hook . whitespace-mode)
         (prog-mode-hook . which-function-mode)
         (prog-mode-hook . my/prog-mode-hook)))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config (setq magit-repository-directories
                '(("~/git" . 1)
                  ("~/quicklisp/local-projects" . 1))))

(use-package eglot
  :ensure t
  :bind ((:map eglot-mode-map
               ("C-c h" . eglot-help-at-point))))

(use-package flymake
  :ensure t)

(use-package flymake-eslint
  :ensure t
  :config
  (setq flymake-eslint-executable-name "eslint")
  (setq flymake-eslint-executable-args nil)
  (setq flymake-eslint-show-rule-name t)
  (setq flymake-eslint-defer-binary-check t))

;;; Go programming

(use-package go-mode
  :ensure t
  :after eglot
  :hook ((before-save-hook . gofmt-before-save)
         (go-mode-hook . eglot-ensure))
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c g i" . go-goto-imports)
              ("C-c C-k" . godoc)))

(use-package go-eldoc
  :ensure t
  :hook (go-mode-hook . go-eldoc-setup))

;;; Ruby

(use-package rvm
  :ensure t
  :config (rvm-use-default))

(use-package ruby-mode
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"  . ruby-mode))
  :init
  (defun my/ruby-mode-hook ()
    (setq ruby-deep-arglist t)
    (setq ruby-deep-indent-paren nil)
    (setq c-tab-always-indent nil))
  :hook (ruby-mode-hook . my/ruby-mode-hook))

;;; Lisp programming

(use-package sly
  :ensure t
  ;;:hook (sly-mode-hook . lisp-mode)
  :config
  (let ((sbcl-bin-path (expand-file-name "lib/sbcl" "~")))
    (when (file-exists-p sbcl-bin-path)
      (setenv "SBCL_HOME" sbcl-bin-path)))
  (setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
                                   (ecl ("ecl"))
                                   (clisp ("clisp" "-ansi"))
                                   (chicken ("csi"))
                                   (abcl ("abcl"))))

  (setq common-lisp-hyperspec-symbol-table
        (concat
         (cond ((file-directory-p "/usr/local/share/doc/clisp-hyperspec")
                "file:/usr/local/share/doc/clisp-hyperspec/")
               ((file-directory-p "~/lisp/docs/HyperSpec")
                (concat "file:" (getenv "HOME") "/lisp/docs/HyperSpec/"))
               (t "http://www.lispworks.com/documentation/HyperSpec/"))
         "Data/Map_Sym.txt")))

(use-package sly-repl-ansi-color
  :ensure t
  :after sly
  :config (sly-enable-contrib 'sly-repl-ansi-color))

(use-package quack
  :disabled
  :ensure t
  :config
  (setq quack-default-program "csi")
  (setq quack-dir (concat user-emacs-directory "quack/"))
  (setq quack-fontify-style nil)
  (setq quack-newline-behavior 'indent-newline-indent)
  (setq quack-pretty-lambda-p nil)
  (setq quack-remap-find-file-bindings-p nil)
  (setq quack-run-scheme-always-prompts-p nil)
  (setq quack-run-scheme-prompt-defaults-to-last-p t)
  (setq quack-smart-open-paren-p t)
  (setq quack-switch-to-scheme-method 'other-window))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj$")

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-lein-parameters "repl :headless :host localhost")
  (setq nrepl-hide-special-buffers t))

(use-package geiser
  :disabled
  :ensure t
  :config
  (when (eq system-type 'berkeley-unix)
    (setq geiser-chicken-binary "chicken-csi")
    (setq geiser-guile-binary "guile2")))

;;;; PHP programming

(use-package composer
  :ensure t
  :defer t)

(use-package company-php
  :ensure t)

(use-package php-mode
  :ensure t
  :after company-php
  :mode "\\.php[345]?\\'\\|\\.phtml\\'"
  :config
  (defun my/php-mode-hook ()
    (setq php-site-url "http://fi2.php.net/")
    (php-enable-symfony2-coding-style)
    (define-abbrev php-mode-abbrev-table "ex" "extends")
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq c-basic-offset 4))
  :hook (php-mode-hook . my/php-mode-hook))

;;;; C programming

(use-package cc-mode
  :bind (:map c-mode-map
              ("C-c m" . man-follow)
              ("C-c C-d" . gdb)
              ("C-m" . c-context-line-break)
              ("C-c o" . ff-find-other-file))
  :hook ((c-mode-common-hook . which-function-mode)
         (c-mode-common-hook . cwarn-mode)
         (c-mode-hook . 'my/c-mode)
         (c++-mode-hook . 'my/c-mode))
  :config
  (defun my/c-mode ()
    "My C programming options."
    (c-set-style "bsd"))
  (defun my/c++-mode ()
    "My C++ programming options."
    (setq fill-column 100)
    (c-set-style "stroustrup")
    (setq whitespace-line-column 100
          whitespace-style '(face lines-tail))))

(use-package cperl-mode
  :init
  (defalias 'perl-mode 'cperl-mode)
  :config
  (defun my/cperl-mode-hook ()
    "Default CPerl settings."
    (setq cperl-fontlock t)
    (setq cperl-info-on-command-no-prompt t)
    (setq cperl-clobber-lisp-bindings t)
    (setq cperl-lazy-help-time 5)
    (setq cperl-indent-level 4)
    (setq cperl-invalid-face 'default))
  :hook (cperl-mode-hook . my/cperl-mode-hook))

(use-package web-mode
  :ensure t
  :mode (("\\.jsp\\'" . web-mode)
         ("\\.ap[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (defun my/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4)
    (when (and (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
               (require 'eglot nil 'noerror))
      (eglot-ensure)))
  (when (require 'flycheck nil 'noerror)
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))
  :hook (web-mode-hook . my/web-mode-hook))

(use-package flycheck
  :disabled
  :ensure t
  :diminish flycheck-mode
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq flycheck-phpcs-standard "Zend")
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;;; ------------------------------
;;; Finalizers
;;; ------------------------------

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Load optional local startup files
(load (expand-file-name "init-local.el" user-emacs-directory) t t)

;; Only start server mode for non-admin accounts
(unless (string-equal "root" (getenv "USER"))
  (when (and (fboundp 'server-running-p)
             (server-running-p))
    (server-start)))

(provide 'init)

;;; init.el ends here
