;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2022-02-08 22:45:20 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 28.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - fix warnings on this file

;;; Code:

;; Make startup faster by reducing the frequency of gc
(setq gc-cons-threshold (* 50 1000 1000))

(defconst elisp-dir (locate-user-emacs-file "elisp"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
  (setq isearch-allow-scroll 'unlimited)
  (setq query-replace-highlight t))

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
  (when (fboundp 'context-menu-mode)
    (context-menu-mode 1))
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
  (setq ispell-dictionary "en_US"))

(use-package wcheck-mode
  :ensure t
  :config
  (global-set-key (kbd "C-c s")
                  (let ((map (make-sparse-keymap)))
                    (define-key map "w" 'wcheck-mode)
                    (define-key map "l" 'wcheck-change-language)
                    (define-key map "a" 'wcheck-actions)
                    (define-key map "f" 'wcheck-jump-forward)
                    (define-key map "b" 'wcheck-jump-backward)
                    map))
  (setq wcheck-language-data
        `(("British English"
           (program . ,(or (executable-find "ispell") "ispell"))
           (args "-l" "-d" "british")
           (action-program . ,(or (executable-find "ispell") "ispell"))
           (action-args "-a" "-d" "british")
           (action-parser . wcheck-parser-ispell-suggestions))
          ("Finnish"
           (program . ,(or (executable-find "enchant-2")
                           (executable-find "enchant")))
           (args "-l" "-d" "fi")
           (syntax . my-finnish-syntax-table)
           (action-program . "/usr/bin/enchant")
           (action-args "-a" "-d" "fi")
           (action-parser . wcheck-parser-ispell-suggestions))
          )))

(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package repeat
  :if (version<= "28" emacs-version)
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  (repeat-mode 1))

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

(use-package electric
  :config
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs '((34 . 34)
                              (8216 . 8217)
                              (8220 . 8221)
                              (123 . 125)))
  (setq electric-pair-skip-self t)
  (setq electric-pair-skip-whitespace 'chomp)
  :hook ((after-init-hook . electric-indent-mode)))

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
  :mode ("\\.[tT]e[xX]\\'" . latex-mode)
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
  (setq TeX-auto-untabify t)
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
  :mode "\\.epub\\'"
  :config
  (defun my-nov-setup-hook ()
    (face-remap-add-relative 'variable-pitch :family "ETBembo Roman"
                             :height 1.0)
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

(use-package x509-mode
  :ensure t)

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
  ;; Any file start with xml will be treat as nxml-mode
  :magic ("<\\?xml" . nxml-mode)
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

  ;; Use nxml-mode instead of sgml, xml or html mode.
  (mapc
   (lambda (pair)
     (if (or (eq (cdr pair) 'xml-mode)
             (eq (cdr pair) 'sgml-mode))
         (setcdr pair 'nxml-mode)))
   auto-mode-alist)
  (define-key nxml-mode-map (kbd "C-c C-f") 'bf-pretty-print-xml-region))

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
  :hook ((after-init-hook . auto-compression-mode)
         (focus-out-hook . garbage-collect))
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
    (set-face-attribute 'default nil :family "Input Mono Narrow" :height 100)
    (set-face-attribute 'variable-pitch nil :family "Input Serif")
    (set-face-attribute 'fixed-pitch nil :family "Input Mono Narrow")
    (set-face-attribute 'tooltip nil :family "Input Mono Narrow"))

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Don't prompt if killing buffer with process attached
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

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

;; (use-package dracula-theme
;;   :ensure t
;;   :config (load-theme 'dracula t nil))

 (use-package emacs
   :init
   (load-theme 'modus-vivendi t nil))

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
  (setq diary-file "/ssh:tmy@mars.bittivirhe.fi:diary"))

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
  (setq save-place-file (locate-user-emacs-file "places"))
  (save-place-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude "\\elpa")
  :hook (after-init-hook . recentf-mode))

(use-package bookmark
  :config
  (setq bookmark-default-file (locate-user-emacs-file "bookmarks"))
  (setq bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (setq savehist-autosave-interval 60)
  :hook (after-init-hook . savehist-mode))

(use-package abbrev
  :hook (kill-emacs-hook . write-abbrev-file)
  :config
  (setq abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
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
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (when (eq system-type 'berkeley-unix)
    (setq exec-path-from-shell-arguments '("-l")))
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH"
          "JAVA_HOME" "GOPATH"
          "GERBIL_HOME" "CVSROOT")))

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
  (setq comint-prompt-read-only t)
  :hook (comint-mode-hook . (lambda ()
                              (define-key comint-mode-map [remap kill-region]
                                          'comint-kill-region)
                              (define-key comint-mode-map [remap kill-whole-line]
                                          'comint-kill-whole-line))))

(use-package eshell
  :config
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  (setq eshell-save-history-on-exit t)
  (setq eshell-scroll-show-maximum-output t)
  (setq eshell-scroll-to-bottom-on-output t)
  (setq eshell-cmpl-autolist t)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-cmpl-cycle-cutoff-length 2)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-cp-overwrite-files nil)
  (setq eshell-default-target-is-dot t)
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-list-files-after-cd t)
  (setq eshell-review-quick-commands t)
  (setq eshell-save-history-on-exit t)
  (setq eshell-scroll-show-maximum-output nil)
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (setq eshell-visual-options '(("git" "--help" "--paginate"))))

;;; ------------------------------
;;; Org-mode
;;; ------------------------------

(use-package org
  :demand t
  :init
  (setq org-list-allow-alphabetical t)
  :config
  (setq org-directory "/ssh:tmy@mars.bittivirhe.fi:Org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))
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
  ;; allow shell execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))
  :hook ((org-mode-hook . variable-pitch-mode)
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
  (let ((todo-template (concat "* TODO %^{Title}\n"
                               ":PROPERTIES:\n"
                               ":CAPTURED: %U\n"
                               ":END:\n\n"
                               "%i%l")))
    ;; "* TODO %?\n  %i\n  %a"
    (setq org-capture-templates
          `(("t" "Todo" entry (file+headline "todo.org" "Tasks")
             ,todo-template)
            ("s" "School work" entry (file+headline "school.org" "School work")
             ,todo-template)
            ("m" "Master's thesis" entry (file+headline "school.org" "Thesis")
             ,todo-template)
            ("w" "Work tasks" entry (file+headline "work.org" "Work tasks")
             ,todo-template)
            ("n" "Notes" entry (file+datetree "notes.org")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("j" "Journal" entry (file+datetree "journal.org")
             "* %?\nEntered on %U\n  %i\n  %a"))))

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
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  (add-to-list 'org-latex-classes
               '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               t)
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
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
  (setq ibuffer-shrink-to-minimum-size t)
  :hook (ibuffer-mode-hook . ibuffer-auto-mode))

;;; -----------------------------
;;; IRC
;;; ------------------------------

(use-package rcirc
  :config
  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :channels ("#openbsd" "#lisp"))))
  (setq rcirc-default-nick "zmyrgel")
  (setq rcirc-default-user-name "zmyrgel")
  (setq rcirc-default-full-name "Curious Minds Want To Know")

  (let ((nickserv-pass (secrets-get-secret "default" "libera-pass")))
    (when nickserv-pass
      (setq rcirc-authinfo
            `(("libera" nickserv "zmyrgel" ,nickserv-pass)))))

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
  (setq erc-log-channels-directory (locate-user-emacs-file "erc-logs"))
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
                  (nnir-search-engine imap)
                  (nnimap-stream ssl))
          (nnimap "imap-metro"
                  (nnimap-address "imap.metropolia.fi")
                  (nnimap-server-port "993")
                  (nnimap-stream tls))
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnir-search-engine imap)
                  (nnimap-stream tls))))
  :bind ("C-z m" . gnus))

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
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks"))
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

(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  ;;TODO: change to only apply json formatting when the content-type is
  ;;application/json
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

;; (use-package company-restclient
;;   :ensure t
;;   :after (restclient)
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))

;;; ------------------------------
;;; Completion
;;; ------------------------------

(use-package orderless
  :ensure t)

(use-package marginalia
  :ensure t
  :config
  (setq marginalia-max-relative-age 0)
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package mct
  :ensure t
  :config
  (setq mct-hide-completion-mode-line t)
  (setq mct-remove-shadowed-file-name t)
  (setq mct-show-completion-line-numbers nil)
  (setq mct-completions-format 'one-column)
  (mct-minibuffer-mode 1)
  (mct-region-mode 1))
;; mct-backward-updir.

(use-package minibuffer
  :after (orderless)
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))))
  (setq completion-cycle-threshold 2)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters nil)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completions-detailed t)
  (when (version<= "28" emacs-version)
    (setq completions-group t)
    (setq completions-group-sort 'alphabetical)
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt)))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq minibuffer-beginning-of-buffer-movement t)
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq minibuffer-eldef-shorten-default t)
  (setq echo-keystrokes 0.5)
  (setq read-answer-short t)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

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
  (setq dabbrev-upcase-means-case-search t))

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
  (setq dired-omit-verbose nil)
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

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode))

(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

(use-package vc
  :config
  (setq vc-suppress-confirm t)
  (setq vc-command-messages t)
  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale)
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil)
  (setq vc-git-root-log-format
        '("%d %h %ad %an: %s"
          "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (defun vc-git-checkout-remote ()
    "Checkout Git remote and set local branch to track it."
    )

  (defun vc-git-kill-commit-hash (log)
    "Kill commit hash at point to kill-ring for later use."
    (interactive)
    (let ((hash 1))
      ))

  ;; see:  https://ane.iki.fi/emacs/patches.html
  (defun vc-git-send-patch ()
    "Prepare git commits to be sent via email using Git CLI tools.

Perhaps useful to set global option: `git config --global sendemail.annotate yes'."
    (interactive)
    (with-editor-async-shell-command "git send-email -1"))

  (defun vc-git-apply-patch (patch project)
    "path to patch and project (default current if in one)"
    (interactive)
    (shell-command-on-region (point-min) (point-max) "git am")))

(use-package vc-got
  :if (file-directory-p "~/git/vc-got")
  :load-path "~/git/vc-got"
  :defer t
  :init
  (add-to-list 'vc-handled-backends 'Got)
  (add-to-list 'vc-directory-exclusion-list ".got"))

(use-package bug-reference
  :hook ((prog-mode . bug-reference-prog-mode)
         (text-mode . bug-reference-mode)))

(use-package compile
  :config
  (setq compilation-save-buffers-predicate nil)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (setq compilation-window-height 12))

(use-package ansi-color
  :after (compile)
  :hook (compilation-filter-hook . colorize-compilation-buffer)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) ;; TODO: or compilation-filter-start?
                                  (point-max)))))

;; or use smerge-ediff to resolve conflicts
(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package diff-mode
  :config
  (setq diff-advance-after-apply-hunk t)
  (setq diff-default-read-only t)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-refine 'font-lock)
  (setq diff-update-on-the-fly t))

(use-package diff
  :config (setq diff-switches '("-u")))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
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

(use-package magit-gitflow
  :if (string= (system-name) "ws-946")
  :ensure t
  :hook (magit-mode-hook . turn-on-magit-gitflow))

(use-package eglot
  :ensure t
  :bind ((:map eglot-mode-map
               ("C-c h" . eglot-help-at-point))))

(use-package flymake
  :ensure t
  :bind ((:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

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
        (let ((pkg-path "/usr/local/share/doc/clisp-hyperspec/Data/Map_Sym.txt")
              (home-path "~/lisp/docs/HyperSpec/Data/Map_Sym.txt"))
          (cond ((file-exists-p pkg-path) pkg-path)
                ((file-exists-p home-path) home-path)
                (t "http://www.lispworks.com/documentation/HyperSpec/Data/Map_Sym.txt")))))

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

(use-package gerbil-mode
  :when (getenv "GERBIL_HOME")
  :ensure nil
  :defer t
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :bind (:map comint-mode-map
              (("C-S-n" . comint-next-input)
               ("C-S-p" . comint-previous-input)
               ("C-S-l" . clear-comint-buffer))
              :map gerbil-mode-map
              (("C-S-l" . clear-comint-buffer)))
  :init
  (setf gambit (getenv "GAMBIT_HOME"))
  (setf gerbil (getenv "GERBIL_HOME"))
  (autoload 'gerbil-mode
    (concat gerbil "/etc/gerbil-mode.el") "Gerbil editing mode." t)
  :hook
  ((inferior-scheme-mode-hook . gambit-inferior-mode))
  :config
  (require 'gambit (concat gambit "/misc/gambit.el"))
  (setf scheme-program-name (concat gerbil "/bin/gxi"))
  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (visit-tags-table (concat gerbil "/src/TAGS"))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))))

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

(use-package php-mode
  :ensure t
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
              ("C-h M" . man-follow)
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
    (c-set-style "bsd")
    (setq indent-tabs-mode t))
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

(use-package elpy
  :disabled
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  ;; Use IPython for REPL
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(use-package py-autopep8
  :disabled
  :ensure t
  :hook (elpy-mode-hook . py-autopep8-enable-on-save))

;; integrate with jupyter

(use-package ein
  :disabled
  :ensure t
  :config
  (setq ein:jupyter-server-command "jupyter-notebook")
  (setq ein:jupyter-server-use-subcommand nil))

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
  :hook ((web-mode-hook . my/web-mode-hook)
         (web-mode-hook . flymake-eslint-enable)))

(use-package typescript-mode
  :ensure t
  :after flymake-eslint
  :hook ((typescript-mode-hook . eglot-ensure)
         (typescript-mode-hook . flymake-eslint-enable))
  :config
  (setq whitespace-line-column 120))

(use-package keepass-mode
  :ensure t)

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

;; Restore old gc threshold value
(setq gc-cons-threshold 800000)

;; Load optional local startup files
(load (locate-user-emacs-file "init-local.el") t t)

;; Only start server mode for non-admin accounts
(unless (string-equal "root" (getenv "USER"))
  (when (and (fboundp 'server-running-p)
             (server-running-p))
    (server-start)))

(provide 'init)

;;; init.el ends here
