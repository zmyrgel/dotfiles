;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2020-08-14 17:33:05 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 26.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - fix warnings on this file
;;; - elisp-hook
;;; - check desktop.el
;;; - use server-after-make-frame-hook for desktop

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defconst elisp-dir (expand-file-name "elisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; avoid re-initializing packages
(unless package--initialized (package-initialize))

;; load of use-package to handle rest of package initialization.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; prefer to use stable versions over git versions
(setq use-package-always-pin "melpa-stable")
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
  ;; TODO: workaround for emacs bug, fixed in 26.3+
  (when (version<= emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; https://bugs.debian.org/766397
(when (version<= emacs-version "27")
  (setq tls-program '("gnutls-cli --x509cafile %t -p %p %h")))

(delete-selection-mode 1)

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

;; Misc options
(auto-compression-mode 1)

;; mouse options
(setq mouse-yank-at-point t)
(mouse-wheel-mode t)

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

;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))

;;; ------------------------------
;;; Text editing
;;; ------------------------------

(use-package grep
  :config
  (when (version<= "27" emacs-version)
    (setq grep-find-use-xargs 'exec-plus)))

(use-package hungry-delete
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

;; (use-package adjust-parens
;;   :disabled
;;   :ensure t)

(use-package electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs '((8216 . 8217) (8220 . 8221) (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  :hook (after-init-hook . (lambda ()
                             (electric-indent-mode 1)
                             (electric-pair-mode -1)
                             (electric-quote-mode -1))))

;; (defun th/pdf-view-revert-buffer-maybe (file)
;;   (let ((buf (find-buffer-visiting file)))
;;     (when buf
;;   (with-current-buffer buf
;;     (when (derived-mode-p 'pdf-view-mode)
;;       (pdf-view-revert-buffer nil t))))))
;; (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
;;     #'th/pdf-view-revert-buffer-maybe)


(use-package tex
  :defer t
  :ensure auctex
  :hook (latex-mode-hook . auto-fill-mode)
  :init

 ;;  (((output-dvi has-no-display-manager)
 ;;  "dvi2tty")
 ;; ((output-dvi style-pstricks)
 ;;  "dvips and gv")
 ;; (output-dvi "xdvi")
 ;; (output-pdf "Evince")
  ;; (output-html "xdg-open"))

  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-insert-braces nil)
  (setq TeX-electric-escape t)
  (setq TeX-electric-macro t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent))

(use-package pdf-tools
  :ensure t
  ;; :bind (:map pdf-view-mode-map
  ;;             ("C-s" . isearch-forward)
  ;;             ("h" . pdf-annot-add-highlight-markup-annotation)
  ;;             ("t" . pdf-annot-add-text-annotation)
  ;;             ("D" . pdf-annot-delete)
  ;;             )
  :config
  (setq pdf-view-display-size 'fit-page)
  ;;(setq pdf-annot-activate-created-annotations t)
  ;;(setq pdf-view-resize-factor 1.1)
  (pdf-tools-install))

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
  :config
  (add-to-list 'magic-mode-alist '("---" . yaml-mode)))

(use-package nxml-mode
  :mode (("\\.plist\\'" . nxml-mode)
         ("\\.rss\\'"   . nxml-mode)
         ("\\.svg\\'"   . nxml-mode)
         ("\\.xml\\'"   . nxml-mode)
         ("\\.xsd\\'"   . nxml-mode)
         ("\\.xslt\\'"  . nxml-mode)
         ("\\.pom$"     . nxml-mode))
  :config
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
  :hook (typescript-mode-hook . eglot-ensure))

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
  :config (show-paren-mode t))

(use-package frame
  :config (blink-cursor-mode -1))

;; default emacs configurations
(use-package emacs
  :bind (("C-x C-k" . kill-region)
         ("C-w" . backward-kill-word) ;; XXX: change this to backward-kill-word-or-region
         ("C-c C-j" . join-line)
         ("M-z" . zap-up-to-char)
         ("M-o" . other-window)
         ("<f1>" . eshell)
         ("<f2>" . rgrep)
         ("<f6>" . magit-status)
         ("<f11>" . gnus)
         ("<f12>" . bookmark-bmenu-list))
  :config
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (menu-bar-mode t)

  (setq case-fold-search t)

  (setq-default show-trailing-whitespace t)
  (setq-default require-final-newline t)
  (setq-default cursor-type 'box)
  (setq-default truncate-lines t)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 72)
  (setq-default tab-always-indent 'complete)

  (setq completion-ignore-case t)

  (setq sentence-end-double-space nil)

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

  ;; Maximize first frame
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-hook 'help-mode-hook (lambda () (setq truncate-lines t)))

  ;; Set Default font if present
  (let ((my-font-name "tamsyn-16"))
    (when (find-font (font-spec :name my-font-name))
      (add-to-list 'default-frame-alist `(font . ,my-font-name))
      (set-face-attribute 'default nil :font my-font-name)))

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
  (put 'overwrite-mode 'disabled t)

  ;; show buffer name in title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
		     (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
		   "Emacs: %b")))))

(use-package simple
  :config
  (setq set-mark-command-repeat-pop t)
  (setq next-line-add-newlines nil)
  (setq kill-ring-max 100)
  (setq yank-pop-change-selection t)
  (setq save-interprogram-paste-before-kill t)

  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t)
  :hook ((text-mode-hook . auto-fill-mode)
         (before-save-hook . delete-trailing-whitespace)))

(use-package diminish
  :ensure t)

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config (load-theme 'gruvbox-dark-soft t nil))

(use-package modus-vivendi-theme
  :ensure t
  :config (load-theme 'modus-vivendi t nil))

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
  (recentf-mode t))

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (setq savehist-autosave-interval 60)
  (savehist-mode t))

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
  (setq large-file-warning-threshold 50000000) ;; 50mb
  (setq backup-directory-alist `((".*" . ,temporary-file-directory)))
  ;;(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
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
          "GERBIL_HOME" "CVS_ROOT"))
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

;; (use-package ssh-tunnels
;;   :ensure t
;;   :config
;;   (setq ssh-tunnels-configurations '((:name "foo" :host "bar.fi " :remote-port 54321))))

;;; ------------------------------
;;; Org-mode
;;; ------------------------------

(use-package org
  :config
  (setq org-directory (expand-file-name "org"  user-emacs-directory))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-include-diary t) ;; TODO: this exists?
  (setq org-agenda-todo-ignore-with-date t) ;; TODO: this exists?)
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'note)
  (setq org-startup-indented t)
  (setq org-special-ctrl-a/e t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                            (sequence "|" "CANCELED(c@/!)")
                            (sequence "STALLED(s@/!)" "|")
                            (sequence "PENDING(p@/!)" "|")))
  ;; capture notes
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-mode-hook . flyspell-mode)
         (message-mode-hook . turn-on-orgtbl)))

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
  ;;:disabled
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
  ;;(setq rcirc-time-format "%Y-%m-%d %H:%M ")
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)
         ;;(rcirc-mode-hook . flyspell-mode)
         ))

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
(setq message-send-mail-function 'smtpmail-send-it)

(use-package smtpmail
  :config
  (setq smtpmail-default-smtp-server "smtp.fastmail.com")
  (setq smtpmail-smtp-server         "smtp.fastmail.com")
  (setq smtpmail-local-domain        "bittivirhe.fi")
  (setq smtpmail-smtp-service        465)
  (setq smtpmail-stream-type         'ssl))

;; gnus
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.gmane.io"))
  (setq gnus-treat-hide-citation t)
  (setq gnus-cited-lines-visible '(0 . 5))
  (setq gnus-always-read-dribble-file t)
  (setq mm-inline-large-images 'resize)
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq mm-text-html-renderer 'shr)

  (setq gnus-secondary-select-methods
        '((nnimap "work-gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "993")
                  (nnimap-stream ssl))
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnimap-stream tls)))))

;;; ------------------------------
;;; Web Browsing settings
;;; ------------------------------

(use-package newsticker
  :config
  (setq newsticker-url-list
        '(;; ("title" "URL" other options)
          ("Free Software Foundation Europe" "https://fsfe.org/news/news.it.rss")
          ("Free Software Foundation USA" "https://static.fsf.org/fsforg/rss/blogs.xml")
          ("(or Emacs" "http://oremacs.com/atom.xml")
          ("Emacs Blog" "http://emacsblog.org/feed/")
          ("Howardism - Howard Abrams blog" "http://howardism.org/index.xml")
          ("Endless parentheses" "http://endlessparentheses.com/atom.xml")
          ("Mastering Emacs" "https://www.masteringemacs.org/feed")
          ("Scripter" "https://scripter.co/posts/atom.xml")
          ("One Of Us" "https://oneofus.la/have-emacs-will-hack/feed.xml")
          ("Org-mode upcoming changes" "https://updates.orgmode.org/feed/changes")
          ("Reddit - Emacs" "https://www.reddit.com/r/emacs.rss")
          ("Reddit - Org-mode" "https://www.reddit.com/r/orgmode.rss")
          ("Reddit - Stallman Was Right" "https://www.reddit.com/r/StallmanWasRight.rss")
          ("XKCd" "https://xkcd.com/atom.xml"))))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml"
          "https://news.ycombinator.com/rss"
          "http://www.tedunangst.com/flak/rss"
          "https://undeadly.org/cgi?action=rss"
          "https://www.phoronix.com/rss.php"
          "http://planetsysadmin.com/atom.xml"
          "https://planet.lisp.org/rss20.xml"
          "https://lobste.rs/t/emacs.lisp.security.ask.ai.openbsd.programming.rss")))

(use-package eww
  :config
  (setq eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\|pdf\\)")
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-new-window-flag nil)
  (setq browse-url-firefox-new-window-is-tab t))

;;; ------------------------------
;;; Completion
;;; ------------------------------

;; XXX: setup provs window rules or check them out
(use-package icomplete
  :config
  (setq icomplete-prospects-height 1)
  (setq icomplete-in-buffer t)
  (setq icomplete-delay-completions-threshold 0.3)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-separator " | ")
  (setq icomplete-with-completion-tables t)
  (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))

;: XXX: eval
;; (use-package orderless
;;   :ensure t
;;   :init (icomplete-mode)
;;   :config (setq completion-styles '(orderless)))

(use-package minibuffer
  :config
  (setq completions-format 'vertical)
  (setq read-file-name-completion-ignore-case t)
  ;;completion-styles (append completion-styles '(initials))
  (setq completion-styles '(partial-completion substring))
  (setq completion-category-overrides '((file (styles basic substring))))
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)
  (setq completion-show-help nil)
  (setq completion-cycle-threshold 3)
  (unless (version< emacs-version "27")
    (setq completion-flex-nospace nil))
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-styles '(partial-completion substring initials flex))
  (setq completion-category-overrides
        '((file (styles initials basic))
          (buffer (styles initials basic))
          (info-menu (styles basic))))
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init-hook . global-company-mode)
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
  :hook (dired-mode-hook . hl-line-mode)
  :config
  (require 'dired-x)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-isearch-filenames t)
  (setq dired-ls-F-marks-symlinks t)
  ;; Don't pass --dired flag to ls on BSD
  (when (eq system-type 'berkeley-unix)
    (setq dired-use-ls-dired nil))

  (setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")
  (setq dired-guess-shell-alist-user
        '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "mplayer"
           "\\.rar$" "unrar e")))
  (setq dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                   "tar")))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

(use-package bongo
  :ensure t
  :defer t
  :hook (bongo-player-started-hook . bongo-no-autoplay-video)
  :config
  (defun bongo-no-autoplay-video ()
    "don't autoplay next track if playing video"
    (with-bongo-playlist-buffer
     (when (bongo-video-file-name-p
            (bongo-player-get bongo-player 'file-name))
       (setq bongo-next-action 'bongo-stop))))

  (setq bongo-custom-backend-matchers
        `((mplayer
           (local-file "file:" "http:" "ftp:")
           "ogg" "flac" "mp3" "m4a" "mka" "wav" "wma"
           "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "m4v"
           "mkv" "flv" "mov" "asf" "wmv" "rm" "rmvb" "ts"))))

;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(use-package vc
  :config
  (setq vc-suppress-confirm t)
  (setq vc-command-messages t)
  ;; (setq vc-cvs-global-switches '("-P" "d"))
  ;; (setq vc-cvs-stay-local t)
  ;; (setq vc-find-revision-no-save t)
  ;; :bind (("C-x v b" . vc-retrieve-tag)  ; "branch" switch
  ;;        ("C-x v t" . vc-create-tag)
  ;;        ("C-x v I" . vc-log-incoming)  ; the actual git fetch
  ;;        ("C-x v F" . vc-update)        ; "F" because "P" is push
  ;;        ("C-x v d" . vc-diff)))
  )

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

(use-package prog-mode
  :diminish subword-mode
  :config
  (defun my/prog-mode-hook ()
    "Hook to run when entering generic prog-mode."
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail)
          which-func-unknown "TOP LEVEL")
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                   1 font-lock-warning-face prepend))))
  :hook ((prog-mode-hook . electric-pair-mode)
         (prog-mode-hook . subword-mode)
         (prog-mode-hook . which-function-mode)
         ;;(prog-mode-hook . flyspell-prog-mode)
         (prog-mode-hook . my/prog-mode-hook)))

(use-package magit
  :ensure t
  :bind ("C-x v /" . magit-status))

(use-package eglot
  :ensure t)

(use-package flymake
  ;;:config
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  ;; (setq flymake-proc-allowed-file-name-masks
  ;;       (cons '(".+\\.ts$"
  ;;               flymake-proc-simple-make-init
  ;;               flymake-proc-simple-cleanup
  ;;               flymake-proc-get-real-file-name)
  ;;             flymake-proc-allowed-file-name-masks))
  )

;; XXX: project specific variables?
(use-package flymake-eslint
  :ensure t)

;;; Go programming

(use-package go-mode
  :ensure t
  :after eglot
  :hook ((before-save-hook . gofmt-before-save)
         (go-mode-hook . eglot-ensure))
  :bind (:map go-mode-map
              ("C-c m" . gofmt)
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
  :pin melpa
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
  :pin melpa
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
(setq nrepl-hide-special-buffers t)
  :hook (cider-mode-hook . eldoc-mode))

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
  :after (flycheck eglot)
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
     (when (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
       (eglot-ensure)))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  :hook (web-mode-hook . my/web-mode-hook))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq flycheck-phpcs-standard "Zend")
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;;; ------------------------------
;;; Functions
;;; ------------------------------

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

;; Create *scratch* automatically
(run-with-idle-timer 1 t
                     '(lambda ()
                        (unless (get-buffer "*scratch*")
                          (with-current-buffer (get-buffer-create "*scratch*")
                            (lisp-interaction-mode)))))

;; Only start server mode for non-admin accounts
(unless (and (string-equal "root" (getenv "USER"))
             (server-running-p))
  (server-start))

(provide 'init)

;;; init.el ends here
