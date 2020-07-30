;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2020-07-30 18:05:20 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 26.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - elisp-hook
;;; - check key bindings, define-key etc.
;;; - fix warnings on this file
;;; - elisp-hook
;;; - check desktop.el
;;; - use server-after-make-frame-hook for desktop
;;; - display-fill-column-indicator
;;; - winner
;;; - check log-edit-generate-changelog-from-diff: C-c C-w
;;; - fido-mode for icomplete
;;; - isearch-lazy-count
;;; - xterm-set-window-title: experimental
;;; - check global-so-long-mode: long-lines cause pauses
;;; - vc: add support for got?

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defconst elisp-dir (expand-file-name "elisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; avoid re-initializing packages
(when (version< emacs-version "27")
  (package-initialize))

;; load of use-package to handle rest of package initialization.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; ------------------------------
;;; General
;;; ------------------------------

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

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

;; keep a little more history to see whats going on
(setq message-log-max 16384)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t
      next-line-add-newlines nil)

(delete-selection-mode 1)

;; confirm before exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

(use-package isearch
  :config (setq search-highlight t))

(setq query-replace-highlight t)

;; Misc options
(auto-compression-mode 1)

;; mouse options
(setq mouse-yank-at-point t)
(mouse-wheel-mode t)

(defun my/text-mode-hook ()
  "Default settings for Text."
  (set-fill-column 80))

(use-package flyspell
  :hook (((change-log-mode-hook log-edit-mode-hook) . 'flyspell-mode-off)
         (text-mode-hook . 'flyspell-mode))
  :config
  (setq flyspell-issue-message-flag nil))

;; Hooks
(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; ------------------------------
;;; Text editing
;;; ------------------------------

;; XXX: eval
;;(use-package crux :ensure t)

(use-package grep
  :config
  (when (version<= "27" emacs-version)
    (setq grep-find-use-xargs 'exec-plus)))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package smartparens
  :diminish
  :ensure t
  :init (require 'smartparens-config)
  :config
  (smartparens-global-mode 1)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (add-to-list 'sp-lisp-modes 'sly-mode)
  (sp-local-pair #'sly-mrepl-mode "'" nil :actions nil))

(use-package adjust-parens
  :disabled
  :ensure t)

(use-package tex
  :defer t
  :ensure auctex
  :hook (latex-mode-hook . auto-fill-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-insert-braces nil
        TeX-electric-escape t
        TeX-electric-macro t
        TeX-newline-function 'reindent-then-newline-and-indent))

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

(use-package ansible-vault
  ;;; TODO: add vault-identity support
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode))

;;; ------------------------------
;;; Visual settings
;;; ------------------------------

(use-package diminish
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config (sml/setup))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :config (sml/apply-theme 'powerline))

(use-package zenburn-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-soft t nil))

(use-package base16-theme
  :ensure t)

(use-package font-core
  :config (global-font-lock-mode t))

(use-package font-lock
  :config (setq font-lock-maximum-decoration t))

(use-package paren
  :config (show-paren-mode t))

(use-package frame
  :config (blink-cursor-mode -1)
)

;; default emacs configurations
(use-package emacs
  :config

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (menu-bar-mode t)

  (setq case-fold-search t)

  (setq-default cursor-type 'box)

  (setq initial-scratch-message ""
        inhibit-startup-screen t
        inhibit-startup-echo-area-message t)

  (setq visible-bell t)
  (setq window-min-height 3)

  ;; disable dialog boxes
  (setq use-file-dialog nil
        use-dialog-box nil)

  ;; Maximize first frame
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

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

  ;; show buffer name in title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
		     (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
		   "Emacs: %b")))))

(use-package simple
  :config
  (setq set-mark-command-repeat-pop t)
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))

;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(use-package suomalainen-kalenteri
  :ensure t)

(use-package calendar
  :hook ((diary-list-entries-hook . diary-include-other-diary-files)
         (diary-list-entries-hook . diary-sort-entries)
         (diary-mark-entries-hook . diary-mark-included-diary-files)
         (calendar-today-visible-hook . calendar-mark-today))
  :config
  ;; localize calendar for finland
  (setq calendar-week-start-day 1
        calendar-day-name-array
        ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
         "torstai" "perjantai" "lauantai"]
        calendar-month-name-array
        ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
         "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
         "lokakuu" "marraskuu" "joulukuu"])

  ;; make calendar use european date format
  (add-hook 'calendar-load-hook (lambda () (calendar-set-date-style 'european)))

  ;; rest of calendar configs
  (setq calendar-mark-holidays-flag t
        calendar-view-diary-initially-flag t
        calendar-date-style 'european
        calendar-mark-diary-entries-flag t)

  (setq diary-show-holidays-flag t
        diary-file (expand-file-name "diary" user-emacs-directory)
        diary-display-function 'diary-fancy-display ;; XXX: is this needed
        diary-number-of-entries 7)) ;; XXX: is this needed)

;; time utilities
(use-package time-stamp
  :config
  (setq time-stamp-active t
        time-stamp-line-limit 10
        time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)"))

(use-package time
  :config
  (display-time-mode -1)
  (setq display-time-24hr-format t
        display-time-day-and-date nil
        display-time-format nil
        display-time-use-mail-icon t))

;;; ------------------------------
;;; Session
;;; ------------------------------

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "places" user-emacs-directory))
  (save-place-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 50)
  (recentf-mode t))

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
        bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)
        savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60)
  (savehist-mode t))

(use-package abbrev
  :hook (kill-emacs-hook . write-abbrev-file)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)
        save-abbrevs t)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

(use-package files
  :config
  (setq large-file-warning-threshold 50000000
        backup-directory-alist `((".*" . ,temporary-file-directory))
        ;;auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        make-backup-files t
        backup-by-copying t))

;;; ------------------------------
;;; Shell settings
;;; ------------------------------

(use-package exec-path-from-shell
  :config
  (when (eq system-type 'berkeley-unix)
    (setq exec-path-from-shell-arguments '("-l")))
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH"
          "JAVA_HOME" "GOPATH"
          "GERBIL_HOME"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package multi-term
  :ensure t
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term)))

(defun my/sh-mode-hook ()
  (set (make-local-variable 'indent-tabs-mode) t))

(add-hook 'sh-mode-hook 'my/sh-mode-hook)

(setq shell-command-switch "-c"
      explicit-sh-args '("-login" "-i"))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(use-package comint
  :config
  (setq comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output t
        comint-completion-autolist t
        comint-input-ignoredups t
        comint-completion-addsuffix t
        comint-prompt-read-only t))

(use-package eshell
  :config
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
        eshell-save-history-on-exit t
        eshell-scroll-show-maximum-output t
        eshell-scroll-to-bottom-on-output t))

;;; customization for term, ansi-term
;; disable cua and transient mark modes in term-char-mode
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'transient-mark-mode) t))
(ad-activate 'term-line-mode)

(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)

;;; ------------------------------
;;; Org-mode
;;; ------------------------------

(use-package org
  :config
  (setq org-directory (expand-file-name "org"  user-emacs-directory)
        org-outline-path-complete-in-steps nil
        org-agenda-files (list org-directory)
        org-agenda-include-diary t ;; TODO: this exists?
        org-agenda-todo-ignore-with-date t ;; TODO: this exists?
        org-insert-mode-line-in-empty-file t
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-log-done 'note
        org-startup-indented t
        org-special-ctrl-a/e t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                            (sequence "|" "CANCELED(c@/!)")
                            (sequence "STALLED(s@/!)" "|")
                            (sequence "PENDING(p@/!)" "|"))
        ;; capture notes
        org-default-notes-file (expand-file-name "notes.org" org-directory))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :hook ((org-mode-hook . 'flyspell-mode)
         (message-mode . turn-on-orgtbl)))

;;; ------------------------------
;;; Buffer management
;;; ------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer)
  (setq ibuffer-default-sorting-mode 'major-mode
        ibuffer-expert t)
  :hook ((ibuffer-mode-hook . ibuffer-auto-mode))
  :bind (:map ibuffer-mode-map
              ("C-x C-f" . counsel-find-file)))

(use-package ibuffer-vc
  :disabled
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
  (setq rcirc-default-nick "zmyrgel"
        rcirc-default-user-name "zmyrgel"
        rcirc-default-full-name "Curious Minds Want To Know")

  (let ((nickserv-pass (secrets-get-secret "default" "freenode-pass")))
    (when nickserv-pass
      (setq rcirc-authinfo
            `(("freenode" nickserv "zmyrgel" ,nickserv-pass)))))

  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  ;;(setq rcirc-time-format "%Y-%m-%d %H:%M ")
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)
         (rcirc-mode-hook . flyspell-mode)))

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

  (setq erc-prompt-for-password nil
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit nil
        erc-kill-server-buffer-on-quit t
        erc-auto-query 'window-noselect
        erc-keywords '("zmyrgel" "tmy"))

  (setq erc-track-enable-keybindings t
        erc-track-remove-disconnected-buffers t
        erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-timestamp-format "[%R-%m/%d]"
        erc-hide-timestamps nil)

  (pcomplete-erc-setup)

  (setq erc-pcomplete-order-nickname-completions t
        erc-log-channels-directory (expand-file-name ".irclogs" "~")
        erc-log-insert-log-on-open nil
        erc-log-file-coding-system 'utf-8-unix
        erc-save-buffer-on-part t
        erc-max-buffer-size 20000
        erc-truncate-buffer-on-save t)
  (defvar erc-insert-post-hook nil))

;;; ------------------------------
;;; Email settings
;;; ------------------------------

(setq user-mail-address "timo.myyra@bittivirhe.fi"
      user-full-name "Timo Myyrä")
(setq message-send-mail-function 'smtpmail-send-it)

(use-package smtpmail
  :config
  (setq smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-server         "smtp.fastmail.com"
        smtpmail-local-domain        "bittivirhe.fi"
        smtpmail-smtp-service        465
        smtpmail-stream-type         'ssl))

;; gnus
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.gmane.io")
        gnus-treat-hide-citation t
        gnus-cited-lines-visible '(0 . 5)
        gnus-always-read-dribble-file t
        mm-inline-large-images 'resize
        mm-discouraged-alternatives '("text/html" "text/richtext")
        mm-text-html-renderer 'shr)

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

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-use-curl t
        elfeed-feeds
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
  (setq eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\|pdf\\)"
        browse-url-browser-function 'eww-browse-url
        browse-url-new-window-flag nil
        browse-url-firefox-new-window-is-tab t))

;;; ------------------------------
;;; Completion
;;; ------------------------------

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-mode-map
         ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-height 10
        ;;ivy-count-format ""          ;; does not count candidates
        ;;ivy-initial-inputs-alist nil ;; no regexp by default
        ;;ivy-re-builders-alist '((t . ivy--regex-ignore-order)) ;; configure regexp engine. ;; allow input not in order))
        ))

(use-package counsel
  :ensure t
  :diminish t
  :config (counsel-mode))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))

(setq dabbrev-case-replace nil)

(use-package icomplete
  :config
  (setq icomplete-prospects-height 2)
  (icomplete-mode t))

(setq completion-ignore-case t
      tab-always-indent 'complete)

(use-package minibuffer
  :config
  (setq read-file-name-completion-ignore-case t
        completion-styles (append completion-styles '(initials))))

;;; ------------------------------
;;; File and directory management
;;; ------------------------------

(use-package bongo
  :ensure t
  :defer t
  :config
  (setq bongo-custom-backend-matchers
        `((mplayer
           (local-file "file:" "http:" "ftp:")
           "ogg" "flac" "mp3" "m4a" "mka" "wav" "wma"
           "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "m4v"
           "mkv" "flv" "mov" "asf" "wmv" "rm" "rmvb" "ts")))

  ;; don't autoplay next track if playing video
  (add-hook 'bongo-player-started-hook
            (lambda ()
              (with-bongo-playlist-buffer
               (when (bongo-video-file-name-p
                      (bongo-player-get bongo-player 'file-name))
                 (setq bongo-next-action 'bongo-stop))))))

(use-package dired
  :init
  (defun my/dired-load-hook ()
    "My Dired load hook."
    (setq truncate-lines t))
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :hook ((dired-load-hook . 'my/dired-mode-hook)
         (dired-mode-hook . 'hl-line-mode))
  :config
  (require 'dired-x)
  (setq dired-isearch-filenames t
        dired-ls-F-marks-symlinks t)
  ;; Don't pass --dired flag to ls on BSD
  (when (eq system-type 'berkeley-unix)
    (setq dired-use-ls-dired nil))

  (setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$"
        dired-guess-shell-alist-user
        '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "mplayer"
           "\\.rar$" "unrar e"))
        dired-guess-shell-gnutar (if (eq system-type 'berkeley-unix)
                                     "gtar"
                                   "tar")))

;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(use-package magit
  :ensure t
  :bind ("C-x v /" . magit-status))

(use-package eglot
  :ensure t)

;;; Go programming

(use-package company-go
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package go-mode
  :ensure t
  :after (company company-go)
  :hook ((before-save-hook . gofmt-before-save)
         (go-mode-hook . company-mode))
  :bind (:map go-mode-map
              ("C-c m" . gofmt)
              ("M-." . godef-jump)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c g i" . go-goto-imports)
              ("C-c C-k" . godoc))
  :config
  (setq gofmt-command "goimports")
  (set (make-local-variable 'compile-command) "go build -v && go test -v && go vet"))

(use-package go-eldoc
  :ensure t
  :hook (go-mode-hook . go-eldoc-setup))

(use-package godoctor
  :ensure t
  :defer t)

(use-package go-guru
  :ensure t
  :defer t)

(use-package go-errcheck
  :ensure t
  :defer t)

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
  :hook my/ruby-mode-hook)

;;; Lisp programming

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package sly
  :ensure t
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
         "Data/Map_Sym.txt"))

  (add-hook 'lisp-mode-hook 'sly-mode))

(use-package sly-repl-ansi-color
  :ensure t
  :after sly
  :config (sly-enable-contrib 'sly-repl-ansi-color))

(use-package quack
  :disabled
  :ensure t
  :config
  (setq quack-default-program "csi"
        quack-dir (concat user-emacs-directory "quack/")
        quack-fontify-style nil
        quack-newline-behavior 'indent-newline-indent
        quack-pretty-lambda-p nil
        quack-remap-find-file-bindings-p nil
        quack-run-scheme-always-prompts-p nil
        quack-run-scheme-prompt-defaults-to-last-p t
        quack-smart-open-paren-p t
        quack-switch-to-scheme-method 'other-window))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj$"
  :hook (clojure-mode-hook . my/shared-lisp-hook))

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-lein-parameters "repl :headless :host localhost"
        nrepl-hide-special-buffers t)
  :hook (cider-mode-hook . eldoc-mode))

(use-package geiser
  :disabled
  :ensure t
  :config
  (when (eq system-type 'berkeley-unix)
    (setq geiser-chicken-binary "chicken-csi"
          geiser-guile-binary "guile2")))

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
    (setq indent-tabs-mode nil
          tab-width 4
          c-basic-offset 4))
  :hook (php-mode-hook . my/php-mode-hook))

(use-package rainbow-mode
  :ensure t)

(use-package compile
  :config
  (setq compilation-save-buffers-predicate nil
        compilation-ask-about-save nil
        compilation-window-height 12))

(use-package diff-mode
  :config
  (setq diff-font-lock-prettify nil))

(use-package diff
  :config (setq diff-switches '("-u")))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w")
  :hook (ediff-after-quit-hook-internal . winner-undo))

(use-package prog-mode
  :config
  (defun my/prog-mode-hook ()
    "Hook to run when entering generic prog-mode."
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail)
          which-func-unknown "TOP LEVEL")
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                   1 font-lock-warning-face prepend)))
  :hook ((prog-mode . 'electric-pair-mode)
         (prog-mode . 'subword-mode)
         (prog-mode . 'which-function-mode)
         (prog-mode . 'flyspell-prog-mode)
         (prog-mode . 'my/prog-mode-hook))))

(use-package cc-mode
  :bind (:map c-mode-map
              ("C-c m" . man-follow)
              ("C-c C-d" . gdb)
              ("C-m" . c-context-line-break)
              ("C-c o" . ff-find-other-file))
  :hook ((c-mode-common-hook . which-function-mode)
         (c-mode-common-hook . cwarn-mode))
  :config
  (defun my/c-mode-common ()
    "Programming options shared for C-like languages."
    (cwarn-mode 1)
    (setq compilation-scroll-output 'first-error))
  (defun my/c-mode ()
    "My C programming options."
    (c-set-style "bsd"))
  (defun my/c++-mode ()
    "My C++ programming options."
    (setq fill-column 100)
    (c-set-style "stroustrup")
    (setq whitespace-line-column 100
          whitespace-style '(face lines-tail)))

  (add-hook 'c-mode-hook 'my/c-mode)
  (add-hook 'c++-mode-hook 'my/c++-mode))

(use-package cperl-mode
  :init
  (defalias 'perl-mode 'cperl-mode)
  :config
  (defun my/cperl-mode-hook ()
    "Default CPerl settings."
    (setq cperl-fontlock t
          cperl-info-on-command-no-prompt t
          cperl-clobber-lisp-bindings t
          cperl-lazy-help-time 5
          cperl-indent-level 4
          cperl-invalid-face 'default))
  :hook (cperl-mode . my/cperl-mode-hook))

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
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 4)
     (when (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
       (eglot-ensure)))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  :hook (web-mode . my/web-mode-hook))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (after-init-hook . global-flycheck-mode)
  :init (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  :config
  (setq flycheck-phpcs-standard "Zend")
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
;;; Keybindings
;;; ------------------------------

(global-set-key (kbd "C-x C-k") 'kill-region)
;;(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; enable regexp search by default
;;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; shortcuts
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'rgrep)
(global-set-key (kbd "<f6>") 'magit-status)
(global-set-key (kbd "<f11>") 'gnus)
(global-set-key (kbd "<f12>") 'bookmark-bmenu-list)

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

(unless (boundp 'server-process)
  (server-start))

(provide 'init)

;;; init.el ends here
