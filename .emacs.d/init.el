;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2019-04-14 10:29:31 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 26.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - Autoloads for gnus
;;; - check key bindings, define-key etc.
;;; - fix warnings on this file
;;; - add minor modes in hooks

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defconst elisp-dir (concat user-emacs-directory "elisp/"))
(defconst elpa-dir (concat user-emacs-directory "elpa/"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Ensure ELPA exists
(when (not (file-directory-p elpa-dir))
  (make-directory elpa-dir t))

(require 'package)

(add-to-list 'load-path elisp-dir)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; load of use-package to handle rest of package initialization.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(use-package exec-path-from-shell
  :init
  (when (eq system-type 'berkeley-unix)
    (setq exec-path-from-shell-arguments '("-l")))
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "JAVA_HOME" "GOPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

(use-package try
  :ensure t)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package achievements
  :ensure t)

(use-package which-key
  :defer t
  :ensure t
  :config (which-key-mode))

(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-char))

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind ("C-x v /" . magit-status))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package smex
  ;;:disabled
  :ensure t
  :bind
  (("M-x" . 'smex)
   ("M-X" . 'smex-major-mode-commands)
   ("C-c C-c M-x" . 'execute-extended-command)))

(use-package flx
  :ensure t)

(use-package ivy
  :disabled
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ;;ivy-use-virtual-buffers t                              ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ;;ivy-height 10                                          ;; number of result lines to display
        ;;ivy-count-format ""                                    ;; does not count candidates
        ;;ivy-initial-inputs-alist nil                           ;; no regexp by default
        ;;ivy-re-builders-alist '((t . ivy--regex-ignore-order)) ;; configure regexp engine. ;; allow input not in order
        )
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package counsel
  :ensure t
  :diminish t
  :config
  (counsel-mode))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$\\|\\.yaml$")

(use-package slime-company
  :ensure t
  :config
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)))

(use-package slime
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (load "~/quicklisp/log4slime-setup.el")
  (global-log4slime-mode 1)
  :config
  (defun my/slime-mode-hook ()
    (setq slime-description-autofocus t
          slime-repl-history-trim-whitespaces t
          slime-repl-wrap-history t
          slime-repl-history-file (concat user-emacs-directory "slime-history.eld")
          slime-repl-history-remove-duplicates t
          slime-ed-use-dedicated-frame t
          slime-kill-without-query-p t
          slime-startup-animation t
          slime-net-coding-system 'utf-8-unix))

  (add-hook 'slime-mode-hook 'my/slime-mode-hook)
  (add-hook 'lisp-mode-hook 'slime-mode)

  ;; tweaks for windows-nt
  (if (eq system-type 'windows-nt)
      (setq slime-lisp-implementations
            (list (abcl ("java" "-cp" "C:\\abcl" "-jar" "C:\\abcl\\abcl.jar" "org.armedbear.lisp.Main"))
                  (ccl (list (expand-file-name "~/../../ccl/wx86cl64.exe") "-K UTF-8"))))
    (setq slime-lisp-implementations '((sbcl ("sbcl"))
                                       (ecl ("ecl"))
                                       (clisp ("clisp" "-ansi"))
                                       (chicken ("csi"))
                                       (abcl ("abcl")))))

  ;; try to find local hyperspec or fallback to use the default web site
  (setq common-lisp-hyperspec-symbol-table
        (concat
         (cond ((file-directory-p "/usr/local/share/doc/clisp-hyperspec")
                "file:/usr/local/share/doc/clisp-hyperspec/")
               ((file-directory-p "~/lisp/docs/HyperSpec")
                (concat "file:" (getenv "HOME") "/lisp/docs/HyperSpec/"))
               (t "http://www.lispworks.com/documentation/HyperSpec/"))
         "Data/Map_Sym.txt"))

  (setq slime-use-autodoc-mode t)
  (slime-setup '(slime-asdf
                 slime-indentation
                 slime-tramp
                 slime-fancy
                 slime-hyperdoc
                 slime-company ;; check how to verify it is loaded
                 ))

  (setq slime-complete-symbol*-fancy t
        slime-complete-symbol-function (if (fboundp 'counsel-cl)
                                           'counsel-cl
                                         'slime-fuzzy-complete-symbol)))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program (case system-type
                             (gnu/linux "/bin/bash")
                             (windows-nt "C:\\bin\\cmd.exe")
                             (berkeley-unix "/bin/ksh")
                             (usg-unix-v "/bin/ksh")))
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term)))

(use-package auctex
  :ensure t
  :hook (latex-mode-hook . auto-fill-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-insert-braces nil
        TeX-electric-escape t
        TeX-electric-macro t
        TeX-newline-function 'reindent-then-newline-and-indent))

(use-package hungry-delete
  :ensure t
  ;;:config (global-hungry-delete-mode)
  )

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

(use-package sly
  ;;:disabled
  :ensure t
  :config
  (add-to-list 'load-path (concat user-emacs-directory "elisp/sly/"))
  (require 'sly-autoloads)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
                                   (ecl ("ecl"))
                                   (clisp ("clisp" "-ansi"))
                                   (chicken ("csi"))
                                   (abcl ("abcl"))))
  (cond ((file-directory-p "/usr/local/share/doc/clisp-hyperspec")
         (setq common-lisp-hyperspec-root "file:/usr/local/share/doc/clisp-hyperspec/"))
        ((file-directory-p "~/lisp/docs/HyperSpec")
         (setq common-lisp-hyperspec-root (concat "file:" (getenv "HOME") "/lisp/docs/HyperSpec/")))
        (t (setq common-lisp-hyperspec-root
                 "http://www.lispworks.com/documentation/HyperSpec/")))

  (setq common-lisp-hyperspec-symbol-table
        (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (add-hook 'lisp-mode-hook 'sly-mode)

  (setq sly-use-autodoc-mode t)

  (sly-setup '(sly-fancy))
  )

(use-package sly-repl-ansi-color
  :ensure t
  :config (push 'sly-repl-ansi-color sly-contribs))

(use-package bongo
  :ensure t
  :config
  (setq bongo-custom-backend-matchers
        `((mplayer
           (local-file "file:" "http:" "ftp:")
           "ogg" "flac" "mp3" "m4a" "mka" "wav" "wma" "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "m4v" "mkv" "flv" "mov" "asf" "wmv" "rm" "rmvb" "ts")))

  ;; don't autoplay next track if playing video
  (add-hook 'bongo-player-started-hook
            (lambda ()
              (with-bongo-playlist-buffer
               (when (bongo-video-file-name-p
                      (bongo-player-get bongo-player 'file-name))
                 (setq bongo-next-action 'bongo-stop))))))

(use-package ido
  :config
  (progn
    (setq ido-save-directory-list-file (concat user-emacs-directory "ido.last")
          ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
          ido-everywhere t
          ido-case-fold  t
          ido-enable-last-directory-history t
          ido-max-work-directory-list 30
          ido-max-work-file-list 50
          ido-enable-flex-matching t
          ido-max-prospects 4
          ido-confirm-unique-completion t
          ido-completion-buffer-all-completions nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-use-url-at-point t
          ido-ignore-extensions t
          ))
  (ido-mode 1))

(use-package suomalainen-kalenteri
  :ensure t)

;; (use-package smart-mode-line
;;   :ensure t
;;   :config (sml/setup))

;; (use-package smart-mode-line-powerline-theme
;;   :ensure t
;;   :config (sml/apply-theme 'powerline))

(use-package smartparens
  :disabled
  :ensure t
  :init (require 'smartparens-config)
  :config
  (smartparens-global-strict-mode 1)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (add-to-list 'sp-lisp-modes 'sly-mode)
  (sp-local-pair #'sly-mrepl-mode "'" nil :actions nil)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))

(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)

  (defun my/company-mode-hook ()
    (define-key company-active-map (kbd "C-p") 'company-select-next)
    (define-key company-active-map (kbd "C-n") 'company-select-previous))

  (add-hook 'company-mode-hook 'my/company-mode-hook)
  :bind ("M-/" . company-complete))

(use-package company-go
    :ensure t
    :config
    (add-to-list (make-local-variable 'company-backends)
                 'company-go))

 (use-package zenburn-theme
   :ensure t)

 (use-package base16-theme
   :ensure t
   ;;:config (load-theme 'base16-gruvbox-dark-medium t)
   )

(load-theme 'wombat t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq ;;flycheck-completion-system 'ivy
        flycheck-phpcs-standard "Zend"))

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode))

(use-package rainbow-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.jsp\\'" . web-mode)
         ("\\.ap[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  (defun my/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4))
  (add-hook 'web-mode-hook 'my/web-mode-hook))

(use-package rvm
  :ensure t
  :config (rvm-use-default))

(use-package go-mode
  :ensure t
  :config
  (defun my/go-mode-hook ()
    "Options for Go language."
    (when (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
    (setq gofmt-command "goimports")
    (local-set-key (kbd "C-c m") 'gofmt)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c g i") 'go-goto-imports)
    (local-set-key (kbd "C-c C-k") 'godoc))

  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'my/go-mode-hook)
  ;;(add-hook 'go-mode-hook 'company-mode)
  )

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package godoctor
  :ensure t)

(use-package go-guru
  :ensure t)

(use-package go-errcheck
  :ensure t)

(use-package projectile-rails
  :ensure t
  :config (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" . clojure-mode)
  :config (add-hook 'clojure-mode-hook 'my/shared-lisp-hook))

(use-package cider
  :ensure t
  :config
  (setq cider-lein-parameters "repl :headless :host localhost")
  (setq nrepl-hide-special-buffers t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode))

(use-package geiser
  :ensure t
  :config
  (setq geiser-guile-binary (if (eq system-type 'berkeley-unix)
                                "guile2"
                              "guile")))

(use-package racket-mode
  :disabled
  :config
  (add-hook 'racket-mode-hook 'my/shared-lisp-hook)
  (add-hook 'racket-repl-mode-hook 'my/shared-lisp-hook))

(use-package diminish
  :ensure t)

(use-package elfeed
  :ensure t
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
          "https://planet.lisp.org/rss20.xml")))

(use-package php-mode
  :ensure t
  :mode (("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))
  :config
  (defun my/php-mode-hook ()
    (setq php-site-url "http://fi2.php.net/")
    (php-enable-symfony2-coding-style)
    (define-abbrev php-mode-abbrev-table "ex" "extends")
    (setq indent-tabs-mode nil
          tab-width 4
          c-basic-offset 4))

  (add-hook 'php-mode-hook 'my/php-mode-hook))

(use-package composer
  :ensure t
  )

(use-package company-php
  :ensure t
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (when (bound-and-true-p company-mode)
                 (require 'company-php)
                 (add-to-list (make-local-variable 'company-backends)
                              'company-ac-php-backend)))))

(use-package company-shell
  :ensure t
)

(use-package ibuffer-vc
  :disabled ;; causes a lot slow-down, music skips etc.
  :ensure t
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

;;; ------------------------------
;;; General
;;; ------------------------------

;; silence gnutls warnings
(setq gnutls-min-prime-bits nil
      gnutls-verify-error nil)

;; keep a little more history to see whats going on
(setq message-log-max 16384)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t
      next-line-add-newlines nil)

(delete-selection-mode 1)

;; confirm before exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Search and replace
(setq search-highlight t
      query-replace-highlight t
      case-fold-search t)

;; Startup
(setq initial-scratch-message ""
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Misc options
(auto-compression-mode 1)
(setq set-mark-command-repeat-pop t)

;; mouse options
(setq mouse-yank-at-point t)
(mouse-wheel-mode t)

;; Encoding
;; (setq locale-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (set-language-environment "UTF-8")
;; (set-locale-environment "en_US.UTF-8")

;; Add Spell-check for select modes if spell-checker is installed
(when (or (executable-find "aspell")
          (executable-find "ispell"))

  ;; enable flyspell for most text-based buffers
  (defun my/disable-flyspell-hook ()
    "Disable flyspell mode."
    (flyspell-mode -1))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'change-log-mode-hook 'my/disable-flyspell)
  (add-hook 'log-edit-mode-hook 'my/disable-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-issue-message-flag nil))

(defun my/text-mode-hook ()
  "Default settings for Text."
  (set-fill-column 80))

;; Hooks
(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; enable disabled features
(put 'narrow-to-region 'disabled nil)

;; https://bugs.debian.org/766397
(setq tls-program '("gnutls-cli --x509cafile %t -p %p %h"))

;;; ------------------------------
;;; Visual settings
;;; ------------------------------

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(show-paren-mode t)
(setq visible-bell t)
(setq window-min-height 3)

(blink-cursor-mode -1)
(setq-default cursor-type 'box)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(menu-bar-mode t)

;; set my font
(let ((my-font-name "tamsyn-16"))
  (when (find-font (font-spec :name my-font-name))
    (add-to-list 'default-frame-alist `(font . ,my-font-name ))
    (set-face-attribute 'default nil :font my-font-name)))

;; Maximize first frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Setup clipboard options if running in X

;; Graphical Eamcs seems to freeze when handeling clipboard, so
;; dencrease the selection timeout so it won't wait for so long.
;; https://omecha.info/blog/org-capture-freezes-emacs.html
(when (eq system-type 'berkeley-unix)
  (setq x-selection-timeout 10))

;;(setq x-select-enable-clipboard-manager nil)
;; (when (display-graphic-p))
;; (setq
;;    ;;select-enable-clipboard t
;;    ;;save-interprogram-paste-before-kill t
;;    ;;interprogram-paste-function 'gui-selection-value
;;    )

;; disable dialog boxes
(setq use-file-dialog nil
      use-dialog-box nil)

;; the modeline
(line-number-mode t)
(column-number-mode t)
(display-time-mode -1)

;; show file size
(size-indication-mode t)

;; XXX: sort these
(setq apropos-do-all t)
(setq load-prefer-newer t)

;; show buffer name in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
		 "Emacs: %b"))))

;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(use-package calendar
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
        calendar-mark-diary-entries-flag t    )
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  (setq diary-show-holidays-flag t
        diary-file (concat user-emacs-directory "diary")
        diary-display-function 'diary-fancy-display ;; XXX: is this needed
        diary-number-of-entries 7) ;; XXX: is this needed
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

;; time utilities
(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")

(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format nil
      display-time-use-mail-icon t)
;;; ------------------------------
;;; Session
;;; ------------------------------

(use-package saveplace
  :config
  (save-place-mode 1)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 50)
  (recentf-mode t))

(use-package bookmark
  :config
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
        bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode t))

(use-package abbrev
  :config
  (setq abbrev-file-name (concat user-emacs-directory "abbrev_defs")
        save-abbrevs t)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  (add-hook 'kill-emacs-hook 'write-abbrev-file))

(setq backup-directory-alist (list `("." . ,(concat user-emacs-directory "backups/")))
      make-backup-files t
      backup-by-copying t
      auto-save-timeout 600
      version-control t
      kept-new-versions 2
      kept-old-versions 3
      delete-old-versions t)

;;; ------------------------------
;;; Shell settings
;;; ------------------------------

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

;; XXX: check these
;; (require 'eshell)
;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t))

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
  (setq org-directory (concat user-emacs-directory "/org/")
        org-outline-path-complete-in-steps nil
        org-agenda-files (list org-directory)
        org-agenda-include-diary t
        org-agenda-todo-ignore-with-date t
        org-insert-mode-line-in-empty-file t
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-log-done 'note
        org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                            (sequence "|" "CANCELED(c@/!)")
                            (sequence "STALLED(s@/!)" "|")
                            (sequence "PENDING(p@/!)" "|")))

  (global-set-key (kbd "C-c l") 'org-store-link)

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c b") 'org-iswitchb)

  (add-hook 'message-mode-hook 'turn-on-orgstruct)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)

  ;; (setq org-startup-indented 'f)
  ;; (setq org-directory "~/org")
  ;; (setq org-special-ctrl-a/e 't)
  ;; (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; (define-key global-map "\C-cc" 'org-capture)
  ;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  ;; (setq org-src-fontify-natively 't)
  ;; (setq org-src-tab-acts-natively t)
  ;; (setq org-src-window-setup 'current-window)

  ;; capture notes
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (global-set-key (kbd "C-c c") 'org-capture))

;;; ------------------------------
;;; Buffer management
;;; ------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(setq ibuffer-default-sorting-mode 'major-mode
      ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (mode . dired-mode))
         ("Programming" (used-mode . prog-mode))
         ("Shell" (or (mode . shell-mode)
                      (mode . term-mode)
                      (mode . eshell-mode)))
         ("Emacs" (or (mode . emacs-lisp-mode)
                      (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")))
         ("Organization" (or (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . org-mode)
                             (mode . muse-mode)))
         ("Web"  (or (mode . w3m-mode)
                     (mode . erc-mode)))
         ("Email"  (or (mode . mu4e-mode)
                       (name . "^mu4e")))
         ("Gnus" (or (mode . message-mode)
                     (mode . bbdb-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode)
                     (name . "^\\.bbdb$")
                     (name . "^\\.newsrc-dribble"))))))

(defun my/ibuffer-mode-hook ()
  "Handle Ibuffer settings."
  (local-set-key (kbd "C-x C-f") 'counsel-find-file)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook)
(defalias 'list-buffers 'ibuffer)

;; Don't prompt if killing buffer with process attached
;; (setq kill-buffer-query-functions
;;       (remq 'process-kill-buffer-query-function
;;             kill-buffer-query-functions))

;;; -----------------------------
;;; ERC
;;; ------------------------------
(use-package erc
  :config
  (setq erc-modules (append erc-modules '(services notify spelling log)))
  (erc-update-modules)

  (setq erc-prompt-for-password nil
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit nil
        erc-kill-server-buffer-on-quit t
        erc-auto-query 'window-noselect
        erc-keywords '("zmyrgel" "tmy"))

  (add-hook 'erc-mode-hook 'erc-services-mode)
  (add-hook 'erc-mode-hook 'erc-autojoin-mode)
  (add-hook 'erc-mode-hook 'erc-match-mode)
  (add-hook 'erc-mode-hook 'erc-track-mode)
  (add-hook 'erc-mode-hook 'erc-fill-mode)
  (add-hook 'erc-mode-hook 'erc-ring-mode)
  (add-hook 'erc-mode-hook 'erc-netsplit-mode)
  (add-hook 'erc-mode-hook 'erc-timestamp-mode)
  (add-hook 'erc-mode-hook 'erc-spelling-mode)
  (add-hook 'erc-mode-hook 'erc-notify-mode)

  (setq erc-track-enable-keybindings t
        erc-track-remove-disconnected-buffers t
        erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-timestamp-format "[%R-%m/%d]"
        erc-hide-timestamps nil)
  (add-hook 'erc-mode-hook 'erc-pcomplete-mode)
  (pcomplete-erc-setup)
  (setq erc-pcomplete-order-nickname-completions t)
  (add-hook 'erc-mode-hook 'erc-log-mode)
  (setq erc-log-channels-directory "~/.irclogs/"
        erc-log-insert-log-on-open nil
        erc-log-file-coding-system 'utf-8-unix
        erc-save-buffer-on-part t)
  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (setq erc-max-buffer-size 20000)
  (defvar erc-insert-post-hook nil)
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  (setq erc-truncate-buffer-on-save t))

;;; ------------------------------
;;; Email settings
;;; ------------------------------

(setq user-mail-address "timo.myyra@bittivirhe.fi"
      user-full-name "Timo Myyrä")

;; ;; smtp mail setting; these are the same that `gnus' uses.
(setq message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-server         "smtp.fastmail.com"
      smtpmail-local-domain        "bittivirhe.fi"
      smtpmail-smtp-service        465
      smtpmail-stream-type         'ssl)

;; gnus
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.gmane.org")
        gnus-treat-hide-citation t
        gnus-cited-lines-visible '(0 . 5)
        gnus-always-read-dribble-file t
        mm-inline-large-images 'resize
        mm-discouraged-alternatives '("text/html" "text/richtext")
        mm-text-html-renderer 'shr)

  (setq gnus-secondary-select-methods
        '(
          (nnimap "work-gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "993")
                  (nnimap-stream ssl)
                  ;;(nnir-search-engine imap)
                  )
          (nnimap "wickedbsd"
                  (nnimap-address "192.168.1.120")
                  (nnimap-stream ssl))
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnimap-stream tls)))))


;; Web Browsing
(use-package eww
  :config
  (setq eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\|pdf\\)"
)
  (setq browse-url-browser-function 'eww-browse-url
        browse-url-new-window-flag nil
        browse-url-firefox-new-window-is-tab t))

;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(setq compilation-save-buffers-predicate nil
      compilation-ask-about-save nil
      compilation-window-height 12
      gdb-many-windows t)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(setq diff-switches '("-u"))

(add-hook 'prog-mode-hook 'subword-mode)

(defun my/prog-mode-hook ()
  "Generel programming mode options."
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail)))
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;;; CC-mode styles

(use-package irony
  :disabled
  :ensure t)

(use-package company-irony
  :ensure t)

(use-package flycheck-irony
  :ensure t
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package prog-mode
  :config
  (defun my/prog-mode-hook ()
    "Hook to run when entering generic prog-mode."
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                   1 font-lock-warning-face prepend))))
  (add-hook 'prog-mode-hook 'my/prog-mode-hook))

(use-package cc-mode
  :config
  (define-key c-mode-map (kbd "C-c m") 'man-follow)
  (define-key c-mode-map (kbd "C-c C-d") 'gdb)
  (define-key c-mode-map (kbd "RET") 'c-context-line-break)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)

  (defun my/c-mode-common ()
    "Programming options shared for C-like languages."

    (when (bound-and-true-p company-mode)
      (add-to-list (make-local-variable 'company-backends)
                   'company-irony))

    ;;(setq company-backends (delete 'company-semantic company-backends))
    (setq which-func-unknown "TOP LEVEL"
          compilation-scroll-output 'first-error))

  (add-hook 'c-mode-common-hook 'which-function-mode)
  (add-hook 'c-mode-common-hook 'cwarn-mode)

  (defun my/c-mode ()
    "My C programming options."
    (c-set-style "bsd")
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail)))

  (defun my/c++-mode ()
    "My C++ programming options."
    (setq fill-column 100)
    (c-set-style "stroustrup")
    (setq whitespace-line-column 100
          whitespace-style '(face lines-tail)))

  ;;(add-hook 'c-mode-common-hook 'my/c-mode-common)
  (add-hook 'c-mode-hook 'my/c-mode)
  (add-hook 'c++-mode-hook 'my/c++-mode))

(use-package cperl-mode
  :config
  (defalias 'perl-mode 'cperl-mode)
  (defun my/cperl-mode-hook ()
    "Default CPerl settings."
    (setq cperl-fontlock t
          cperl-info-on-command-no-prompt t
          cperl-clobber-lisp-bindings t
          cperl-lazy-help-time 5
          cperl-indent-level 4
          cperl-invalid-face 'default))
  (add-hook 'cperl-mode-hook 'my/cperl-mode-hook))

;;; Ruby settings
(defun my/css-mode-hook ()
  (setq css-indent-level 2)
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'my/css-mode-hook)

(defun my/shared-lisp-hook ()
  ;;(rainbow-delimiters-mode t)
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail)))

(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'my/shared-lisp-hook))

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook 'my/shared-lisp-hook))

;; (use-package scheme-mode
;;   :config
;;   (add-hook 'scheme-mode-hook 'my/shared-lisp-hook))

;; ruby
(use-package ruby-mode
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"  . ruby-mode))
  :config
  (defun my/ruby-mode-hook ()
    (setq ruby-deep-arglist t)
    (setq ruby-deep-indent-paren nil)
    (setq c-tab-always-indent nil))
  (add-hook 'ruby-mode-hook 'my/ruby-mode-hook))

;; Javascript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;;; ------------------------------
;;; Completion
;;; ------------------------------

(setq dabbrev-case-replace nil)

(icomplete-mode t)
(setq icomplete-prospects-height 2
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      tab-always-indent 'complete
      completion-styles (append completion-styles '(initials)))

;;; ------------------------------
;;; Dired options
;;; ------------------------------

(setq dired-isearch-filenames t
      dired-ls-F-marks-symlinks t)

;; Don't pass --dired flag to ls on BSD
(when (eq system-type 'berkeley-unix)
  (setq dired-use-ls-dired nil))

(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Enhance dired mode
(defun my/dired-load-hook ()
  "My Dired load hook."
  ;; Bind dired-x-find-file.
  ;;(setq dired-x-hands-off-my-keys nil)
  (load "dired-x")
  (setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$"
        dired-guess-shell-alist-user
        '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "mplayer"
           "\\.rar$" "unrar e"))
        dired-guess-shell-gnutar (if (eq system-type 'berkeley-unix)
                                     "gtar"
                                   "tar")))

(add-hook 'dired-load-hook 'my/dired-mode-hook)

(defun my/dired-mode-hook ()
  (setq truncate-lines t))

;;(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'my/dired-mode-hook)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map (kbd "C-x C-j") 'dired-jump)
(define-key global-map (kbd "C-x 4 C-j") 'dired-jump-other-window)

;;; ------------------------------
;;; Functions
;;; ------------------------------

(defun rename-current-file-or-buffer ()
  "Rename current buffer."
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

(defun find-alternative-file-with-sudo ()
  "Try to open file with sudo."
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

;;; ------------------------------
;;; Keybindings
;;; ------------------------------

(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-c R") 'rename-current-file-or-buffer)

(global-set-key (kbd "M-z") 'zap-up-to-char)

;; shortcuts
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'rgrep)
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "<f11>") 'gnus)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

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
(when (file-exists-p (concat user-emacs-directory "init-local.el"))
  (load (concat user-emacs-directory "init-local.el")))

(provide 'init)

;;; init.el ends here
