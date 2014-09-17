;;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;;
;;;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;;; Time-stamp: <2014-09-17 13:48:24 (tmy)>
;;;; URL: http://github.com/zmyrgel/dotfiles
;;;; Compatibility: GNU Emacs 23.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TODO:
;;;; - Autoloads for gnus
;;;; - indentation of programming modes
;;;; - ERC configuration, modules and such

;; Define few utilities
(defun concat-path (&rest parts)
  "Utility to concatenate path"
  (let ((result nil))
    (dolist (path parts (directory-file-name (expand-file-name result)))
      (if result
          (setq result (concat result "/" path))
        (setq result path)))))

(defun add-extension (ext)
  "Adds extension ext to load-path if it names existing directory.
   If it names existing file, it loads it."
  (cond ((file-directory-p ext)
         (add-to-list 'load-path ext))
        ((file-exists-p ext)
         (load ext))
        (t (warn (concat "Not a directory or file: " ext)))))

;; Provide few defaults
(defconst emacs-dir (concat-path (getenv "HOME") ".emacs.d"))
(defconst elisp-dir (concat-path emacs-dir "elisp"))
(defconst elpa-dir (concat-path emacs-dir "elpa"))
(setq custom-file (concat-path emacs-dir "custom.el"))
(load custom-file 'noerror)

;; Ensure ELPA exists
(when (not (file-directory-p elpa-dir))
  (make-directory elpa-dir t))

;; Add emacs dir to load path
(add-extension emacs-dir)
(add-extension elisp-dir)

(when (not (boundp 'custom-theme-load-path))
  (setq custom-theme-load-path nil))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(defvar *my-packages*
  '(
    ;; general
    ace-jump-mode
    auctex
    keyfreq
    org
    suomalainen-kalenteri
    undo-tree
    multi-term

    ;; web browsing
    w3m

    ;; completion
    auto-complete
    company
    idomenu
    ag
    smex
    grizzl

    ;; email
    bbdb
    boxquote

    ;; themes
    pastels-on-dark-theme
    professional-theme

    ;; programming
    ;autopair
    smartparens
    mode-compile
    flymake-json
    yasnippet
    rainbow-mode
    projectile
    yaml-mode
    magit

    ;; web stuff
    web-mode
    haml-mode

    ;; php stuff
    php-mode
    flymake-php
    flymake-phpcs

    ;; lisp
    geben
    chicken-scheme
    cider
    clojure-mode
    quack
    paredit
    redshank

    ;; ruby stuff
    flymake-ruby
    rvm
    inf-ruby
    projectile-rails
    omniref
    rspec-mode
    ruby-compilation
    robe
    bundler
    feature-mode
    ))

;; only for fresh install
(unless package-archive-contents
  (package-refresh-contents))

;; Install my packages
(dolist (p *my-packages*)
  (when (not (package-installed-p p))
    (package-install p)))

;; store command frequency stats
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; ------------------------------
;;; General
;;; ------------------------------

;; silence gnutls warnings
(setq gnutls-min-prime-bits nil)

;; Bump threshold to avoid constant garbage collection
(setq gc-cons-threshold 20000000)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t
      next-line-add-newlines nil)

(delete-selection-mode 1)

;; Search and replace
(setq search-highlight t
      query-replace-highlight t
      dabbrev-case-replace nil
      case-fold-search t)

;; Startup
(setq initial-scratch-message ""
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Misc options
(auto-compression-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq set-mark-command-repeat-pop t)

;; mouse options
(setq mouse-yank-at-point t)

;; Encoding
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")

;; Add Spell-check for select modes if spell-checker is installed
(when (fboundp 'flyspell-mode)
  (when (or (executable-find "aspell")
            (executable-find "ispell"))
    ;; enable flyspell for most text-based buffers
    (dolist (hook '(text-mode-hook org-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))
    ;; enable flyspell to check comments in source code
    (dolist (hook '(prog-mode-hook))
      (add-hook hook (lambda () (flyspell-prog-mode))))

    (setq flyspell-issue-message-flag nil)))

;; Hooks
(add-hook 'text-mode-hook (lambda () (set-fill-column 80)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; enable disabled features
(put 'narrow-to-region 'disabled nil)

;;; ------------------------------
;;; Visual settings
;;; ------------------------------

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(show-paren-mode t)
(setq visible-bell 1)
(setq window-min-height 3)
(blink-cursor-mode -1)

(setq-default cursor-type 'hbar)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode nil))
(when (fboundp 'mouse-wheel-mode)
  (mouse-wheel-mode t))

(setq default-frame-alist '((font-backend . "xft")
                            (font . "gohufont-10")
                            (left-fringe . -1)
                            (right-fringe . -1)
                            (fullscreen . 1)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

;; Setup clipboard options if running in X
(cond ((eq window-system 'x)
       (setq x-select-enable-clipboard t
             interprogram-paste-function 'x-cut-buffer-or-selection-value))
      ((or (not window-system)
           (fboundp 'menu-bar-mode))
       (menu-bar-mode nil)))

;; disable dialog boxes
(setq use-file-dialog nil
      use-dialog-box nil)

;; the modeline
(when (fboundp 'line-number-mode)
  (line-number-mode t))
(when (fboundp 'column-number-mode)
  (column-number-mode t))
(when (fboundp 'display-time-mode)
  (display-time-mode 1))

;; show file size
(when (fboundp size-indication-mode)
  (size-indication-mode t))

;; set theme
(load-theme 'professional)

;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")

(setq calendar-week-start-day 1
      calendar-day-name-array
      ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
       "torstai" "perjantai" "lauantai"]
      calendar-month-name-array
      ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
       "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
       "lokakuu" "marraskuu" "joulukuu"])

(setq diary-file  (concat emacs-dir "/diary")
      display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format nil
      display-time-use-mail-icon t
      calendar-time-zone 120
      calendar-latitude 60.2
      calendar-longitude 25.0
      diary-display-function 'diary-fancy-display
      diary-show-holidays-flag t
      calendar-mark-holidays-flag t
      calendar-view-diary-initially-flag t
      calendar-date-style 'european
      calendar-mark-diary-entries-flag t
      diary-number-of-entries 7)

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;;; ------------------------------
;;; Session
;;; ------------------------------

(when (fboundp 'recentf-mode)
  (recentf-mode))

(setq bookmark-default-file (concat-path emacs-dir "emacs.bmk")
      bookmark-save-flag 1)

(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat-path emacs-dir "savehist"))
(savehist-mode t)

(setq abbrev-file-name (concat-path emacs-dir "abbrev_defs")
      save-abbrevs t)

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook 'write-abbrev-file)

(setq backup-directory-alist (list `("." . ,(concat-path emacs-dir "backups")))
      make-backup-files t
      backup-by-copying t
      auto-save-timeout 600
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;;; ------------------------------
;;; Shell settings
;;; ------------------------------

(setq shell-command-switch "-c"
      explicit-sh-args '("-login" "-i"))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-scroll-show-maximum-output t
      comint-completion-autolist t
      comint-input-ignoredups t
      comint-completion-addsuffix t
      comint-prompt-read-only t)

(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
      eshell-save-history-on-exit t
      eshell-scroll-show-maximum-output t
      eshell-scroll-to-bottom-on-output t)

;;; customization for term, ansi-term
;; disable cua and transient mark modes in term-char-mode
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'transient-mark-mode) t))
(ad-activate 'term-line-mode)

(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)

;;; ------------------------------
;;; Org-mode
;;; ------------------------------

(eval-after-load 'org-mode
  '(progn
     (setq org-directory (concat-path emacs-dir "/org")
           org-agenda-files (list org-directory)
           org-agenda-include-all-todo t ;; deprecated, find better way
           org-agenda-include-diary t
           org-agenda-todo-ignore-with-date t
           org-default-notes-file (concat-path org-directory "/notes.org")
           org-completion-use-ido t
           org-outline-path-complete-in-steps nil
           org-insert-mode-line-in-empty-file t
           org-mobile-inbox-for-pull (concat-path org-directory "/flagged.org")
           org-mobile-directory (concat-path org-directory "/MobileOrg")
           org-enforce-todo-checkbox-dependencies t
           org-enforce-todo-dependencies t
           org-log-done 'note
           org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                               (sequence "|" "CANCELED(c@/!)")
                               (sequence "STALLED(s@/!)" "|")
                               (sequence "PENDING(p@/!)" "|")))

     (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
     (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

     (setq org-capture-templates
           '(("m" "Meeting" entry (file (concat org-directory "/meetings.org"))
              "* TODO %?\t:work:meeting:\n  %i\n  %a")
             ("a" "Task" entry (file (concat org-directory "/work.org"))
              "* TODO %?\t:work:task:\n  %i\n  %a")
             ("f" "Defect" entry (file (concat org-directory "/work.org"))
              "* TODO %?\t:work:defect:\n  %i\n  %a")
             ("e" "Enhancement" entry (file (concat org-directory "/work.org"))
              "* TODO %?\t:work:enchancement:\n  %i\n  %a")
             ("u" "System update" entry (file (concat org-directory "/work.org"))
              "* TODO %?\t:work:update:\n  %i\n  %a")
             ("p" "Project" entry (file (concat org-directory "/work.org"))
              "* TODO %?\t:project:\n  %i\n  %a")
             ("s" "Study" entry (file (concat org-directory "/work.org"))
              "* TODO %?\t:work:study:\n  %i\n  %a")
             ("t" "TODO entry" entry (file (concat org-directory "/gtd.org"))
              "* TODO %?\t:misc:\n  %i\n  %a")))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)



;;; ------------------------------
;;; Buffer management
;;; ------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'ibuffer)
  (setq ibuffer-default-sorting-mode 'major-mode
        ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Programming" (or (mode . sh-mode)
                              (mode . scheme-mode)
                              (mode . java-mode)
                              (mode . malabar-mode)
                              (mode . jde-mode)
                              (mode . cperl-mode)
                              (mode . sepia-mode)
                              (mode . ruby-mode)
                              (mode . python-mode)
                              (mode . c-mode)
                              (mode . c++-mode)
                              (mode . clojure-mode)
                              (mode . lisp-mode)
                              (mode . php-mode)))
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
           ("Gnus" (or (mode . message-mode)
                       (mode . bbdb-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\.bbdb$")
                       (name . "^\\.newsrc-dribble"))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))

  (defalias 'list-buffers 'ibuffer))

;;; ------------------------------
;;; ERC
;;; ------------------------------

(eval-after-load 'erc
  '(progn
     (setq erc-modules (append erc-modules '(services notify spelling log)))
     (erc-update-modules)

     (setq erc-prompt-for-password nil
           erc-kill-buffer-on-part t
           erc-kill-queries-on-quit nil
           erc-kill-server-buffer-on-quit t
           erc-auto-query 'window-noselect
           erc-keywords '("zmyrgel"))

     (erc-services-mode 1)
     (erc-autojoin-mode 1)
     (erc-match-mode 1)
     (erc-track-mode 1)
     (erc-fill-mode 1)
     (erc-ring-mode 1)
     (erc-netsplit-mode 1)
     (erc-timestamp-mode 1)
     (erc-spelling-mode 1)
     (erc-notify-mode 1)
     (setq erc-track-enable-keybindings t
           erc-track-remove-disconnected-buffers t
           erc-track-exclude-server-buffer t
           erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                     "324" "329" "332" "333" "353" "477"))
     (setq erc-timestamp-format "[%R-%m/%d]"
           erc-hide-timestamps nil)
     (erc-pcomplete-mode 1)
     (pcomplete-erc-setup)
     (setq erc-pcomplete-order-nickname-completions t)
     (erc-log-mode 1)
     (setq erc-log-channels-directory "~/.irclogs/"
           erc-log-insert-log-on-open nil
           erc-log-file-coding-system 'utf-8-unix
           erc-save-buffer-on-part t)
     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
     (setq erc-max-buffer-size 20000)
     (defvar erc-insert-post-hook nil)
     (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
     (setq erc-truncate-buffer-on-save t)))

;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(setq compilation-save-buffers-predicate '(lambda () nil)
      compilation-ask-about-save nil
      compilation-window-height 12
      gdb-many-windows t)

;; project management
(when (fboundp 'projectile-global-mode)
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'grizzl))

;; add autopairing
(require 'smartparens-config)
(smartparens-global-mode t)

;;; CC-mode styles

(defun new-c-lineup-arglist (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (let ((syntax (car (car (c-guess-basic-syntax)))))
      (while (or (eq syntax 'arglist-intro)
                 (or (eq syntax 'arglist-cont)
                     (eq syntax 'arglist-cont-nonempty)))
        (forward-line -1)))
    (beginning-of-line)
    (re-search-forward "[^ \t]" (c-point 'eol))
    (goto-char (+ (match-beginning 0) 4))
    (vector (current-column))))

(c-add-style "openbsd"
	     '("bsd"
	       (c-ignore-auto-fill . '(string))
	       (c-subword-mode . 1)
	       (c-basic-offset . 8)
	       (c-label-minimum-indentation . 0)
               (c-offsets-alist .
                                ((arglist-intro . new-c-lineup-arglist)
                                 (arglist-cont . new-c-lineup-arglist)
                                 (arglist-cont-nonempty . new-c-lineup-arglist)
                                 (arglist-close . 0)
                                 (substatement-open . 0)
                                 (statement-cont . *)
                                 (case-label . 0)
                                 (knr-argdecl . *)))
	       (fill-column . 80)
               (tab-width . 8)
	       (indent-tabs-mode . t)))

(setq c-default-style '((java-mode . "java")
                        (c-mode . "openbsd")
                        (c++-mode . "stroustrup")))

;; C programming

(defun my-c-mode-common ()
  (interactive)
  (hs-minor-mode t)
  ;; enable auto pairing mode
  (cond ((fboundp 'smartparens-mode) (smartparens-mode 1))
        ((fboundp 'autopair-mode) (autopair-mode 1))
        ((fboundp 'electric-pair-mode) (electric-pair-mode 1)))
  (which-function-mode t)
  (cwarn-mode 1)
  (cond ((fboundp 'subword-mode)
         (subword-mode 1))
        ((fboundp 'c-subword-mode)
         (c-subword-mode 1)))

  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                 1 font-lock-warning-face prepend)))
  (setq which-func-unknown "TOP LEVEL"
        compilation-scroll-output 'first-error
        compilation-read-command nil
        c-hungry-delete-key t)

  (local-set-key (kbd "C-c m") 'man-follow)
  (local-set-key (kbd "C-c C-c") 'mode-compile)
  (local-set-key (kbd "C-c C-d") 'gdb)
  (local-set-key (kbd "RET") 'c-context-line-break)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-M-i") 'semantic-ia-complete-symbol))

(defun my-c-mode ()
  (c-set-style "openbsd")
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail)))

(defun my-c++-mode ()
  (setq fill-column 100)
  (c-set-style "stroustrup")
  (setq whitespace-line-column 100
        whitespace-style '(face lines-tail)))

(add-hook 'c-mode-common-hook 'my-c-mode-common)
(add-hook 'c-mode-hook 'my-c-mode)
(add-hook 'c++-mode-hook 'my-c++-mode)

(when (fboundp 'cperl-mode)
  (defalias 'perl-mode 'cperl-mode))

(add-hook 'cperl-mode-hook
          (lambda ()
            (flymake-mode 1)
            (when (fboundp 'electric-pair-mode)
              (electric-pair-mode 0))
            (setq cperl-fontlock t
                  cperl-electric-lbrace-space t
                  cperl-electric-parens t
                  cperl-electric-linefeed t
                  cperl-electric-keywords t
                  cperl-info-on-command-no-prompt t
                  cperl-clobber-lisp-bindings t
                  cperl-lazy-help-time 5
                  cperl-indent-level 4
                  cperl-auto-newline t
                  cperl-invalid-face 'default)
            (local-set-key (kbd "C-h f") 'cperl-perldoc)))

(when (fboundp 'flymake-phpcs-load)
  (setq flymake-phpcs-standard "Zend"))

(when (fboundp 'php-mode)

  (add-to-list 'auto-mode-alist
               '("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))

  (defun my-php-mode-hook ()
    (setq php-manual-url "http://www.php.net/manual/en"
          php-search-url "http://www.php.net/"
          whitespace-line-column 80
          whitespace-style '(face lines-tail))
    (php-enable-symfony2-coding-style)
    (flymake-phpcs-load))

  (add-hook 'php-mode-hook 'my-php-mode-hook))

(when (fboundp 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (defun web-mode-hook ()
    "Hooks for Web mode."
    ;; Indentation HTML offset indentation
    (setq web-mode-markup-indent-offset 2)
    ;; CSS offset indentation
    (setq web-mode-css-indent-offset 2)
    ;;Script offset indentation (for JavaScript, Java, PHP, etc.)
    (setq web-mode-code-indent-offset 2)))
  (add-hook 'web-mode-hook 'web-mode-hook)

;;; ruby settings
(when (fboundp 'ruby-mode)
    (add-to-list 'auto-mode-alist
                 '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
    (add-to-list 'auto-mode-alist
                 '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq ruby-deep-arglist t)
               (setq ruby-deep-indent-paren nil) ;; handle function param indent
               (setq c-tab-always-indent nil))))

(when (fboundp 'flymake-ruby)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

;; test following before enabling
;; (when (fboundp 'robe-mode)
;;   (add-hook 'ruby-mode-hook 'robe-mode))

(when (fboundp 'rvm)
  (rvm-use-default))

(when (fboundp 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(when (fboundp 'css-mode)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

;;; Lisp settings
(autoload 'paredit-mode "paredit" "Paredit-mode" nil)

(defun my-shared-lisp-hook ()
  (when (fboundp 'paredit-mode)
    (paredit-mode 1))
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode 1))
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'my-shared-lisp-hook)

(add-hook 'lisp-mode-hook 'my-shared-lisp-hook)
(add-hook 'lisp-mode-hook (lambda () (slime-mode 1)))

;; Scheme settings
(when (fboundp 'chicken-slime)
  (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
  (add-hook 'scheme-mode-hook 'my-shared-lisp-hook)
  (setq scheme-program-name "csi")
  (add-to-list 'load-path "/var/lib/chicken/5/"))

(when (fboundp 'chicken-scheme)
  (require 'chicken-scheme)
  (add-hook 'scheme-mode-hook 'my-shared-lisp-hook))

;; clojure
(when (fboundp 'clojure-mode)
  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook 'my-shared-lisp-hook))

(when (fboundp  'cider)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode))

;;; ------------------------------
;;; Completion
;;; ------------------------------

(icomplete-mode t)
(setq icomplete-prospects-height 2
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      tab-always-indent 'complete
      completion-styles (append completion-styles '(initials)))

(when (fboundp 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(yas/hippie-try-expand
	  try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs))
  (global-set-key (kbd "M-/") 'hippie-expand))

(eval-after-load 'ido
  '(progn
     (add-hook 'ibuffer-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-x C-f") 'ido-find-file)))
     (ido-everywhere 1)
     (setq ido-save-directory-list-file (concat emacs-dir "/ido.last")
           ido-ignore-buffers
           '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
           ido-everywhere t
           ido-case-fold  t
           ido-enable-last-directory-history t
           ido-max-work-directory-list 30
           ido-max-work-file-list      50
           ido-enable-flex-matching t
           ido-max-prospects 4
           ido-confirm-unique-completion t
           ido-completion-buffer-all-completions nil
           ido-create-new-buffer 'always
           ido-use-filename-at-point nil
           ido-use-url-at-point t)

     (when (fboundp 'idomenu)
       (global-set-key (kbd "C-c i") 'idomenu))))

(when (fboundp 'ido-mode)
  (ido-mode 1))

;;; ------------------------------
;;; Dired options
;;; ------------------------------

(setq dired-isearch-filenames t
      dired-ls-F-marks-symlinks t)

;; Don't pass --dired flag to ls on BSD
(when (eq system-type 'berkeley-unix)
  (setq dired-use-ls-dired nil))

;; Enhance dired mode
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (setq dired-omit-files "^#\\|\\.$\\|^\\."
                  dired-guess-shell-alist-user
                  '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "mplayer"
                     "\\.rar$" "unrar e")))))

(add-hook 'dired-mode-hook
          (lambda ()
            (hl-line-mode 1)
            (setq truncate-lines t)))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map (kbd "C-x C-j") 'dired-jump)
(define-key global-map (kbd "C-x 4 C-j") 'dired-jump-other-window)

;;; ------------------------------
;;; Functions
;;; ------------------------------

(defun ssh ()
  "Simple command to open remote shell through SSH."
  (interactive)
  (cd (read-string "Path:" "/ssh:"))
  (shell))

(defun rename-current-file-or-buffer ()
  "Rename current buffer"
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

(defun quit-prompt ()
  "Prompts before exiting emacs."
  (interactive)
  (when (y-or-n-p "Quit terminal? ")
    (save-buffers-kill-terminal)))

;;; ------------------------------
;;; Keybindings
;;; ------------------------------

(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-x C-c") 'quit-prompt)
(global-set-key (kbd "C-c R") 'rename-current-file-or-buffer)

;; shortcuts
(global-set-key (kbd "<f11>") 'gnus)
(global-set-key (kbd "<f12>") 'bookmark-bmenu-list)

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x v /") 'magit-status))

(when (fboundp 'ace-jump-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode))

;; Browser
(setq browse-url-browser-function
      (cond ((and (locate-library "w3m") (executable-find "w3m")) 'w3m-browse-url)
            ((executable-find "firefox") 'browse-url-firefox)
            ((executable-find "lynx") 'lynx)
            (t nil)))

;;; ------------------------------
;;; External packages
;;; ------------------------------

(autoload 'yas/hippie-try-expand "yasnippet")
(eval-after-load "yasnippet"
  '(progn
     (yas-global-mode 1)))

;; Web Browsing
(eval-after-load 'w3m-search
  '(progn
     (add-to-list 'w3m-search-engine-alist '("duckduckgo" "https://duckduckgo.com/?q=%s"))
     (add-to-list 'w3m-search-engine-alist '("fi.wikipedia" "http://fi.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8))
     (setq w3m-search-default-engine "duckduckgo")))

(eval-after-load 'w3m
  '(progn
     (when (fboundp 'newsticker-treeview)
       (setq newsticker-html-renderer 'w3m-region))

     (setq browse-url-browser-function 'w3m-browse-url
           browse-url-new-window-flag t
           browse-url-firefox-new-window-is-tab t
           w3m-use-form t
           w3m-default-display-inline-images t
           w3m-use-cookies t
           w3m-use-tab nil
           url-keep-history t
           w3m-profile-directory emacs-dir
           w3m-default-save-directory "~/Downloads"
           w3m-coding-system 'utf-8
           w3m-file-coding-system 'utf-8
           w3m-file-name-coding-system 'utf-8
           w3m-output-coding-system 'utf-8
           w3m-terminal-coding-system 'utf-8
           w3m-home-page "http://www.openbsd.org")

     (defun my-w3m-hook ()
       (define-key w3m-mode-map "z" 'w3m-previous-buffer)
       (define-key w3m-mode-map "x" 'w3m-next-buffer))

     (defun my-w3m-rename-buffer (url)
       "base buffer name on title"
       (let* ((size 32)
              (title w3m-current-title)
              (name (truncate-string-to-width
                     (replace-regexp-in-string " " "_" title)
                     size)))
         (rename-buffer name t)))

     (add-hook 'w3m-mode-hook 'my-w3m-hook)
     (add-hook 'w3m-display-hook 'my-w3m-rename-buffer)

     (defadvice w3m-modeline-title (around my-w3m-modeline-title)
       "prevent original function from running; cleanup remnants"
       (setq w3m-modeline-separator ""
             w3m-modeline-title-string ""))
     (ad-activate 'w3m-modeline-title)))

(eval-after-load 'w3m-search
  '(progn
     (setq w3m-session-file (concat-path emacs-dir "w3m-session")
           w3m-session-save-always t
           w3m-session-load-always t
           w3m-session-show-titles t
           w3m-session-duplicate-tabs 'never)))

;;; Auctex
(eval-after-load 'auctex
  '(progn
     (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
     (setq TeX-auto-save t
           TeX-parse-self t
           TeX-insert-braces nil
           TeX-electric-escape t
           TeX-electric-macro t
           TeX-newline-function 'reindent-then-newline-and-indent)))

(eval-after-load 'multi-term
  '(progn
     (setq multi-term-program (case system-type
                                (gnu/linux "/bin/bash")
                                (windows-nt "C:\\bin\\cmd.exe")
                                (berkeley-unix "/bin/ksh")
                                (usg-unix-v "/bin/ksh")))))
(when (fboundp 'multi-term)
  (global-set-key (kbd "C-c t") 'multi-term-next)
  (global-set-key (kbd "C-c T") 'multi-term))

;; smex
(autoload 'smex "smex" "Smex" t)
(autoload 'smex-major-mode-commands "smex" "Smex" t)
(eval-after-load 'smex
  '(progn
     (smex-initialize)
     (setq smex-save-file (concat-path emacs-dir "/smex-items"))))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; undo-tree
(autoload 'global-undo-tree-mode "undo-tree")
(eval-after-load 'undo-tree
  '(progn
     (global-undo-tree-mode)))

;; quack
(eval-after-load 'quack
  '(progn
     (setq quack-default-program "csi"
           quack-dir (concat-path emacs-dir "quack")
           quack-fontify-style nil
           quack-newline-behavior 'indent-newline-indent
           quack-pretty-lambda-p nil
           quack-remap-find-file-bindings-p nil
           quack-run-scheme-always-prompts-p nil
           quack-run-scheme-prompt-defaults-to-last-p t
           quack-smart-open-paren-p t
           quack-switch-to-scheme-method 'other-window)))

;; gnus
(eval-after-load 'gnus
  '(progn
     (setq gnus-select-method '(nntp "news.gmane.org")
           mm-inline-text-html-with-images t
           mm-inline-large-images 'resize
           mm-discouraged-alternatives '("text/html" "text/richtext")
           gnus-treat-hide-citation t
           gnus-cited-lines-visible '(0 . 5))

     ;; Set renderer for HTML
     (setq mm-text-html-renderer
           (cond ((and (locate-library "w3m") (executable-find "w3m")) 'w3m)
                 ((fboundp 'libxml-parse-html-region) 'shr)
                 ((executable-find "w3m") 'gnus-w3m)
                 ((executable-find "lynx") 'lynx)
                 (t nil)))))


;; ruby-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; Slime
;; Load local slime installation and fallback to quicklisp slime otherwise
(cond ((file-directory-p (concat elisp-dir "/slime"))
       (add-to-list 'load-path (concat elisp-dir "/slime"))
       (require 'slime-autoloads))
      ((file-directory-p (expand-file-name "~/quicklisp/slime-helper.el"))
       (load (expand-file-name "~/quicklisp/slime-helper.el")))
      ((file-directory-p (expand-file-name "~/../../quicklisp/slime-helper.el"))
       (load (expand-file-name "~/../../quicklisp/slime-helper.el"))))

(eval-after-load 'slime
  '(progn
     (setq slime-description-autofocus t
           slime-repl-history-trim-whitespaces t
           slime-repl-wrap-history t
           slime-repl-history-file (concat emacs-dir "slime-history.eld")
           slime-repl-history-remove-duplicates t
           slime-ed-use-dedicated-frame t
           slime-kill-without-query-p t
           slime-startup-animation t
           slime-net-coding-system 'utf-8-unix
           slime-lisp-implementations
           '((sbcl  ("sbcl"))
             (ecl  ("ecl"))
             (chicken ("csi"))
             (clisp ("clisp" "-ansi"))))

     ;; tweaks for windows-nt
     (if (eq system-type 'windows-nt)
         (progn
           (push '(abcl ("java" "-cp" "C:\\abcl" "-jar" "C:\\abcl\\abcl.jar" "org.armedbear.lisp.Main"))
                 slime-lisp-implementations)
           (push '(ccl ,(list (expand-file-name "~/../../ccl/wx86cl64.exe") "-K UTF-8"))
                 slime-lisp-implementations))
       (progn
         (push '(abcl ("abcl")) slime-lisp-implementations)))

     ;; try to find local hyperspec or fallback to use the default web site
     (cond ((file-directory-p "/usr/local/share/doc/clisp-hyperspec")
            (setq common-lisp-hyperspec-root "file:/usr/local/share/doc/clisp-hyperspec/"))
           ((file-directory-p "~/lisp/docs/HyperSpec")
            (setq common-lisp-hyperspec-root "file:~/lisp/docs/HyperSpec/"))
           (t (setq common-lisp-hyperspec-root
                    "http://www.lispworks.com/documentation/HyperSpec/")))

     (setq common-lisp-hyperspec-symbol-table
           (concat-path common-lisp-hyperspec-root "Data/Map_Sym.txt"))

     (add-hook 'lisp-mode-hook (lambda ()
                                 (slime-mode 1)))
     (add-hook 'slime-repl-mode-hook (lambda ()
                                       (paredit-mode 1)))

     (global-set-key (kbd "C-c s") 'slime-selector)
     (def-slime-selector-method ?l
       "most recently visited lisp-mode buffer."
       (slime-recently-visited-buffer 'lisp-mode))
     (def-slime-selector-method ?c
       "most recently visited scheme-mode buffer."
       (slime-recently-visited-buffer 'scheme-mode))
     (def-slime-selector-method ?j
       "most recently visited clojure-mode buffer."
       (slime-recently-visited-buffer 'clojure-mode))

     (setq slime-use-autodoc-mode t)

     (slime-setup '(slime-asdf
                    slime-indentation
                    slime-tramp
                    slime-fancy
                    slime-hyperdoc))

     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;; Load optional local startup files
(add-extension (concat-path emacs-dir "init-local.el"))
