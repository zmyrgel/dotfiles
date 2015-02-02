;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2015-02-02 22:35:22 (zmyrgel)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 23.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - Autoloads for gnus
;;; - check key bindings, define-key etc.
;;; - fix warnings on this file
;;; - test if ido-flx could be used instead of grizzl
;;; - test if lispy could replace paredit
;;; - Move keybindings and settings to with-eval-after-load
;;; - add minor modes in hooks

;;; Code:

;; Define few utilities
(defun concat-path (&rest parts)
  "Utility to concatenate path from PARTS."
  (let ((result nil))
    (dolist (path parts (directory-file-name (expand-file-name result)))
      (if result
          (setq result (concat result "/" path))
        (setq result path)))))

(defun add-extension (ext)
  "Add extension EXT to 'load-path' if it names existing directory.
If it names existing file, it loads it."
  (cond ((file-directory-p ext)
         (add-to-list 'load-path ext))
        ((file-exists-p ext)
         (load ext))
        (t (warn (concat "Not a directory or file: " ext)))))

;; Provide few defaults
(require 'cl) ; required by grizzl at least
(defconst elisp-dir (concat-path user-emacs-directory "elisp"))
(defconst elpa-dir (concat-path user-emacs-directory "elpa"))
(setq custom-file (concat-path user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Ensure ELPA exists
(when (not (file-directory-p elpa-dir))
  (make-directory elpa-dir t))

;; Add emacs dir to load path
(add-extension elisp-dir)

(when (not (boundp 'custom-theme-load-path))
  (setq custom-theme-load-path nil))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(defvar *my-packages*
  '(ace-jump-mode
    auctex
    org
    suomalainen-kalenteri
    multi-term
    smart-mode-line
    smart-mode-line-powerline-theme
    w3m
    company
    ag
    smex
    grizzl
    bbdb
    boxquote
    pastels-on-dark-theme
    zenburn-theme
    smartparens
    mode-compile
    flycheck
    yasnippet
    rainbow-mode
    projectile
    ibuffer-projectile
    yaml-mode
    magit
    slime
    web-mode
    haml-mode
    php-mode
    geben
    ;;geiser
    scheme-complete
    cider
    clojure-mode
    quack
    paredit
    redshank
    rvm
    inf-ruby
    projectile-rails
    omniref
    rspec-mode
    ruby-compilation
    robe
    bundler
    feature-mode
    company-go
    go-direx
    go-eldoc
    go-errcheck
    go-mode
    go-play
    go-projectile
    go-snippets
    go-stacktracer
    racket-mode
    ))

;; only for fresh install
(unless package-archive-contents
  (package-refresh-contents))

;; Install my packages
(dolist (p *my-packages*)
  (when (not (package-installed-p p))
    (package-install p)))

;;; ------------------------------
;;; General
;;; ------------------------------

;; silence gnutls warnings
(setq gnutls-min-prime-bits nil
      gnutls-verify-error nil)

;; Bump threshold to avoid constant garbage collection
(setq gc-cons-threshold 20000000
      message-log-max 16384)

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
(when (or (executable-find "aspell")
          (executable-find "ispell"))
  ;; enable flyspell for most text-based buffers
  (defun zmg/disable-flyspell-hook ()
    "Disable flyspell mode."
    (flyspell-mode -1))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'change-log-mode-hook 'zmg/disable-flyspell)
  (add-hook 'log-edit-mode-hook 'zmg/disable-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-issue-message-flag nil))

(defun zmg/text-mode-hook ()
  "Default settings for Text."
  (set-fill-column 80))

;; Hooks
(add-hook 'text-mode-hook 'zmg/text-mode-hook)
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

(scroll-bar-mode -1)
(tool-bar-mode -1)
(mouse-wheel-mode t)
(menu-bar-mode t)

;; ;; Set Default font if present
(when (find-font (font-spec :name "gohufont-10"))
  (add-to-list 'default-frame-alist '(font . "gohufont-10" ))
  (set-face-attribute 'default nil :font "gohufont-10"))

;; (setq default-frame-alist '((font-backend . "xft")
;;                             (font . "gohufont-10")
;;                             (left-fringe . -1)
;;                             (right-fringe . -1)
;;                             (fullscreen . 1)
;;                             (menu-bar-lines . 0)
;;                             (tool-bar-lines . 0)))

;; Maximize first frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Setup clipboard options if running in X
(when (display-graphic-p)
  (setq x-select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; disable dialog boxes
(setq use-file-dialog nil
      use-dialog-box nil)

;; the modeline
(line-number-mode t)
(column-number-mode t)
(display-time-mode 1)
(sml/setup)
(sml/apply-theme 'powerline)

;; show file size
(size-indication-mode t)

;; set theme
(load-theme 'monokai)

;; Handle screen drawing before input
(setq redisplay-dont-pause t)

;; show buffer name in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
		 "Emacs: %b"))))

;;; ------------------------------
;;; Calendar and diary settings
;;; ------------------------------

(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")

(setq calendar-week-start-day 1
      calendar-time-zone 120
      calendar-latitude 60.2
      calendar-longitude 25.0
      calendar-mark-holidays-flag t
      calendar-view-diary-initially-flag t
      calendar-date-style 'european
      calendar-mark-diary-entries-flag t
      calendar-day-name-array
      ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
       "torstai" "perjantai" "lauantai"]
      calendar-month-name-array
      ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
       "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
       "lokakuu" "marraskuu" "joulukuu"]
      diary-show-holidays-flag t
      diary-file (concat user-emacs-directory "diary"))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format nil
      display-time-use-mail-icon t)

(setq diary-display-function 'diary-fancy-display
      diary-number-of-entries 7)

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;;; ------------------------------
;;; Session
;;; ------------------------------

(setq recentf-save-file (concat user-emacs-directory "recentf")
      recentf-max-saved-items 50)
(recentf-mode t)
(when (fboundp 'ido-completing-read)
  (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))

(setq bookmark-default-file (concat-path user-emacs-directory "emacs.bmk")
      bookmark-save-flag 1)

(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat-path user-emacs-directory "savehist"))
(savehist-mode t)

(setq abbrev-file-name (concat-path user-emacs-directory "abbrev_defs")
      save-abbrevs t)

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook 'write-abbrev-file)

(setq backup-directory-alist (list `("." . ,(concat-path user-emacs-directory "backups")))
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

(setq shell-command-switch "-c"
      explicit-sh-args '("-login" "-i"))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-scroll-show-maximum-output t
      comint-completion-autolist t
      comint-input-ignoredups t
      comint-completion-addsuffix t
      comint-prompt-read-only t)

(with-eval-after-load 'eshell
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
        eshell-save-history-on-exit t
        eshell-scroll-show-maximum-output t
        eshell-scroll-to-bottom-on-output t)

  (require 'eshell)
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t))

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

(defun zmg/org-mode-hook ()
  "Default Org-mode settings."
  (setq org-directory (concat-path user-emacs-directory "/org")
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-agenda-files (list org-directory)
        org-agenda-include-all-todo t ;; deprecated, find better way
        org-agenda-include-diary t
        org-agenda-todo-ignore-with-date t
        org-default-notes-file (concat-path org-directory "/notes.org")
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
                            (sequence "PENDING(p@/!)" "|"))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-mode-hook 'zmg/org-mode-hook)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defun zmg/org-capture-mode-hook ()
  "Default Org-capture mode settings."
  (setq org-capture-templates
        '(("m" "Meeting" entry (file (concat org-directory "/meetings.org"))
           "* TODO %?\t:work:meeting:\n  %i\n  %a")
          ("a" "Task" entry (file (concat org-directory "/work.org"))
           "* TODO %?\t:work:task:\n  %i\n  %a")
          ("f" "Defect" entry (file (concat org-directory "/work.org"))
           "* TODO %?\t:work:defect:\n  %i\n  %a")
          ("E" "Enhancement" entry (file (concat org-directory "/work.org"))
           "* TODO %?\t:work:enchancement:\n  %i\n  %a")
          ("u" "System update" entry (file (concat org-directory "/work.org"))
           "* TODO %?\t:work:update:\n  %i\n  %a")
          ("p" "Project" entry (file (concat org-directory "/work.org"))
           "* TODO %?\t:project:\n  %i\n  %a")
          ("s" "Study" entry (file (concat org-directory "/work.org"))
           "* TODO %?\t:work:study:\n  %i\n  %a")
          ("t" "TODO entry" entry (file (concat org-directory "/gtd.org"))
           "* TODO %?\t:misc:\n  %i\n  %a"))))

(add-hook 'org-capture-mode-hook 'zmg/org-capture-mode-hook)
(add-hook 'org-capture-mode-hook 'zmg/org-mode-hook)

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

(defun zmg/ibuffer-mode-hook ()
  "Handle Ibuffer settings."
  (local-set-key (kbd "C-x C-f") 'ido-find-file)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(add-hook 'ibuffer-mode-hook 'zmg/ibuffer-mode-hook)
(defalias 'list-buffers 'ibuffer)

;;(setq confirm-nonexistent-file-or-buffer nil)

;; Don't prompt if killing buffer with process attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; -----------------------------
;;; ERC
;;; ------------------------------
(with-eval-after-load 'erc
  (setq erc-modules (append erc-modules '(services notify spelling log)))
  (erc-update-modules)

  (setq erc-prompt-for-password nil
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit nil
        erc-kill-server-buffer-on-quit t
        erc-auto-query 'window-noselect
        erc-keywords '("zmyrgel"))

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

(setq user-mail-address "timo.myyra@wickedbsd.net"
      user-full-name "Timo Myyrä")

;; ;; smtp mail setting; these are the same that `gnus' uses.
(setq message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server "mail.wickedbsd.net"
      smtpmail-smtp-server         "mail.wickedbsd.net"
      smtpmail-local-domain        "wickedbsd.net"
      smtpmail-smtp-service        587
      smtpmail-stream-type         'ssl)

;; gnus
(setq gnus-select-method '(nntp "news.gmane.org")
      mm-w3m-safe-url-regexp nil ;; consider all ulrs safe
      mm-inline-text-html-with-images t
      mm-inline-large-images 'resize
      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-treat-hide-citation t
      gnus-cited-lines-visible '(0 . 5)
      gnus-always-read-dribble-file t)

;; Set renderer for HTML
(setq mm-text-html-renderer
      (cond ((and (locate-library "w3m") (executable-find "w3m")) 'w3m)
            ((fboundp 'libxml-parse-html-region) 'shr)
            (t nil)))

(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-stream ssl))
        (nnimap "wickedbsd"
                (nnimap-address "mail.wickedbsd.net")
                (nnimap-stream ssl))))

;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(setq compilation-save-buffers-predicate nil
      compilation-ask-about-save nil
      compilation-window-height 12
      gdb-many-windows t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-completion-system 'grizzl
      flycheck-phpcs-standard "Zend")

(setq diff-switches '("-u"))

;; go-mode company-go go-eldoc go-projectile go-snippets
;; go-mode (setq gofmt-command "goimports")
;; set company popup delay in seconds (setq company-idle-delay 0)
(defun zmg/go-mode-hook ()
  "Options for Go language."
  (local-set-key (kbd "C-c m") 'gofmt)
  (local-set-key (kbd "M-.") 'godef-jump)
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'zmg/go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'company-mode)

;; project management
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; add autopairing
(require 'smartparens-config)
(smartparens-global-mode t)

;;; CC-mode styles

(with-eval-after-load 'cc-mode

  (define-key c-mode-map (kbd "C-c m") 'man-follow)
  (define-key c-mode-map (kbd "C-c C-c") 'mode-compile)
  (define-key c-mode-map (kbd "C-c C-d") 'gdb)
  (define-key c-mode-map (kbd "RET") 'c-context-line-break)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c-mode-map (kbd "C-M-i") 'semantic-ia-complete-symbol)

  (defun zmg/c-mode-common ()
    "Programming options shared for C-like languages."
    (setq company-backends (delete 'company-semantic company-backends))
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                   1 font-lock-warning-face prepend)))
    (setq which-func-unknown "TOP LEVEL"
          compilation-scroll-output 'first-error
          compilation-read-command nil
          c-hungry-delete-key t))

  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook 'which-func-mode)
  (add-hook 'c-mode-common-hook 'cwarn-mode)
  (add-hook 'c-mode-common-hook 'subword-mode)
  (add-hook 'c-mode-common-hook 'smartparens-mode)

  (defun zmg/c-mode ()
    "My C programming options."
    (c-set-style "bsd")
    (setq whitespace-line-column 80
          whitespace-style '(face lines-tail)))

  (defun zmg/c++-mode ()
    "My C++ programming options."
    (setq fill-column 100)
    (c-set-style "stroustrup")
    (setq whitespace-line-column 100
          whitespace-style '(face lines-tail)))

  (add-hook 'c-mode-common-hook 'zmg/c-mode-common)
  (add-hook 'c-mode-hook 'zmg/c-mode)
  (add-hook 'c++-mode-hook 'zmg/c++-mode))

(when (fboundp 'cperl-mode)
  (defalias 'perl-mode 'cperl-mode))

(defun zmg/cperl-mode-hook ()
  "Default CPerl settings."
  (setq cperl-fontlock t
        cperl-electric-lbrace-space t
        cperl-electric-parens nil
        cperl-electric-linefeed t
        cperl-electric-keywords t
        cperl-info-on-command-no-prompt t
        cperl-clobber-lisp-bindings t
        cperl-lazy-help-time 5
        cperl-indent-level 4
        cperl-auto-newline t
        cperl-invalid-face 'default)
  (local-set-key (kbd "C-h f") 'cperl-perldoc))

(add-hook 'cperl-mode-hook 'zmg/cperl-mode-hook)

(add-to-list 'auto-mode-alist
             '("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))

(defun zmg/php-mode-hook ()
  (setq php-manual-url "http://www.php.net/manual/en"
        php-search-url "http://www.php.net/"
        whitespace-line-column 80
        whitespace-style '(face lines-tail))
  (php-enable-symfony2-coding-style))

(add-hook 'php-mode-hook 'zmg/php-mode-hook)

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ap[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun zmg/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'zmg/web-mode-hook)

;;; Ruby settings
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"  . ruby-mode))

(defun zmg/ruby-mode-hook ()
  (setq ruby-deep-arglist t)
  (setq ruby-deep-indent-paren nil)
  (setq c-tab-always-indent nil))

(add-hook 'ruby-mode-hook 'zmg/ruby-mode-hook)

(with-eval-after-load 'ruby-mode
  (rvm-use-default))

(add-to-list 'auto-mode-alist '("\\.yml$\\|\\.yaml$" . yaml-mode))

(defun zmg/css-mode-hook ()
  (setq css-indent-level 2)
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'zmg/css-mode-hook)

;;; Lisp settings
(autoload 'paredit-mode "paredit" "Paredit-mode" nil)

(defun zmg/shared-lisp-hook ()
  (paredit-mode t)
  (rainbow-delimiters-mode t)
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'zmg/shared-lisp-hook)
(add-hook 'lisp-mode-hook 'zmg/shared-lisp-hook)
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'scheme-mode-hook 'zmg/shared-lisp-hook)
(add-hook 'racket-mode-hook 'zmg/shared-lisp-hook)
(add-hook 'racket-repl-mode-hook 'zmg/shared-lisp-hook)

;; clojure
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode-hook 'zmg/shared-lisp-hook)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; ;; Scheme settings

(eval-after-load 'scheme
  '(define-key scheme-mode-map (kbd "<tab>") 'scheme-complete-or-indent))

(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)
            (define-key scheme-mode-map (kbd "") 'scheme-inj)))

;; geiser config
(with-eval-after-load 'geiser
  (setq geiser-guile-binary
        (if (eq system-type 'berkeley-unix)
            "guile2"
          "guile")
        geiser-default-implementation 'guile
        geiser-mode-start-repl-p t)

  (add-hook 'scheme-mode-hook 'geiser-smart-tab-mode))

;; TODO: add snippet here
;; (require 'autoinsert)
;; (add-hook 'find-file-hooks 'auto-insert)

;; (setq auto-insert-alist
;;       '(("\\.scm" .
;;          (insert "#!/bin/sh\n#| -*- scheme -*-\nexec csi -s $0 \"$@\"\n|#\n"))))

;;; ------------------------------
;;; Completion
;;; ------------------------------
(setq company-idle-delay 0.3)
(add-hook 'after-init-hook 'global-company-mode)

(defun zmg/company-mode-hook ()
  (define-key company-active-map (kbd "C-p") 'company-select-next)
  (define-key company-active-map (kbd "C-n") 'company-select-previous))

(add-hook 'company-mode-hook 'zmg/company-mode-hook)
(global-set-key (kbd "M-/") 'company-complete)

(icomplete-mode t)
(setq icomplete-prospects-height 2
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      tab-always-indent 'complete
      completion-styles (append completion-styles '(initials)))

;;; Ido settings
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
      ido-use-url-at-point t)

(setq ido-ignore-extensions t)

(ido-mode 1)
(global-set-key (kbd "C-c i") 'idomenu)

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
(defun zmg/dired-load-hook ()
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

(add-hook 'dired-load-hook 'zmg/dired-mode-hook)

(defun zmg/dired-mode-hook ()
  (setq truncate-lines t))

;;(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'zmg/dired-mode-hook)

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

(defun quit-prompt ()
  "Prompts before exiting Emacs."
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
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'rgrep)
(global-set-key (kbd "<f11>") 'gnus)
(global-set-key (kbd "<f12>") 'bookmark-bmenu-list)

(setq browse-url-browser-function
      (cond ((and (locate-library "w3m") (executable-find "w3m")) 'w3m-browse-url)
            (t 'eww-browse-url)))

;;; ------------------------------
;;; External packages
;;; ------------------------------

(global-set-key (kbd "C-x v /") 'magit-status)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(yas-global-mode 1)

;; Web Browsing
(with-eval-after-load 'w3m-search
  (add-to-list 'w3m-search-engine-alist '("duckduckgo" "https://duckduckgo.com/?q=%s"))
  (add-to-list 'w3m-search-engine-alist '("fi.wikipedia" "http://fi.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8))
  (setq w3m-search-default-engine "duckduckgo"))

(with-eval-after-load 'w3m
  (define-key w3m-mode-map "z" 'w3m-previous-buffer)
  (define-key w3m-mode-map "x" 'w3m-next-buffer))

(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t
      w3m-use-form t
      w3m-default-display-inline-images t
      w3m-use-cookies t
      w3m-use-tab nil
      url-keep-history t
      w3m-profile-directory user-emacs-directory
      w3m-default-save-directory "~/Downloads"
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-home-page "http://www.openbsd.org")

(defun zmg/w3m-rename-buffer (url)
  "Base buffer name on title URL."
  (let* ((size 32)
         (title w3m-current-title)
         (name (truncate-string-to-width
                (replace-regexp-in-string " " "_" title)
                size)))
    (rename-buffer name t)))

(add-hook 'w3m-display-hook 'zmg/w3m-rename-buffer)

(defadvice w3m-modeline-title (around my-w3m-modeline-title)
  "Prevent original function from running; cleanup remnants."
  (setq w3m-modeline-separator ""
        w3m-modeline-title-string ""))
(ad-activate 'w3m-modeline-title)

(setq w3m-session-file (concat-path user-emacs-directory "w3m-session")
      w3m-session-save-always t
      w3m-session-load-always t
      w3m-session-show-titles t
      w3m-session-duplicate-tabs 'never)

;;; Auctex
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-insert-braces nil
      TeX-electric-escape t
      TeX-electric-macro t
      TeX-newline-function 'reindent-then-newline-and-indent)

(setq multi-term-program (case system-type
                           (gnu/linux "/bin/bash")
                           (windows-nt "C:\\bin\\cmd.exe")
                           (berkeley-unix "/bin/ksh")
                           (usg-unix-v "/bin/ksh")))
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

;; smex
(autoload 'smex "smex" "Smex" t)
(autoload 'smex-major-mode-commands "smex" "Smex" t)
(with-eval-after-load 'smex
  (smex-initialize))
(setq smex-save-file (concat-path user-emacs-directory "smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; quack
(setq quack-default-program "csi"
      quack-dir (concat-path user-emacs-directory "quack")
      quack-fontify-style nil
      quack-newline-behavior 'indent-newline-indent
      quack-pretty-lambda-p nil
      quack-remap-find-file-bindings-p nil
      quack-run-scheme-always-prompts-p nil
      quack-run-scheme-prompt-defaults-to-last-p t
      quack-smart-open-paren-p t
      quack-switch-to-scheme-method 'other-window)

;; slime
(with-eval-after-load 'slime

  (defun zmg/slime-mode-hook ()
    "Default Slime settings."
    (setq slime-description-autofocus t
          slime-repl-history-trim-whitespaces t
          slime-repl-wrap-history t
          slime-repl-history-file (concat user-emacs-directory "slime-history.eld")
          slime-repl-history-remove-duplicates t
          slime-ed-use-dedicated-frame t
          slime-kill-without-query-p t
          slime-startup-animation t
          slime-net-coding-system 'utf-8-unix))

  (add-hook 'slime-mode-hook 'zmg/slime-mode-hook)

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
  (cond ((file-directory-p "/usr/local/share/doc/clisp-hyperspec")
         (setq common-lisp-hyperspec-root "file:/usr/local/share/doc/clisp-hyperspec/"))
        ((file-directory-p "~/lisp/docs/HyperSpec")
         (setq common-lisp-hyperspec-root "file:~/lisp/docs/HyperSpec/"))
        (t (setq common-lisp-hyperspec-root
                 "http://www.lispworks.com/documentation/HyperSpec/")))

  (setq common-lisp-hyperspec-symbol-table
        (concat-path common-lisp-hyperspec-root "Data/Map_Sym.txt"))

  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'slime-repl-mode-hook 'paredit-mode)

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
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

;; Load optional local startup files
(add-extension (concat-path user-emacs-directory "init-local.el"))

(provide 'init)

;;; init.el ends here
