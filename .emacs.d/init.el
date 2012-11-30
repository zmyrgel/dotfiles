;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;
;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;; Time-stamp: <2012-11-30 07:56:09 (tmy)>
;; URL: http://github.com/zmyrgel/dotfiles
;; Compatibility: GNU Emacs 23.1 (may work with other versions)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;; - Autoloads for gnus
;; - indentation of programming modes
;; - ERC configuration, modules and such

;; Define few utilities
(defun concat-path (&rest parts)
  "Utility to concatenate path"
  (let ((result nil))
    (dolist (path parts (directory-file-name (expand-file-name result)))
      (if result
          (setq result (concat result "/" path))
        (setq result path)))))

(defun add-ext-dir (dir)
  "Adds extension dir to load-path if it exists."
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)))

(defun add-ext-file (file)
  "Adds extension dir to load-path if it exists."
  (when (file-exists-p file)
    (load file)))

;; Provide few defaults
(defconst emacs-dir (concat-path (getenv "HOME") ".emacs.d"))
(defconst elisp-dir (concat-path emacs-dir "elisp"))
(defconst elpa-dir (concat-path emacs-dir "elpa"))
(setq custom-file (concat-path emacs-dir "custom.el"))
(load custom-file 'noerror)

;; Add emacs dir to load path
(add-ext-dir emacs-dir)
(add-ext-dir elisp-dir)

;; Load optional startup files
(add-ext-file (concat-path emacs-dir (format "init-%d.el" emacs-major-version)))
(add-ext-file (concat-path emacs-dir (format "init-%s.el" (symbol-name window-system))))
(add-ext-file (concat-path emacs-dir "init-external.el"))
(add-ext-file (concat-path emacs-dir "init-local.el"))

;; ------------------------------
;; General
;; ------------------------------

(setq default-frame-alist '((font-backend . "xft")
                            (font . "terminus-10")
                            (left-fringe . -1)
                            (right-fringe . -1)
                            (fullscreen . 1)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

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

;; Add Spell-check for select modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Hooks
(add-hook 'text-mode-hook
	  (lambda()
	    (set-fill-column 80)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; enable disabled features
(put 'narrow-to-region 'disabled nil)

;; ------------------------------
;; Visual settings
;; ------------------------------

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(show-paren-mode t)
(setq visible-bell 1)
(blink-cursor-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode nil))

(when (fboundp 'mouse-wheel-mode)
  (mouse-wheel-mode t))

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
(line-number-mode t)
(column-number-mode t)
(display-time-mode 1)
(setq window-min-height 3)

;; show file size
(when (fboundp size-indication-mode)
  (size-indication-mode t))

;; ------------------------------
;; Calendar and diary settings
;; ------------------------------

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

;; ------------------------------
;; Session
;; ------------------------------

(setq bookmark-default-file (concat-path emacs-dir "emacs.bmk")
      bookmark-save-flag 1)

(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat-path emacs-dir "savehist"))
(savehist-mode t)

(setq abbrev-file-name (concat-path emacs-dir "abbrev_defs")
      abbrev-mode t
      save-abbrevs t)
(abbrev-mode 1)
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

;; ------------------------------
;; Shell settings
;; ------------------------------

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

;; ------------------------------
;; Org-mode
;; ------------------------------

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

;; ------------------------------
;; Buffer management
;; ------------------------------

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'ibuffer)
  (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("ERC" (mode . erc-mode))
           ("Programming" (or (mode . sh-mode)
                              (mode . scheme-mode)
                              (mode . java-mode)
                              (mode . malabar-mode)
                              (mode . jde-mode)
                              (mode . cperl-mode)
                              (mode . sepia-mode)
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
           ("W3M"  (mode . w3m-mode))
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
              (ibuffer-switch-to-saved-filter-groups "default")))

  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; ------------------------------
;; ERC
;; ------------------------------

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
     (defvar erc-insert-post-hook)
     (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
     (setq erc-truncate-buffer-on-save t)))

;; ------------------------------
;; Programming settings
;; ------------------------------

(setq compilation-window-height 12
      gdb-many-windows t)

;; Use CEDET
(when (or (> emacs-major-version 23)
          (and (= emacs-major-version 23)
               (>= emacs-minor-version 2)))
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          global-semantic-idle-summary-mode
          global-semantic-mru-bookmark-mode
          global-semantic-stickyfunc-mode))
  (semantic-mode 0)
  (global-ede-mode 1))

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

(c-add-style "php" '("bsd"
                     (c-subword-mode . 1)
                     (c-basic-offset . 2)
                     (fill-column . 80)
                     (tab-width . 2)
                     (indent-tabs-mode . nil)))

(c-add-style "perl" '("bsd"
                      (c-subword-mode . 1)
                      (c-basic-offset . 2)
                      (fill-column . 80)
                      (tab-width . 4)
                      (indent-tabs-mode . nil)))

(setq c-default-style '((java-mode . "java")
                        (c-mode . "openbsd")
                        (c++-mode . "stroustrup")
                        (cperl-mode . "perl")
                        (php-mode . "php")))

;; C programming

(defun my-c-mode-common ()
  (interactive)
  (hs-minor-mode t)
  (when (>= emacs-major-version 24)
    (electric-pair-mode 1))
  (which-function-mode t)
  (cwarn-mode 1)
  (c-subword-mode 1)
  (c-toggle-hungry-state 1)
  (when (or (>= emacs-major-version 24)
            (and (= emacs-major-version 23)
                 (>= emacs-minor-version 2)))
    (semantic-mode 1))

  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                 1 font-lock-warning-face prepend)))
  (setq which-func-unknown "TOP LEVEL"
        compilation-scroll-output 'first-error
        compilation-read-command nil
        c-hungry-delete-key t)

  (when (featurep 'auto-complete)
    (setq ac-sources (append '(ac-source-semantic) ac-sources)))

  (local-set-key (kbd "C-c m") 'man-follow)
  (local-set-key (kbd "C-c C-c") 'compile)
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
  (local-set-key (kbd "C-c e") #'expand-member-functions)
  (setq whitespace-line-column 100
        whitespace-style '(face lines-tail)))

(add-hook 'c-mode-common-hook 'my-c-mode-common)
(add-hook 'c-mode-hook 'my-c-mode)
(add-hook 'c++-mode-hook 'my-c++-mode)

(when (fboundp 'cperl-mode)
  (defalias 'perl-mode 'cperl-mode))
(add-hook 'cperl-mode-hook
          (lambda ()
            ;;(c-set-style "perl")
            (flymake-mode 1)
            (when (>= emacs-major-version 24)
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

(defun my-java-mode-hook ()
  (c-set-style "java"))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(add-to-list 'auto-mode-alist
     	     '("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))

(defun my-php-mode-hook ()
  (setq php-manual-url "http://www.php.net/manual/en"
        php-search-url "http://www.php.net/"
        whitespace-line-column 80
        whitespace-style '(face lines-tail))
  (c-set-style "php"))
(add-hook 'php-mode-hook 'my-php-mode-hook)

;;; Lisp settings
(autoload 'paredit-mode "paredit" "Paredit-mode" nil)

(defun my-shared-lisp-hook ()
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'my-shared-lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'my-shared-lisp-hook)
(add-hook 'lisp-mode-hook 'paredit-mode)

;; Scheme settings
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
(add-hook 'scheme-mode-hook 'my-shared-lisp-hook)
(setq scheme-program-name "csi")
(add-to-list 'load-path "/var/lib/chicken/5/")

;; (require 'chicken-scheme)
;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)

;;; clojure
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook 'my-shared-lisp-hook)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; ------------------------------
;; Completion
;; ------------------------------

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
     (ido-mode 1)
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
           ido-use-url-at-point t)))

;; ------------------------------
;; Dired options
;; ------------------------------

(setq dired-isearch-filenames t
      dired-ls-F-marks-symlinks t)

;; Enhance dired mode
(add-hook 'dired-load-hook
          (lambda ()
            ;;(setq dired-x-hands-off-my-keys nil) ;; ido-find-file
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

;; ------------------------------
;; Functions
;; ------------------------------

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
(global-set-key (kbd "C-c R") 'rename-current-file-or-buffer)

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

;; ------------------------------
;; Keybindings
;; ------------------------------

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-^") 'repeat)
(global-set-key (kbd "C-x C-c") 'quit-prompt)

(autoload 'magit-status "magit" "Magit mode" t)
(global-set-key (kbd "C-x v /") 'magit-status)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;; Oracle stuff
(add-ext-file (concat-path elisp-dir "sqlplus.el"))
(setq sql-oracle-program "/u01/app/oracle/product/11.2.0/xe/bin/sqlplus")

;; Browser
(setq browse-url-browser-function
      (cond ((and (locate-library "w3m") (executable-find "w3m")) 'w3m-browse-url)
            ((executable-find "firefox") 'browse-url-firefox)
            ((executable-find "lynx") 'lynx)
            (t nil)))
