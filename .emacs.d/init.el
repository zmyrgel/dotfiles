;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;
;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;;
;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;; Time-stamp: <2011-08-08 15:00:28 (zmyrgel)>
;; URL: http://www.wickedbsd.net/dotfiles/
;; Compatibility: GNU Emacs 24.0.x (may work with earlier versions)
;;
;; ----------------------------------------------------------------------------
;; Todo:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install el-get if not already available
;;(url-retrieve
;; "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
;; (lambda (s)
;;   (end-of-buffer)
;;   (eval-print-last-sexp)))

;; Provide few defaults
(defconst emacs-dir (expand-file-name "~/.emacs.d"))
(defconst elisp-dir (concat emacs-dir "/elisp"))
(defconst dropbox-dir (expand-file-name "~/Dropbox"))
(setq custom-file (concat emacs-dir "/custom.el"))
(load custom-file 'noerror)

(when (string= system-type "berkeley-unix")
  (setenv "AUTOCONF_VERSION" "2.62"))

(when (file-exists-p (concat emacs-dir "/init-local.el"))
  (load (concat emacs-dir "/init-local.el")))

;; Testing
(add-to-list 'load-path elisp-dir)
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; XXX: Add info's to proper place
;; (add-to-list 'Info-default-directory-list
;;             (expand-file-name (concat emacs-root-dir "libs/scm/dvc/texinfo")) t)

(add-to-list 'load-path (concat emacs-dir "/el-get/el-get"))
(require 'el-get)
(add-to-list 'el-get-recipe-path (concat emacs-dir "/el-get/my-recipies"))

(setq el-get-sources
      '(el-get yasnippet autopair apel flim suomalainen-kalenteri mingus paredit geiser
	       (:name org-mode :after (lambda ()
                                        (add-to-list 'load-path "~/.emacs.d/el-get/org-mode")
                                        (setq org-directory (concat dropbox-dir "/org")
                                              org-agenda-files (list org-directory)
                                              org-agenda-include-diary t
                                              org-default-notes-file (concat org-directory "/notes.org")
                                              org-completion-use-ido t
                                              org-outline-path-complete-in-steps nil
                                              org-insert-mode-line-in-empty-file t
                                              org-mobile-inbox-for-pull (concat org-directory "/flagged.org")
                                              org-mobile-directory (concat dropbox-dir "/MobileOrg")
                                              org-enforce-todo-checkbox-dependencies t
                                              org-enforce-todo-dependencies t
                                              org-log-done 'note
                                              org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d!)")
                                                                  (sequence "|" "CANCELED(c@/!)")
                                                                  (sequence "|" "STALLED(s@/!)")
                                                                  (sequence "PENDING(p@/!)" "|")))

                                        (defun org-summary-todo (n-done n-not-done)
                                          "Switch entry to DONE when all subentries are done, to TODO otherwise."
                                          (let (org-log-done org-log-states)   ; turn off logging
                                            (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
                                        (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

                                        (setq org-capture-templates
                                              '(("m" "Meeting" entry (file+headline (concat org-directory "/meetings.org") "Tapaamiset")
                                                 "* TODO %?\n  %i\n  %a")
                                                ("a" "Task" entry (file+headline (concat org-directory "/work.org") "Työt")
                                                 "* TODO %?\n  %i\n  %a")
                                                ("f" "Defect" entry (file+headline (concat org-directory "/work.org") "Viat")
                                                 "* TODO %?\n  %i\n  %a")
                                                ("e" "Enhancement" entry (file+headline (concat org-directory "/work.org") "Parantelut")
                                                 "* TODO %?\n  %i\n  %a")
                                                ("u" "System update" entry (file+headline (concat org-directory "/work.org") "Järjestelmän päivitykset")
                                                 "* TODO %?\n  %i\n  %a")
                                                ("t" "TODO entry" entry (file+headline (concat org-directory "/gtd.org") "Sekalaiset")
                                                 "* TODO %?\n  %i\n  %a")))

					(global-set-key (kbd "C-c l") 'org-store-link)
					(global-set-key (kbd "C-c c") 'org-capture)
					(global-set-key (kbd "C-c a") 'org-agenda)

					(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

					(setq org-publish-project-alist
					      '(("personal"
						 :base-directory "~/web/"
						 :publishing-directory "~/public_html"
						 :section-numbers nil
						 :table-of-contents nil
						 :style "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>")
                                                ("work"
                                                 :base-directory "~/Dropbox/org"
                                                 :exclude "contacts\\.org\\|gtd\\.org\\|index\\.org\\|notes\\.org"
                                                 :section-numbers nil
                                                 :table-of-contents t
                                                 :style "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>")
                                                ))
                                        (add-hook 'message-mode-hook 'turn-on-orgstruct)
                                        (add-hook 'message-mode-hook 'turn-on-orgstruct++)
                                        (add-hook 'message-mode-hook 'turn-on-orgtbl)

                                        ;; Contacts / replace with BBDB
                                        (require 'org-contacts)
                                        (setq org-contacts-files (list (concat org-directory "/contacts.org")))
                                        ))
	       (:name slime :after (lambda ()
				     (setq slime-description-autofocus t
                                           slime-repl-history-trim-whitespaces t
                                           slime-repl-wrap-history t
                                           slime-repl-history-file (concat emacs-dir "/slime-history.eld")
                                           slime-repl-history-remove-duplicates t
                                           slime-ed-use-dedicated-frame t
                                           slime-kill-without-query-p t
                                           slime-startup-animation t
                                           slime-net-coding-system 'utf-8-unix
                                           ;;common-lisp-hyperspec-root "file:/home/zmyrgel/lisp/docs/HyperSpec/"
                                           ;;common-lisp-hyperspec-symbol-table
                                           ;; (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")
                                           slime-lisp-implementations
					   '((sbcl  ("sbcl"))
					     (clisp ("clisp" "-K full -ansi"))))

                                     ;; conflicts with clojure swank in newer Slime CVS (later than 2009-10-01)
				     (setq slime-use-autodoc-mode nil)

				     (require 'slime)
				     (slime-setup
				      '(slime-repl
					slime-fancy
					slime-asdf
					slime-tramp))

                                     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

                                     (global-set-key (kbd "C-c s") 'slime-selector)
				     (def-slime-selector-method ?l
				       "most recently visited lisp-mode buffer."
				       (slime-recently-visited-buffer 'lisp-mode))
                                     (def-slime-selector-method ?c
                                       "most recently visited scheme-mode buffer."
				       (slime-recently-visited-buffer 'scheme-mode))
				     (def-slime-selector-method ?j
				       "most recently visited clojure-mode buffer."
				       (slime-recently-visited-buffer 'clojure-mode))))
	       (:name magit :after (lambda ()
				     (global-set-key (kbd "C-x v /") 'magit-status)))
               (:name emacs-w3m :after (lambda ()
                                         (require 'w3m-load)
                                         (require 'w3m)
                                         (require 'mime-w3m)
                                         (require 'w3m-session)

                                         (setq w3m-session-file (concat emacs-dir "/w3m-session")
                                               w3m-session-save-always t
                                               w3m-session-load-always t
                                               w3m-session-show-titles t
                                               w3m-session-duplicate-tabs 'never)

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

                                         (defun my-w3m-mode-init ()
                                           (define-key w3m-mode-map "q" 'w3m-previous-buffer)
                                           (define-key w3m-mode-map "w" 'w3m-next-buffer)
                                           (define-key w3m-mode-map "x" 'w3m-close-window))
                                         (add-hook 'w3m-mode-hook 'my-w3m-mode-init)

                                         (defun my-w3m-rename-buffer (url)
                                           "base buffer name on title"
                                           (let* ((size 32)
                                                  (title w3m-current-title)
                                                  (name (truncate-string-to-width
                                                         (replace-regexp-in-string " " "_" title)
                                                         size)))
                                             (rename-buffer name t)))

                                         (add-hook 'w3m-display-hook 'my-w3m-rename-buffer)

                                         (defadvice w3m-modeline-title (around my-w3m-modeline-title)
                                           "prevent original function from running; cleanup remnants"
                                           (setq w3m-modeline-separator ""
                                                 w3m-modeline-title-string ""))
                                         (ad-activate 'w3m-modeline-title)))
	       (:name auctex :after (lambda ()
                                      (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
				      (setq TeX-auto-save t
					    TeX-parse-self t
                                            TeX-insert-braces nil
                                            TeX-electric-escape t
                                            TeX-electric-macro t
                                            TeX-newline-function 'reindent-then-newline-and-indent)))
	       (:name multi-term :after (lambda ()
                                          ;;; Add better check here, windows PC?
                                          (if (string= system-type "gnu/linux")
                                              (setq multi-term-program "/bin/bash")
                                            (setq multi-term-program "/bin/ksh"))
					  (add-hook 'term-mode-hook (lambda () (setq autopair-dont-activate t)))
					  (global-set-key (kbd "C-c t") 'multi-term-next)
					  (global-set-key (kbd "C-c T") 'multi-term)))
	       (:name smex :after (lambda ()
				    (global-set-key (kbd "M-x") 'smex)
				    (global-set-key (kbd "C-x C-m") 'smex)
				    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
				    (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
				    (setq smex-save-file (concat emacs-dir "/smex-items"))))
	       (:name clojure-mode :after (lambda ()
					    (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
					    (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
					    (add-hook 'clojure-mode-hook 'my-lisp-hook)))
	       (:name auto-complete :after (lambda ()
					     (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)))
	       (:name undo-tree :after (lambda ()
					 (global-undo-tree-mode)))
               (:name quack :after (lambda ()
                                     (setq quack-default-program "csi"
                                           quack-dir "~/.emacs.d/quack"
                                           quack-fontify-style nil
                                           quack-newline-behavior 'indent-newline-indent
                                           quack-pretty-lambda-p nil
                                           quack-remap-find-file-bindings-p nil
                                           quack-run-scheme-always-prompts-p nil
                                           quack-run-scheme-prompt-defaults-to-last-p t
                                           quack-smart-open-paren-p t
                                           quack-switch-to-scheme-method 'other-window)))
               (:name nognus :after (lambda ()
                                      (require 'gnus-load)
                                      (setq gnus-select-method '(nntp "news.gmane.org")
                                            mm-inline-text-html-with-images t
                                            mm-discouraged-alternatives '("text/html" "text/richtext"))

                                      ;;(setq gnus-summary-line-format "%U%R│%B%(%s%80=%) │ %f %110=│ %6&user-date;\n")
                                      (setq gnus-treat-hide-citation t
                                            gnus-cited-lines-visible '(0 . 5))

                                      ;; XXX: test
                                      ;; (defun my-save-all-jpeg-parts (handle)
                                      ;;   (when (equal (car (mm-handle-type handle)) "image/jpeg")
                                      ;;     (with-temp-buffer
                                      ;;       (insert (mm-get-part handle))
                                      ;;       (write-region (point-min) (point-max)
                                      ;;                     (read-file-name "Save jpeg to: ")))))
                                      ;; (setq gnus-article-mime-part-function
                                      ;;       'my-save-all-jpeg-parts)


                                      ;; check for new messages every 10 mins
                                      (require 'gnus-demon)
                                      (gnus-demon-add-handler 'gnus-demon-scan-news 10 t)

                                      ;; use w3m to render HTML messages
                                      (if (featurep 'w3m)
                                          (setq mm-text-html-renderer 'w3m)
                                        (setq mm-text-html-renderer 'shr))
                                      ))
               ))

;; Extra settings to handle older emacsen

;;  Use color-theme package on older than 24.1
 (if (< emacs-major-version 24)
     (progn
       (add-to-list 'el-get-sources 'color-theme)
       (add-to-list 'el-get-sources 'color-theme-twilight))
   (setq custom-enabled-themes '(wombat))
   (load-theme 'wombat))

(setq default-frame-alist '((font-backend . "xft")
                            (font . "Dina-10")
                            (left-fringe . -1)
                            (right-fringe . -1)
                            (fullscreen . fullboth)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

;; Use external cedet on older than 23.2
(if (or (< emacs-major-version 23)
        (and (= emacs-major-version 23)
             (= emacs-minor-version 1)))
    (add-to-list 'el-get-sources 'cedet)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          global-semantic-idle-summary-mode
          global-semantic-mru-bookmark-mode
          global-semantic-stickyfunc-mode))
  (semantic-mode 1)
  (global-ede-mode 1))

(el-get)

;; bytecompile... XXX: does all .el-files, fixit
(defun auto-recompile-emacs-file ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.emacs" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

;(add-hook 'after-save-hook 'auto-recompile-emacs-file)

;; ------------------------------
;; General
;; ------------------------------

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
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Misc options
(auto-compression-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq set-mark-command-repeat-pop t)

;; mouse options
(setq mouse-yank-at-point t)
(mouse-wheel-mode t)

;; Encoding
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")

;; spelling
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; Hooks
(add-hook 'text-mode-hook
	  (lambda()
	    (set-fill-column 80)
	    (auto-fill-mode t)))

(add-hook 'before-save-hook
          (lambda ()
            (time-stamp)
            (delete-trailing-whitespace)))

(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'kill-emacs-hook 'write-abbrev-file)
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

(scroll-bar-mode -1)

;; Setup clipboard options if running in X
(if (not window-system)
    (menu-bar-mode nil)
  (setq x-select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

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
      ;; new entries
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

(setq bookmark-default-file (concat emacs-dir "/emacks.bmk")
      bookmark-save-flag 1)

(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat emacs-dir "/savehist"))
(savehist-mode t)

(setq abbrev-file-name (concat emacs-dir "/abbrev_defs")
      abbrev-mode t
      save-abbrevs t)
(abbrev-mode 1)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
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

(setq shell-file-name "bash"
      shell-command-switch "-c"
      explicit-shell-file-name shell-file-name
      explicit-sh-args '("-login" "-i"))

(when (or (< emacs-major-version 23)
          (and (= emacs-major-version 23)
               (= emacs-minor-version 1)))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

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
;; Buffer management
;; ------------------------------

(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

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
                              (mode . lisp-mode)))
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

(when (require 'erc nil t)

  (setq erc-modules (append erc-modules '(services notify spelling log pcomplete)))
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
  (setq erc-truncate-buffer-on-save t))

;; ------------------------------
;; Programming settings
;; ------------------------------

(setq compilation-window-height 12
      gdb-many-windows t)

(require 'cc-mode nil t)

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
	       (c-subword-mode . 0)
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

(defun my-c-mode-common ()
  (interactive)
  (hs-minor-mode t)
  (autopair-mode)
  (which-function-mode t)
  (cwarn-mode 1)
  (subword-mode 1)
  (c-toggle-hungry-state 1)
  (semantic-mode 1)
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                 1 font-lock-warning-face prepend)))
  (setq which-func-unknown "TOP LEVEL"
        compilation-scroll-output 'first-error
        compilation-read-command nil
        c-hungry-delete-key t
        tab-width 8
        indent-tabs-mode t
        fill-column 80)

  (local-set-key (kbd "C-c m") 'man-follow)
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c C-d") 'gdb)
  (local-set-key (kbd "RET") 'c-context-line-break)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-M-i") 'semantic-ia-complete-symbol))

(defun my-c-mode ()
  (c-set-style "openbsd")
  (font-lock-add-keywords 'c-mode '(("^[^\n]\\{80\\}\\(.*\\)$"
                                     1 font-lock-warning-face prepend))))

(defun my-c++-mode ()
  (setq fill-column 100)
  (c-set-style "stroustrup")
  (local-set-key (kbd "C-c e") #'expand-member-functions)
  (font-lock-add-keywords 'c++-mode '(("^[^\n]\\{100\\}\\(.*\\)$"
                                       1 font-lock-warning-face prepend))))

(add-hook 'c-mode-common-hook 'my-c-mode-common)
(add-hook 'c-mode-hook 'my-c-mode)
(add-hook 'c++-mode-hook 'my-c++-mode)

(when (fboundp 'cperl-mode)
  (defalias 'perl-mode 'cperl-mode))
(add-hook 'cperl-mode-hook
          (lambda ()
            (when (require 'flymake nil t)
              (flymake-mode 1))
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
  (setq indent-tabs-mode nil
        fill-column 80
        tab-width 4)
  (c-set-offset 'arglist-intro '+)
  (c-set-style "java"))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(when (file-exists-p (concat elisp-dir "malabar"))
  (setq load-path (cons (concat elisp-dir "malabar/lisp") load-path))
  (require 'malabar-mode)
  (add-hook 'malabar-mode-hook 'my-java-mode-hook)
  (setq malabar-groovy-lib-dir
        (concat elisp-dir "malabar-1.5-SNAPSHOT/lib"))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
  (add-hook 'malabar-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'malabar-compile-file-silently
                        nil t))))

(defun my-lisp-hook ()
  "Shared lisp settings"
  (paredit-mode +1)
  (turn-on-eldoc-mode)
  (c-toggle-hungry-state 1)
  (font-lock-add-keywords nil '(("^[^\n]\\{80\\}\\(.*\\)$"
                                 1 font-lock-warning-face prepend)))
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\)"
                             1 font-lock-warning-face prepend))))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-lisp-hook)

(setq scheme-program-name "csi")
(add-to-list 'load-path "/var/lib/chicken/5/")
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
(add-hook 'scheme-mode-hook 'my-lisp-hook)
(add-hook 'scheme-mode-hook (lambda ()
                              (slime-mode t)))

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

(when (featurep 'ido)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-f") 'ido-find-file)))
  (ido-mode t)
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
        ido-use-url-at-point t))

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

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

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
  (cond ((y-or-n-p "Quit terminal? ")
         (save-buffers-kill-terminal))))

;; ------------------------------
;; Keybindings
;; ------------------------------

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-j") 'join-line)
(global-set-key (kbd "C-^") 'repeat)
(global-set-key (kbd "C-x C-c") 'quit-prompt)
