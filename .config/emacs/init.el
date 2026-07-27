;;; init.el --- Emacs initialization file  -*- lexical-binding: t; -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2026-07-27 15:30:19 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 28.1 (may work with other versions)
;;;
;;; Commentary:
;;; - fix warnings on this init:
;;; -- flymake--handle-report: Can’t find state for flymake-eslint--checker in ‘flymake--state’
;;; - improve init speed, currently 7s.
;;; - fix highlight of got diffs in gnus
;;; - {C-h 4 s} to `help-find-source'
;;; - `kill-matching-buffers-no-ask'
;;; - recover-file has = to show diff
;;; - read-passwd has TAB to toggle password display
;;; - remote-file-name-inhibit-delete-by-moving-to-trash
;;; - remote-file-name-inhibit-auto-save
;;; - 'read-process-output-max' was increased to 65536, init uses 1048576
;;; - `replace-regexp-as-diff', 'multi-file-replace-regexp-as-diff', 'dired-do-replace-regexp-as-diff'
;;; - register-use-preview t
;;; - grep-use-headings t
;;; - { C-x ESC SEC }
;;; - indent-rigidly { C-x TAB }, indent-code-rigidly
;;; (setopt project-vc-extra-root-markers '(".projectile" ".git")) ;; .asd?
;;; project-specific history: use-package xref-project-history?
;;; ff-find-other-file / find-sibling-file
;;; BUG: rcirc has extra whitespace after JOIN entries
;;; - org set timer { C-c C-x ; }
;;; M-x copyright-update
;;; (setq register-use-preview t)

;; (add-hook 'text-mode (setq-local revert-buffer-function (run-tests)))
;;; - C-a to toggle between bol and back-to-indentation
;;; - fails when no notwork
;;; - FIX: kill-this-buffer must be bound to an event with parameters
;;; - fn how-many to get regexp match count
;;; - https://writequit.org/articles/working-with-logs-in-emacs.html#filtering-logs
;;; - add defun to send buffer contents to termbin.com $ cat my_file.log | nc termbin.com 9999
;;; - https://writequit.org/eos/eos.html
;;; - see: https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;;; -- at least pöhinää -> bs generator
;;; - https://github.com/radian-software/apheleia
;;; - use one file with outline mode?
;;; - https://www.reddit.com/r/emacs/comments/1fwqz07/any_tips_about_improving_spell_checking/
;;; - https://rants.org/
;;; - unbind save-buffers-kill-terminal

;;; kill-region-dwim 'emacs-word
;;; setopt for setq == only user settable

;;; Code:

(require 'package)

(add-to-list 'package-archives
             (cons "melpa"
                   (format "http%s://melpa.org/packages/"
                           (if (gnutls-available-p) "s" ""))))

(setq package-archive-priorities
      '(("gnu" . 2)
        ("nongnu" . 1)))

(package-refresh-contents 'async)

(defvar *packages-refreshed* nil)

(setq package-review-policy t
      package-review-diff-command '("git" "diff" "--no-index"
                                    "--color=never" "--diff-filter=d"))

;; TODO: check this
(setq tls-program
      ;; Defaults:
      ;; '("gnutls-cli --insecure -p %p %h"
      ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
      ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
      '(;;"gnutls-cli -p %p %h"
        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; commands:
;; package-update, package-update-all
;; package-recompile, package-recompile-all

;; (network-lookup-address-info "127.1" 'ipv4 'numeric)
;; recentf-open command
;; (setq garbage-collection-messages t)

(defun ensure-packages-present (maybe-packages)
  "Ensures given PACKAGES are installed."
  (dolist (package (if (atom maybe-packages)
                       (list maybe-packages)
                     maybe-packages))
    (unless (or (package-built-in-p package)
                (package-installed-p package))
      (package-install package))))

(defmacro zmg/with-package (package &rest body)
  "Evaluate BODY after loading the given PACKAGE, installing it if needed."
  (declare (indent 1))
  `(progn
     (unless (or (package-built-in-p ,package)
                 (package-installed-p ,package))
       (package-install ,package))
     (if (not (require ,package nil 'noerror))
         (display-warning 'zmg/with-package
                          (format "Loading of package `%s' failed" ,package)
                          :error)
       (with-eval-after-load ,package
         ,@body))))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;; append rest of emacs init files to load path
;; and load them
(add-to-list 'load-path
             (expand-file-name (locate-user-emacs-file "init.d")) t)

(defun is-work-laptop-p ()
  "Predicate to check if running on work laptop."
  (string= (car (split-string (system-name) "\\."))
           "ws-1127"))

(defun my/load-local-init ()
  "Load the local init file."
  (load (expand-file-name (concat "init-"
                                  (car (split-string (system-name) "\\.")))
                          user-emacs-directory) t t))

;; Reduce startup time by ~0.2s reducing the frequency of garbage
;; collection during the initialization.
(let ((gc-cons-threshold (* 50 1000 1000)))
  (require 'init-general)
  (require 'init-text)
  (require 'init-visual)
  (require 'init-calendar)
  (require 'init-session)
  (require 'init-shell)
  (require 'init-org)
  (require 'init-email)
  (require 'init-web)
  (require 'init-completion)
  (require 'init-files)
  (require 'init-programming)
  (require 'init-openbsd)
  (require 'init-ai)
  (require 'init-extras)

  ;; Load optional local startup file
  (add-hook 'after-init-hook 'my/load-local-init t)

  ;; load custom settings
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror))

;;; ------------------------------
;;; Finalizers
;;; ------------------------------
(defun my/log-start-gc ()
  "Notify how long the start up took."
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook 'my/log-start-gc)

;; Only start server mode for non-admin accounts
(unless (and (string-equal "root" (getenv "USER"))
             server-process)
  ;; TODO: Global env here or command specific override?
  (setenv "EDITOR" (expand-file-name "emacsclient" invocation-directory))
  (let ((run-dir (expand-file-name "run" user-emacs-directory)))
    (unless (file-directory-p run-dir)
      (mkdir run-dir)
      (chmod run-dir #o700))
    (setq server-socket-dir run-dir))
  (server-start))

(provide 'init)

;;; init.el ends here
