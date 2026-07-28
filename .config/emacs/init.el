;;; init.el --- Emacs initialization file  -*- lexical-binding: t; -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2026-07-28 23:23:00 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 28.1 (may work with other versions)
;;;
;;; Commentary:

;;; Code:

;;; My configuration file, split into separate files for easier
;;; management.

(require 'package)

(add-to-list 'package-archives
             (cons "melpa"
                   (format "http%s://melpa.org/packages/"
                           (if (gnutls-available-p) "s" ""))))

(setopt package-archive-priorities
        '(("gnu" . 2)
          ("nongnu" . 1)))

(when (or (not (file-exists-p package-user-dir))
          (time-less-p (* 7 24 3600) ; 1 week
		       (time-since (file-attribute-modification-time
				    (file-attributes package-user-dir)))))
  (package-refresh-contents 'async))

(setq package-review-policy t
      package-review-diff-command '("git" "diff" "--no-index"
                                    "--color=never" "--diff-filter=d"))
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
  (string-prefix-p "ws-1127." (system-name)))

(defun my/load-local-init ()
  "Load the local init file."
  (load (expand-file-name (concat "init-"
                                  (car (split-string (system-name) "\\.")))
                          user-emacs-directory) t t))

;; Reduce startup time by ~0.2s reducing the frequency of garbage
;; collection during the initialization.
(let ((gc-cons-threshold (* 50 1000 1000)))
  (require 'init-general)
  (require 'init-session)
  (require 'init-shell)
  (require 'init-org)
  (require 'init-email)
  (require 'init-openbsd)
  (require 'init-completion)
  (require 'init-files)
  (require 'init-visual)
  (require 'init-calendar)
  (require 'init-text) ;; auctex, pdf-tools
  (require 'init-web) ;; elfeed
  (require 'init-programming) ;; vc-got, magit, magit-gitflow
  (require 'init-webdev) ;; web-mode,flymake-eslint, prettier,ts-comint
  (require 'init-lisp) ;; sly, sly-repl-ansi-color,sly-asdf,sly-macrostep,sly-quicklisp,quack,clojure-mode,cider,geiser,
  (require 'init-ai) ;; agent-shell
  (require 'init-extras) ;; marginalia, emms, terraform-doc, terraform-mode, nov,x509-mode,plantuml,vundo,bibliothek, easy-kill, suomalainen-kalenteri, restclient, vcl-mode

  ;; Load optional local startup file
  (add-hook 'after-init-hook 'my/load-local-init t)

  ;; load custom settings
  (setopt custom-file (locate-user-emacs-file "custom.el"))
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

;; Ensure we have server running for non-root users.
(require 'server)
(unless (string-equal "root" (getenv "USER"))
  ;; TODO: Global env here or command specific override?
  ;; XXX: set for specific commands
  ;;(setenv "EDITOR" (expand-file-name "emacsclient" invocation-directory))
  (let ((run-dir (expand-file-name "run" user-emacs-directory)))
    (unless (file-directory-p run-dir)
      (mkdir run-dir)
      (chmod run-dir #o700))
    (setq server-socket-dir run-dir))
  (unless (server-running-p)
    (server-start)))

(provide 'init)

;;; init.el ends here
