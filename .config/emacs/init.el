;;; init.el --- Emacs initialization file  -*- lexical-binding: t; -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2024-10-06 08:14:36 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 28.1 (may work with other versions)
;;;
;;; Commentary:
;;; - fix warnings on this init:
;;; -- flymake--handle-report: Can’t find state for flymake-eslint--checker in ‘flymake--state’
;;; - improve init speed, currently 7s.
;;; - fix highlight of got diffs in gnus
;;; - gpttel package for AI
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
;;; -

;;; Code:

(defconst local-elisp-dir (locate-user-emacs-file "elisp"))

(require 'package)

(add-to-list 'package-archives (cons "melpa"
                                     (format "http%s://melpa.org/packages/"
                                             (if (gnutls-available-p) "s" ""))))
(setq package-archive-priorities
      '(("gnu" . 2)
        ("nongnu" . 1)))

;; TODO: sync refresh?
(package-refresh-contents 'async)

(defvar *packages-refreshed* nil)

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
                          (format "Loading of package `%s' failed" ,package) :error)
       ,@body)))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;; add local elisp files to load path
(unless (file-directory-p local-elisp-dir)
  (make-directory local-elisp-dir))
(add-to-list 'load-path local-elisp-dir nil)

;; append rest of emacs init files to load path
;; and load them
(add-to-list 'load-path (locate-user-emacs-file "init.d") t)

;; Reduce startup time by ~0.2s reducing the frequency of gc
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

  ;; Load optional local startup files
  (load (locate-user-emacs-file "init-local.el") t t)

  ;; load custom settings
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror))

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

;; Only start server mode for non-admin accounts
(unless (string-equal "root" (getenv "USER"))
  (when (and (fboundp 'server-running-p)
             (server-running-p))
    ;; TODO: Global env here or command specific override?
    (setenv "EDITOR" (expand-file-name "emacsclient" invocation-directory))
    (server-start)))

(provide 'init)

;;; init.el ends here
