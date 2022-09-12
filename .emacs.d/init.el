;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2022-09-11 09:09:37 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 28.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - fix warnings on this init:
;;; -- flymake--handle-report: Can’t find state for flymake-eslint--checker in ‘flymake--state’
;;; - improve init speed, currently 7s.

;;; Code:

;; Make startup faster by reducing the frequency of gc
(setq gc-cons-threshold (* 50 1000 1000))

(defconst lisp-dir (locate-user-emacs-file "lisp"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(require 'package)

(add-to-list 'package-archives (cons "melpa"
                                    (format "http%s://melpa.org/packages/"
                                            (if (gnutls-available-p) "s" ""))))
(setq package-archive-priorities
      '(("gnu" . 2)
        ("nongnu" . 1)))

;; TODO: sync refresh?
(package-refresh-contents 'async)

(defmacro zmg/with-package (package &rest body)
  "Evaluate BODY after loading the given PACKAGE, installing it if needed."
  (declare (indent 1))
  `(progn
       (when (and (not (package-built-in-p ,package))
                  (not (package-installed-p ,package)))
         (package-install ,package))
       (if (not (require ,package nil 'noerror))
           (display-warning 'zmg/with-package
                            (format "Loading of package `%s' failed" ,package) :error)
         ,@body)))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(add-to-list 'load-path (locate-user-emacs-file "lisp") nil)

(require 'zmg-emacs-general)
(require 'zmg-emacs-text)
(require 'zmg-emacs-visual)
(require 'zmg-emacs-calendar)
(require 'zmg-emacs-session)
(require 'zmg-emacs-shell)
(require 'zmg-emacs-org)
(require 'zmg-emacs-email)
(require 'zmg-emacs-web)
(require 'zmg-emacs-completion)
(require 'zmg-emacs-files)
(require 'zmg-emacs-programming)

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

;; Restore old gc threshold value
(setq gc-cons-threshold 800000)

;; Load optional local startup files
(load (locate-user-emacs-file "init-local.el") t t)

;; Only start server mode for non-admin accounts
(unless (string-equal "root" (getenv "USER"))
  (when (and (fboundp 'server-running-p)
             (server-running-p))
    (server-start)))

(provide 'init)

;;; init.el ends here
