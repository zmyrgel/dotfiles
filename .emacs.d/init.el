;;; init.el --- Emacs lisp initialization file
;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;;
;;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;;; Time-stamp: <2022-09-10 09:34:04 (tmy)>
;;; URL: http://github.com/zmyrgel/dotfiles
;;; Compatibility: GNU Emacs 28.1 (may work with other versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; - fix warnings on this file
;;;
;;; Code:

;; Make startup faster by reducing the frequency of gc
(setq gc-cons-threshold (* 50 1000 1000))

(defconst lisp-dir (locate-user-emacs-file "lisp"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 2)
        ("nongnu" . 1)))

;; avoid re-initializing packages
(unless package--initialized (package-initialize))

;; load of use-package to handle rest of package initialization.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-hook-name-suffix nil)

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
