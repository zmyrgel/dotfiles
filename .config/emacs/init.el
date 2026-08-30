;;; init.el --- Emacs main init file -*- lexical-binding: t -*-

;; Copyright (c) 2009-2026 Timo Myyrä <timo.myyra@bittivirhe.fi>

;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;; URL: https://github.com/zmyrgel/dotfiles
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; My configuration file, split into separate files for easier
;; management.

;;; Code:

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

(setopt package-review-policy nil)

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

(defvar work-laptop-p (string-prefix-p "ws-1127." (system-name))
  "Utility variable to determine if we're running on work laptop or not.")

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
  (require 'init-web)
  (require 'init-text) ;; auctex, pdf-tools
  (require 'init-programming) ;; vc-got, magit, magit-gitflow
  (require 'init-webdev) ;; web-mode, flymake-eslint, prettier, ts-comint
  (require 'init-lisp) ;; sly, sly-repl-ansi-color, sly-asdf, sly-macrostep, sly-quicklisp, quack, clojure-mode, cider, geiser,
  (require 'init-ai) ;; agent-shell
  (require 'init-extras) ;; marginalia, emms, terraform-doc, terraform-mode, nov,x509-mode, plantuml,bibliothek, suomalainen-kalenteri, restclient, vcl-mode

  ;; Load optional local startup file
  (add-hook 'after-init-hook 'my/load-local-init t)

  ;; load custom settings
  (setopt custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror))

;; Finalizers

(defun my/log-start-gc ()
  "Notify how long the start up took."
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook 'my/log-start-gc)

;;; server
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
    (setq server-socket-dir run-dir)
    (setenv "EMACS_SOCKET_NAME" (concat run-dir "server")))
  (unless (server-running-p)
    (server-start)))

(provide 'init)

;;; init.el ends here
