;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;
;; Author: Timo Myyrä <timo.myyra@wickedbsd.net>
;; Created: 2009-05-12 12:35:44 (zmyrgel)>
;; Time-stamp: <2012-08-02 14:12:53 (tmy)>
;; URL: http://github.com/zmyrgel/dotfiles
;; Compatibility: GNU Emacs 24.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment:
;; Emacs24 version specific settings go here.
;; Package.el is not available in 23.x or older emacsen.


;; Use package.el
(require 'package)
(setq package-archives '(("GNU" . "http://elpa.gnu.org/packages/")
                         ("Marmalade" . "http://marmalade-repo.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")))
(package-initialize)

;; only for fresh install
(unless package-archive-contents
  (package-refresh-contents))

(defun ensure-installed (packages)
  "Ensures given packages are installed using package.el"
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

 ;; packages I use
(ensure-installed '(company
                    clojure-mode
                    magit
                    smex
                    undo-tree
                    paredit
                    yasnippet
                    ace-jump-mode
                    php-mode
                    suomalainen-kalenteri
                    quack
                    boxquote
                    pastels-on-dark-theme
                    hippie-expand-slime
                    mediawiki))
