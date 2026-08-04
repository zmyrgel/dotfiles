;;; init-lisp.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Lisp Programming related settings

;;; Code:

;;; Lisp programming

;; TODO: disable whitespace-mode in *sly-description* buffer
(unless (package-installed-p 'sly)
  (package-vc-install
   '(sly :vc-backend Git
         :url "https://github.com/joaotavora/sly.git"
         :doc "doc/sly.texi")))

;; TODO: why not custom?
(setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
                                 (ecl ("ecl"))
                                 (clisp ("clisp" "-ansi"))
                                 (chicken ("csi"))
                                 (abcl ("abcl"))))
(setopt sly-default-lisp 'sbcl)

(when-let* ((local-hyperspec-path
             (seq-some (lambda (p)
                         (let ((full-path (expand-file-name p)))
                           (when (file-directory-p full-path)
                             full-path)))
                       '("/usr/local/share/doc/clisp-hyperspec/"
                         "/usr/share/doc/hyperspec/"
                         "~/src/lisp/HyperSpec/"))))
  (setq common-lisp-hyperspec-root (concat "file://" local-hyperspec-path))
  (setq common-lisp-hyperspepac-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")))

;; TODO: is this needed with package-vc-install?
;; compile and add sly info manual to emacs info-directory alist
;; (when-let* ((sly-doc-dirs (file-expand-wildcards (concat (locate-user-emacs-file "elpa") "/sly-*/doc"))))
;;   (let ((sly-doc-dir (car sly-doc-dirs)))
;;     (when (file-directory-p sly-doc-dir)
;;       ;; if no Info file found, generate it
;;       (unless (file-exists-p (concat sly-doc-dir "/sly.info"))
;;         (let ((default-directory sly-doc-dir))
;;           (if (eq system-type 'berkeley-unix)
;;               (async-shell-command "gmakeinfo sly.texi")
;;             (async-shell-command "make sly.info")))))
;;       (add-to-list 'Info-directory-list sly-doc-dir)))

;; if we have log4cl dist use it to set global logging
(when-let* ((log4cl-dirs
             (mapcar #'expand-file-name
                     (file-expand-wildcards "~/quicklisp/dists/quicklisp/software/log4cl-*-git"))))
  (add-to-list 'load-path (concat (car (last log4cl-dirs)) "/elisp"))
  (require 'log4sly nil t)
  (global-log4sly-mode 1))

(setopt sly-mrepl-prevent-duplicate-history 'move)
;;(setopt sly-command-switch-to-existing-lisp )

(ensure-packages-present 'sly-repl-ansi-color)
(ensure-packages-present 'sly-asdf)
(ensure-packages-present 'sly-macrostep)
(ensure-packages-present 'sly-quicklisp)

(sly-setup '(sly-fancy
             sly-quicklisp
             sly-macrostep
             sly-repl-ansi-color
             sly-asdf))

(ensure-packages-present '(clojure-mode cider))

;; (add-to-list 'major-mode-remap-alist '(clojure-mode--mode . ruby-ts-mode))
;; (add-to-list 'magic-mode-alist '("\\.clj$" . clojure-mode))

;; (setq cider-lein-parameters "repl :headless :host localhost")
;; (setq nrepl-hide-special-buffers t)

(ensure-packages-present 'geiser)
(when (eq system-type 'berkeley-unix)
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-guile-binary "guile3.0"))

(provide 'init-lisp)

;; init-lisp.el ends here
