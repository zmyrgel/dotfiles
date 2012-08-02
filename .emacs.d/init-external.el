;; ------------------------------
;; External packages
;; ------------------------------

;; Add info's to proper place
(dolist (d (list "emacs-w3m/doc" "bbdb/texinfo" "slime/info"))
  (let ((dir (expand-file-name (concat elisp-dir d))))
    (when (file-exists-p dir)
      (add-to-list 'Info-default-directory-list dir))))

(autoload 'yas/hippie-try-expand "yasnippet")

;; Additional libraries
(add-to-list 'load-path (concat elisp-dir (file-name-as-directory "apel")))
(add-to-list 'load-path (concat elisp-dir (file-name-as-directory "flim")))

;;;; w3m
(when (not (eq system-type 'windows-nt))
  (add-to-list 'load-path (concat elisp-dir (file-name-as-directory "emacs-w3m")))
  (autoload 'w3m "w3m" "W3M browser")
  (require 'w3m-load)
  (require 'w3m)
  (require 'mime-w3m)
  (require 'w3m-session)
  (require 'w3m-search)

  (setq w3m-session-file (concat emacs-dir "w3m-session")
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

  (defun my-w3m-hook ()
    (define-key w3m-mode-map "z" 'w3m-previous-buffer)
    (define-key w3m-mode-map "x" 'w3m-next-buffer)
    (add-to-list 'w3m-search-engine-alist '("duckduckgo" "https://duckduckgo.com/?q=%s"))
    (add-to-list 'w3m-search-engine-alist '("fi.wikipedia" "http://fi.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8))
    (setq w3m-search-default-engine "duckduckgo"))

  (defun my-w3m-rename-buffer (url)
    "base buffer name on title"
    (let* ((size 32)
           (title w3m-current-title)
           (name (truncate-string-to-width
                  (replace-regexp-in-string " " "_" title)
                  size)))
      (rename-buffer name t)))

  (add-hook 'w3m-mode-hook 'my-w3m-hook)
  (add-hook 'w3m-display-hook 'my-w3m-rename-buffer)

  (defadvice w3m-modeline-title (around my-w3m-modeline-title)
    "prevent original function from running; cleanup remnants"
    (setq w3m-modeline-separator ""
          w3m-modeline-title-string ""))
  (ad-activate 'w3m-modeline-title))


;;; Auctex
;;(load (concat elisp-dir "/auctex/auctex.el") nil t t)
;;(load (concat elisp-dir "/auctex/preview/preview-latex.el") nil t t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-insert-braces nil
      TeX-electric-escape t
      TeX-electric-macro t
      TeX-newline-function 'reindent-then-newline-and-indent)

;;; multiterm
(autoload 'multi-term-next "multi-term")
(setq multi-term-program (case system-type
                           (gnu/linux "/bin/bash")
                           (windows-nt "C:\\bin\\cmd.exe")
                           (berkeley-unix "/bin/ksh")
                           (usg-unix-v "/bin/ksh")
                           (t nil)))
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

;;;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(setq smex-save-file (concat emacs-dir "/smex-items"))

;;; undo-tree
(autoload 'global-undo-tree-mode "undo-tree")
(global-undo-tree-mode)

;; quack
(when (require 'quack nil 'noerror)
  (setq quack-default-program "csi"
        quack-dir (concat emacs-dir (file-name-as-directory "quack"))
        quack-fontify-style nil
        quack-newline-behavior 'indent-newline-indent
        quack-pretty-lambda-p nil
        quack-remap-find-file-bindings-p nil
        quack-run-scheme-always-prompts-p nil
        quack-run-scheme-prompt-defaults-to-last-p t
        quack-smart-open-paren-p t
        quack-switch-to-scheme-method 'other-window))


;;; gnus
(require 'gnus)
(load "/usr/share/emacs/24.1/lisp/gnus/mailcap.el") ;; XXX: find better way, conflicts with apel
(setq gnus-select-method '(nntp "news.gmane.org")
      mm-inline-text-html-with-images t
      mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-treat-hide-citation t
      gnus-cited-lines-visible '(0 . 5))

;; check for new messages every 10 mins
(require 'gnus-demon)
(gnus-demon-add-handler 'gnus-demon-scan-news 10 t)

;; use w3m to render HTML messages
(when (featurep 'w3m)
  (setq mm-text-html-renderer 'w3m))

;;  Use color-theme package on older than 24.1
(when (>= emacs-major-version 23)
  (setq custom-enabled-themes '(pastels-on-dark))
  (load-theme 'pastels-on-dark))

;; Zf-mode
(add-to-list 'load-path (concat elisp-dir (file-name-as-directory "zf-mode")))
(add-to-list 'load-path (concat elisp-dir (file-name-as-directory "zf-mode/bundled")))
(setq zf-html-basic-offset 4)
(require 'zf-mode)
(zf-mode-setup)

;; Slime
(add-to-list 'load-path (concat elisp-dir (file-name-as-directory "slime")))
(add-to-list 'load-path (concat elisp-dir (file-name-as-directory "slime/contrib")))
(setq slime-description-autofocus t
      slime-repl-history-trim-whitespaces t
      slime-repl-wrap-history t
      slime-repl-history-file (concat emacs-dir "slime-history.eld")
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
        (clisp ("clisp" "-ansi"))))

;; conflicts with clojure swank in newer Slime CVS (later than 2009-10-01)
(setq slime-use-autodoc-mode t)

(require 'slime) ; autoload here
 (slime-setup '(slime-asdf
                slime-indentation
                slime-mdot-fu
                slime-tramp
                slime-fancy
                slime-sbcl-exts
                slime-xref-browser))

(add-hook 'slime-repl-mode-hook
          #'(lambda ()
              (paredit-mode 1)))

(eval-after-load 'slime
  '(progn
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (add-hook 'lisp-mode-hook (lambda ()
                                 (slime-mode t)))))

 (global-set-key (kbd "C-c s") 'slime-selector)
 (def-slime-selector-method ?l
   "most recently visited lisp-mode buffer."
   (slime-recently-visited-buffer 'lisp-mode))
 (def-slime-selector-method ?c
   "most recently visited scheme-mode buffer."
   (slime-recently-visited-buffer 'scheme-mode))
 (def-slime-selector-method ?j
   "most recently visited clojure-mode buffer."
   (slime-recently-visited-buffer 'clojure-mode))
