;; ------------------------------
;; External packages
;; ------------------------------

;; Add info's to proper place
(dolist (d (list "emacs-w3m/doc" "bbdb/texinfo" "slime/info"))
  (let ((dir (expand-file-name (concat-path elisp-dir d))))
    (when (file-exists-p dir)
      (add-to-list 'Info-default-directory-list dir))))

;;(autoload 'yas/hippie-try-expand "yasnippet")
(require 'yasnippet)
(eval-after-load "yasnippet"
  '(progn
     (yas-global-mode 1)))

;; Web Browsing
(autoload 'w3m "w3m" "Visit the WWW page using w3m" t)
(autoload 'w3m-find-file "w3m" "Find a local file using emacs-w3m." t)
(autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to show a URL." t)
(autoload 'w3m-antenna "w3m-antenna" "Report changes of web sites." t)
(autoload 'w3m-bookmark-view "w3m-bookmark" "Show bookmarks." t)
(autoload 'w3m-dtree "w3m-dtree" "Display a directory tree." t)
(autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t)
(autoload 'w3m-perldoc "w3m-perldoc" "View Perl documents" t)
(autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)
(autoload 'w3m-weather "w3m-weather" "Display a weather report." t)

(eval-after-load "w3m-search"
  '(progn
     (add-to-list 'w3m-search-engine-alist '("duckduckgo" "https://duckduckgo.com/?q=%s"))
     (add-to-list 'w3m-search-engine-alist '("fi.wikipedia" "http://fi.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8))
     (setq w3m-search-default-engine "duckduckgo")))

(eval-after-load "w3m"
  '(progn

     (when (featurep 'newsticker) ; wrong test
       (setq newsticker-html-renderer 'w3m-region))

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
       (define-key w3m-mode-map "x" 'w3m-next-buffer))

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
     (ad-activate 'w3m-modeline-title)))


;;; Auctex
(load (concat-path elisp-dir "auctex/auctex.el") nil t t)
(load (concat-path elisp-dir "auctex/preview/preview-latex.el") nil t t)
(eval-after-load 'auctex
  '(progn
     (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
     (setq TeX-auto-save t
           TeX-parse-self t
           TeX-insert-braces nil
           TeX-electric-escape t
           TeX-electric-macro t
           TeX-newline-function 'reindent-then-newline-and-indent)))

;;; multi-term
(autoload 'multi-term-next "multi-term" "Multi-term mode" t)
(autoload 'multi-term "multi-term" "Multi-term mode" t)
(eval-after-load 'multi-term
  '(progn
     (setq multi-term-program (case system-type
                                (gnu/linux "/bin/bash")
                                (windows-nt "C:\\bin\\cmd.exe")
                                (berkeley-unix "/bin/ksh")
                                (usg-unix-v "/bin/ksh")))))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

;; smex
(autoload 'smex "smex" "Smex" t)
(autoload 'smex-major-mode-commands "smex" "Smex" t)
(eval-after-load 'smex
  '(progn
     (smex-initialize)
     (setq smex-save-file (concat-path emacs-dir "/smex-items"))))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; undo-tree
(autoload 'global-undo-tree-mode "undo-tree")
(eval-after-load "undo-tree"
  '(progn
     (global-undo-tree-mode)))

;; quack
(eval-after-load 'quack
  '(progn
     (setq quack-default-program "csi"
           quack-dir (concat-path emacs-dir "quack")
           quack-fontify-style nil
           quack-newline-behavior 'indent-newline-indent
           quack-pretty-lambda-p nil
           quack-remap-find-file-bindings-p nil
           quack-run-scheme-always-prompts-p nil
           quack-run-scheme-prompt-defaults-to-last-p t
           quack-smart-open-paren-p t
           quack-switch-to-scheme-method 'other-window)))

;;; gnus
(autoload 'gnus "gnus" "news reader" t)
(eval-after-load 'info
  '(progn
     (if (featurep 'xemacs)
         (add-to-list 'Info-directory-list (concat-path elisp-dir "gnus" "texi"))
       (add-to-list 'Info-default-directory-list (concat-path elisp-dir "gnus" "texi")))))

(setq gnus-select-method '(nntp "news.gmane.org")
      mm-inline-text-html-with-images t
      mm-inline-large-images 'resize
      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-treat-hide-citation t
      gnus-cited-lines-visible '(0 . 5))

;; check for new messages every 10 mins
(eval-after-load 'gnus-daemon
  '(progn
     (gnus-demon-add-handler 'gnus-demon-scan-news 10 t)))

;; use w3m to render HTML messages
(if (featurep 'w3m)
    (setq mm-text-html-renderer 'w3m)
  (setq mm-text-html-renderer 'shr))

;; Set color-theme options
(cond ((>= emacs-major-version 24)
       (setq custom-enabled-themes '(pastels-on-dark)))
      ((>= emacs-major-version 23)
       (setq custom-theme-load-path nil)
       (load-theme 'pastels-on-dark)))

;; Zf-mode for PHP
(eval-after-load 'zf-mode
  '(progn
     (setq zf-html-basic-offset 4)
     (zf-mode-setup)))

(autoload 'zf-mode "zf-mode" "ZF-mode for PHP" t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . zf-mode))

;; Slime
(load (concat-path elisp-dir "slime" "slime-autoloads.el") nil t)
(eval-after-load 'slime
  '(progn
     (setq slime-description-autofocus t
           slime-repl-history-trim-whitespaces t
           slime-repl-wrap-history t
           slime-repl-history-file (concat emacs-dir "slime-history.eld")
           slime-repl-history-remove-duplicates t
           slime-ed-use-dedicated-frame t
           slime-kill-without-query-p t
           slime-startup-animation t
           slime-net-coding-system 'utf-8-unix
           slime-lisp-implementations
           '((sbcl  ("sbcl"))
             (clisp ("clisp" "-ansi"))))

     (setq common-lisp-hyperspec-root nil)
     (cond ((file-exists-p "/usr/local/share/doc/clisp-hyperspec")
            (setq common-lisp-hyperspec-root
                  (concat "file:" "/usr/local/share/doc/clisp-hyperspec")))
           ((file-exists-p "~/lisp/docs/HyperSpec")
            (setq common-lisp-hyperspec-root
                  (concat "file:" "~/lisp/docs/HyperSpec"))))

     (when common-lisp-hyperspec-root
       (setq common-lisp-hyperspec-symbol-table
             (concat-path common-lisp-hyperspec-root "Data/Map_Sym.txt")))

     ;; conflicts with clojure swank in newer Slime CVS (later than 2009-10-01)
     (setq slime-use-autodoc-mode t)

     ;; Load contrib modules
     (slime-setup '(slime-asdf
                    slime-indentation
                    slime-mdot-fu
                    slime-tramp
                    slime-fancy
                    slime-sbcl-exts
                    slime-xref-browser))
     (slime-autodoc-mode 1)
     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (add-hook 'lisp-mode-hook 'slime-mode)
     (add-hook 'slime-repl-mode-hook 'paredit-mode)
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
