;;; ------------------------------
;;; Programming settings
;;; ------------------------------

(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

;; TODO: keybindings:
;; C-x v t - prefix for tag commands
;; C-x v b - prefix for branch commands

(use-package vc
  :init
  (when (eq system-type 'berkeley-unix)
    (setenv "CVSROOT" "anoncvs.eu.openbsd.org:/cvs"))
  :config
  ;; `vc-git-annotate-switches' to "-w"
  (setq vc-suppress-confirm t)
  (setq vc-command-messages t)
  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale)
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil)
  (setq vc-git-root-log-format
        '("%d %h %ad %an: %s"
          "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (defun vc-git-checkout-remote ()
    "Checkout Git remote and set local branch to track it."
    )

  (defun vc-git-kill-commit-hash (log)
    "Kill commit hash at point to kill-ring for later use."
    (interactive)
    (let ((hash 1))
      ))

  ;; see:  https://ane.iki.fi/emacs/patches.html
  (defun vc-git-send-patch ()
    "Prepare git commits to be sent via email using Git CLI tools.

Perhaps useful to set global option: `git config --global sendemail.annotate yes'."
    (interactive)
    (with-editor-async-shell-command "git send-email -1"))

  (defun vc-git-apply-patch (patch project)
    "path to patch and project (default current if in one)"
    (interactive)
    (shell-command-on-region (point-min) (point-max) "git am")))

(use-package vc-got
  :if (file-directory-p "~/git/vc-got")
  :load-path "~/git/vc-got"
  :defer t
  :init
  (add-to-list 'vc-handled-backends 'Got)
  (add-to-list 'vc-directory-exclusion-list ".got"))

(use-package bug-reference
  :hook ((prog-mode-hook . bug-reference-prog-mode)
         (text-mode-hook . bug-reference-mode)))

(use-package compile
  :config
  (setq compilation-save-buffers-predicate nil)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (setq compilation-window-height 12))

(use-package ansi-color
  :after (compile)
  :hook (compilation-filter-hook . colorize-compilation-buffer)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) ;; TODO: or compilation-filter-start?
                                  (point-max)))))

;; or use smerge-ediff to resolve conflicts
(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package diff-mode
  :config
  (setq diff-advance-after-apply-hunk t)
  (setq diff-default-read-only t)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-refine 'font-lock)
  (setq diff-update-on-the-fly t))

(use-package diff
  :config (setq diff-switches '("-u")))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  :hook (ediff-after-quit-hook-internal-hook . winner-undo))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package prog-mode
  :config
  (defun my/prog-mode-hook ()
    "Hook to run when entering generic prog-mode."
    (set (make-local-variable 'which-func-unknown) "TOP LEVEL")
    (set (make-local-variable 'whitespace-line-column) 80)
    (set (make-local-variable 'whitespace-style) '(face lines-tail))
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                   1 font-lock-warning-face prepend))))
  :hook ((prog-mode-hook . electric-pair-mode)
         (prog-mode-hook . whitespace-mode)
         (prog-mode-hook . which-function-mode)
         (prog-mode-hook . my/prog-mode-hook)))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config (setq magit-repository-directories
                '(("~/git" . 1)
                  ("~/quicklisp/local-projects" . 1))))

(use-package magit-gitflow
  :if (string= (system-name) "ws-946")
  :ensure t
  :hook (magit-mode-hook . turn-on-magit-gitflow))

(use-package eglot
  :ensure t
  :bind ((:map eglot-mode-map
               ("C-c h" . eglot-help-at-point))))

(use-package flymake
  :ensure t
  :bind ((:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

(use-package flymake-eslint
  :ensure t
  :config
  (setq flymake-eslint-executable-name "eslint")
  (setq flymake-eslint-executable-args nil)
  (setq flymake-eslint-show-rule-name t)
  (setq flymake-eslint-defer-binary-check t))

;;; Go programming

(use-package go-mode
  :ensure t
  :after eglot
  :init (setenv "GOPATH" (expand-file-name "workspace" "~"))
  :hook ((before-save-hook . gofmt-before-save)
         (go-mode-hook . eglot-ensure))
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c g i" . go-goto-imports)
              ("C-c C-k" . godoc)))

(use-package go-eldoc
  :ensure t
  :hook (go-mode-hook . go-eldoc-setup))

;;; Ruby

(use-package ruby-mode
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"  . ruby-mode))
  :init
  (defun my/ruby-mode-hook ()
    (setq ruby-deep-arglist t)
    (setq ruby-deep-indent-paren nil)
    (setq c-tab-always-indent nil))
  :hook (ruby-mode-hook . my/ruby-mode-hook))

;;; Lisp programming

(use-package sly
  :ensure t
  :config
  (let ((sbcl-bin-path (expand-file-name "lib/sbcl" "~")))
    (when (file-exists-p sbcl-bin-path)
      (setenv "SBCL_HOME" sbcl-bin-path)))
  (setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
                                   (ecl ("ecl"))
                                   (clisp ("clisp" "-ansi"))
                                   (chicken ("csi"))
                                   (abcl ("abcl"))))
  (setq common-lisp-hyperspec-symbol-table
        (let ((pkg-path "/usr/local/share/doc/clisp-hyperspec/Data/Map_Sym.txt")
              (home-path "~/lisp/docs/HyperSpec/Data/Map_Sym.txt"))
          (cond ((file-exists-p pkg-path) pkg-path)
                ((file-exists-p home-path) home-path)
                (t "http://www.lispworks.com/documentation/HyperSpec/Data/Map_Sym.txt")))))

;; (use-package sly-repl-ansi-color
;;   :ensure t
;;   :after sly
;;   :config (sly-enable-contrib 'sly-repl-ansi-color))

(use-package quack
  :disabled
  :ensure t
  :config
  (setq quack-default-program "csi")
  (setq quack-dir (concat user-emacs-directory "quack/"))
  (setq quack-fontify-style nil)
  (setq quack-newline-behavior 'indent-newline-indent)
  (setq quack-pretty-lambda-p nil)
  (setq quack-remap-find-file-bindings-p nil)
  (setq quack-run-scheme-always-prompts-p nil)
  (setq quack-run-scheme-prompt-defaults-to-last-p t)
  (setq quack-smart-open-paren-p t)
  (setq quack-switch-to-scheme-method 'other-window))

(use-package gerbil-mode
  :ensure nil
  :defer t
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :bind (:map comint-mode-map
              (("C-S-n" . comint-next-input)
               ("C-S-p" . comint-previous-input)
               ("C-S-l" . clear-comint-buffer))
              :map gerbil-mode-map
              (("C-S-l" . clear-comint-buffer)))
  :init
  (when (and (not (getenv "GERBIL_HOME"))
             (file-exists-p "/usr/local/gerbil"))
    (setenv "GERBIL_HOME" "/usr/local/gerbil")
    (prepend-to-exec-path "/usr/local/gerbil/bin"))

  (defvar gerbil-home (getenv "GERBIL_HOME"))
  (autoload 'gerbil-mode
    (concat gerbil-home "/etc/gerbil-mode.el") "Gerbil editing mode." t)
  :hook
  ((gerbil-mode-hook . linum-mode)
   (inferior-scheme-mode-hook . gambit-inferior-mode))
  :config
  (require 'gambit
           (concat (getenv "GAMBIT_HOME") "/misc/gambit.el"))
  (setf scheme-program-name (concat gerbil-home "/bin/gxi"))

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (visit-tags-table (concat gerbil "/src/TAGS"))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj$")

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-lein-parameters "repl :headless :host localhost")
  (setq nrepl-hide-special-buffers t))

(use-package geiser
  :disabled
  :ensure t
  :config
  (when (eq system-type 'berkeley-unix)
    (setq geiser-chicken-binary "chicken-csi")
    (setq geiser-guile-binary "guile2")))

;;;; PHP programming

(use-package php-mode
  :ensure t
  :mode "\\.php[345]?\\'\\|\\.phtml\\'"
  :config
  (defun my/php-mode-hook ()
    (setq php-site-url "http://fi2.php.net/")
    (php-enable-symfony2-coding-style)
    (define-abbrev php-mode-abbrev-table "ex" "extends")
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq c-basic-offset 4))
  :hook (php-mode-hook . my/php-mode-hook))

;;;; C programming

(use-package cc-mode
  :bind (:map c-mode-map
              ("C-h M" . man-follow)
              ("C-c C-d" . gdb)
              ("C-m" . c-context-line-break)
              ("C-c o" . ff-find-other-file))
  :hook ((c-mode-common-hook . which-function-mode)
         (c-mode-common-hook . cwarn-mode)
         (c-mode-hook . 'my/c-mode)
         (c++-mode-hook . 'my/c-mode))
  :config
  (defun my/c-mode ()
    "My C programming options."
    (c-set-style "bsd")
    (setq indent-tabs-mode t))
  (defun my/c++-mode ()
    "My C++ programming options."
    (setq fill-column 100)
    (c-set-style "stroustrup")
    (setq whitespace-line-column 100
          whitespace-style '(face lines-tail))))

(use-package cperl-mode
  :init
  (defalias 'perl-mode 'cperl-mode)
  :config
  (defun my/cperl-mode-hook ()
    "Default CPerl settings."
    (setq cperl-font-lock t)
    (setq cperl-info-on-command-no-prompt t)
    (setq cperl-clobber-lisp-bindings t)
    (setq cperl-lazy-help-time 5)
    (setq cperl-indent-level 4)
    (setq cperl-invalid-face 'default))
  :hook (cperl-mode-hook . my/cperl-mode-hook))

(use-package elpy
  :disabled
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  ;; Use IPython for REPL
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(use-package py-autopep8
  :disabled
  :ensure t
  :hook (elpy-mode-hook . py-autopep8-enable-on-save))

;; integrate with jupyter

(use-package ein
  :disabled
  :ensure t
  :config
  (setq ein:jupyter-server-command "jupyter-notebook")
  (setq ein:jupyter-server-use-subcommand nil))

(use-package web-mode
  :ensure t
  :mode (("\\.jsp\\'" . web-mode)
         ("\\.ap[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (defun my/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4)
    (when (and (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
               (require 'eglot nil 'noerror))
      (eglot-ensure)))
  :hook ((web-mode-hook . my/web-mode-hook)
         (web-mode-hook . flymake-eslint-enable)))

(use-package typescript-mode
  :ensure t
  :after flymake-eslint
  :hook ((typescript-mode-hook . eglot-ensure)
         (typescript-mode-hook . flymake-eslint-enable))
  :config
  (setq whitespace-line-column 120))

(use-package keepass-mode
  :ensure t)

(provide 'zmg-emacs-programming)
