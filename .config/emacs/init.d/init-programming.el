;;; init-programming.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Programming related settings

;;; Code:

;;; project
(setq project-vc-ignores '("target/" "bin/" "obj/"))
(setq project-vc-extra-root-markers '("pom.xml" "*.csproj" "*.asd"))
;;(add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
;;((nil . ((compile-command . "make --directory=doc/site"))))

;; project commands
;; project-list-buffers C-x p C-b (change to ibuffer?)
;; project-kill-buffers C-x p k
;; project-show-todos C-x p t
(setq project-vc-include-untracked nil)

(defun project-show-todos ()
  "Function shows all found TODO notes in given project in single buffer."
  (interactive)
  (project-find-regexp "\\(TODO:\\|HACK:\\|XXX:\\)"))

(define-key project-prefix-map (kbd "t") 'project-show-todos)

;; TODO: keybindings:
;; C-x v t - prefix for tag commands
;; C-x v b - prefix for branch commands

;;  vc
;; vc-pull-push
;; C-x v b prefix for branch commands: l, c s
;; C-x v ! -> edit next vc command
;; C-x v v in diffs, commit only part of changes
;; vc-prepare-patch, vc-prepare-patches-separately, vc-default-patch-adressee

;;; xref
;; Commands:
;; xref-query-replace-in-results
;; xref-find-references-and-replace

(when (eq system-type 'berkeley-unix)
  (setenv "CVSROOT" "anoncvs.eu.openbsd.org:/cvs"))
;; config
;; `vc-git-annotate-switches' to "-w"
(setq vc-suppress-confirm t)
(setq vc-command-messages t)
(setq vc-find-revision-no-save t)
(setq vc-annotate-display-mode 'scale)
(setq add-log-keep-changes-together t)
(setq vc-git-diff-switches '("--patch-with-stat"))
(setq vc-git-revision-complete-only-branches t)
(setq vc-git-print-log-follow t)
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

;; allow reverting changes in vc-dir
(with-eval-after-load 'vc-dir-mode
  (define-key vc-dir-mode-map (kbd "R") 'vc-revert))

(defun vc-git-checkout-remote ()
  "Checkout Git remote and set local branch to track it."
  )

(defun vc-git-kill-commit-hash ()
  "Kill commit hash at point to kill-ring for later use."
  (interactive)
  ;; assume point is either on commit line or we need to back towards
  ;; commit line to get current revisions hash
  (let ((commit-re "^commit [a-z0-9]+$"))
    (save-excursion
      (unless (bolp)
        (beginning-of-line))
      (or (looking-at commit-re)
          (re-search-backward commit-re))
      (forward-word)
      (forward-char)
      (kill-line))))

;; see:  https://ane.iki.fi/emacs/patches.html
(defun vc-git-send-patch ()
  "Prepare git commits to be sent via email using Git CLI tools.

Perhaps useful to set global option: `git config --global
sendemail.annotate yes'."
  (interactive)
  (with-editor-async-shell-command "git send-email -1"))

(defun vc-git-apply-patch (patch project)
  "path to patch and project (default current if in one)"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "git am"))

(ensure-packages-present 'vc-got)

(when (file-directory-p "~/git/vc-got")
  (add-to-list 'load-path "~/git/vc-got"))
(add-to-list 'vc-handled-backends 'Got)
(add-to-list 'vc-directory-exclusion-list ".got")

;; bug-reference
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(add-hook 'text-mode-hook 'bug-reference-mode)

;; compile
(setq compilation-save-buffers-predicate nil)
(setq compilation-scroll-output 'first-error)
(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
(setq compilation-window-height 12)

;;  ansi-color
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; :preface
(autoload 'ansi-color-apply-on-region "ansi-color")
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) ;; TODO: or compilation-filter-start?
                                (point-max))))

;; or use smerge-ediff to resolve conflicts
;; smerge-mode
;;  :init
(setq smerge-command-prefix (kbd "C-c v"))

;; diff-mode
(setq diff-advance-after-apply-hunk t)
(setq diff-default-read-only t)
(setq diff-font-lock-prettify nil)
(setq diff-font-lock-syntax 'hunk-also)
(setq diff-refine 'font-lock)
(setq diff-update-on-the-fly t)

(setq diff-add-log-use-relative-names t) ; 29

;; diff
(setq diff-switches '("-u"))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(add-hook 'ediff-after-quit-hook-internal-hook 'winner-undo)

;; subword
(add-hook 'prog-mode-hook 'subword-mode)

;; prog-mode
(defun my/prog-mode-hook ()
  "Hook to run when entering generic prog-mode."
  (set (make-local-variable 'which-func-unknown) "TOP LEVEL")
  (set (make-local-variable 'whitespace-line-column) 80)
  (set (make-local-variable 'whitespace-style) '(face lines-tail))
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                 1 font-lock-warning-face prepend))))
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'which-function-mode)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

(ensure-packages-present 'magit)
(setq magit-repository-directories
      '(("~/git" . 1)
        ("~/quicklisp/local-projects" . 1)))
(global-set-key (kbd "C-c g") 'magit-status)

;; magit-gitflow
(when (string= (system-name) "ws-926")
  (ensure-packages-present 'magit-gitflow)
  (require 'magit-gitflow nil t)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(ensure-packages-present 'eglot)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point))

;; flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(ensure-packages-present 'flymake-eslint)
(setq flymake-eslint-executable-name "eslint")
(setq flymake-eslint-executable-args nil)
(setq flymake-eslint-show-rule-name t)
(setq flymake-eslint-defer-binary-check t)

;;; Go programming
;; init
(setenv "GOPATH" (expand-file-name "workspace" "~"))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'eglot-ensure)

(ensure-packages-present '(go-mode go-eldoc))
(with-eval-after-load 'go-mode
  (let ((m go-mode-map))
    (define-key m (kbd "M-.") 'godef-jump)
    (define-key m (kbd "C-c C-r") 'go-remove-unused-imports)
    (define-key m (kbd "C-c g i") 'go-goto-imports)
    (define-key m (kbd "C-c C-k") 'godoc))

  (require 'go-eldoc nil t)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;;; Ruby
;;(add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
(dolist (m '(("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
             ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"  . ruby-mode)))
  (add-to-list 'magic-mode-alist m))

(defun my/ruby-mode-hook ()
  (setq ruby-deep-arglist t)
  (setq ruby-deep-indent-paren nil)
  (setq c-tab-always-indent nil))
(add-hook 'ruby-mode-hook 'my/ruby-mode-hook)

;;; Lisp programming

(global-eldoc-mode 1)

(ensure-packages-present 'sly)

(setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
                                 (ecl ("ecl"))
                                 (clisp ("clisp" "-ansi"))
                                 (chicken ("csi"))
                                 (abcl ("abcl"))))
(when-let ((local-hyperspec-path
            (seq-some (lambda (p)
                        (let ((full-path (expand-file-name p)))
                          (when (file-directory-p full-path)
                            full-path)))
                      '("/usr/local/share/doc/clisp-hyperspec/"
                        "/usr/share/doc/hyperspec/"
                        "~/src/lisp/HyperSpec/"))))
  (setq common-lisp-hyperspec-root (concat "file://" local-hyperspec-path))
  (setq common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")))

;; if we have log4cl dist use it to set global logging
(let ((default-directory (expand-file-name "~/quicklisp/dists/quicklisp/software/")))
  (when-let (log4cl-dirs (file-expand-wildcards "log4cl-*-git"))
    (display-warning 'warning "log4cl dirs: %s" log4cl-dirs)
    (add-to-list 'load-path (concat default-directory (car (last log4cl-dirs)) "/elisp"))
    (require 'log4sly nil t)
    (global-log4sly-mode 1)))

;; (ensure-packages-present 'sly-repl-ansi-color)
;; (sly-enable-contrib 'sly-repl-ansi-color)

;; (zmg/package-instal 'quack)
;; (setq quack-default-program "csi")
;; (setq quack-dir (locate-user-emacs-file "quack"))
;; (setq quack-fontify-style nil)
;; (setq quack-newline-behavior 'indent-newline-indent)
;; (setq quack-pretty-lambda-p nil)
;; (setq quack-remap-find-file-bindings-p nil)
;; (setq quack-run-scheme-always-prompts-p nil)
;; (setq quack-run-scheme-prompt-defaults-to-last-p t)
;; (setq quack-smart-open-paren-p t)
;; (setq quack-switch-to-scheme-method 'other-window)

(when (and (not (getenv "GERBIL_HOME"))
           (file-exists-p "/usr/local/gerbil"))
  (setenv "GERBIL_HOME" "/usr/local/gerbil")
  (defvar gerbil-home (getenv "GERBIL_HOME"))
  (prepend-to-exec-path "/usr/local/gerbil/bin")

  (autoload 'gerbil-mode
    (concat gerbil-home "/etc/gerbil-mode.el") "Gerbil editing mode." t)

  (add-to-list auto-mode-alist '(("\\.ss\\'"  . gerbil-mode)
                                 ("\\.pkg\\'" . gerbil-mode)))
  (let ((m comint-mode-map))
    (define-key m (kbd "C-S-n") 'comint-next-input)
    (define-key m (kbd "C-S-p") 'comint-previous-input)
    (define-key m (kbd "C-S-l") 'clear-comint-buffer))

  (with-eval-after-load 'gerbil-mode
    (setf scheme-program-name (concat gerbil "/bin/gxi"))
    (when (require 'gambit nil t)
      (setf scheme-program-name (concat gerbil-home "/bin/gxi"))
      (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode))

    (define-key m (kbd "C-S-l") 'clear-comint-buffer)

    (defun gerbil-setup-buffers ()
      "Change current buffer mode to gerbil-mode and start a REPL"
      (interactive)
      (gerbil-mode)
      (split-window-right)
      (shrink-window-horizontally 2)
      (let ((buf (buffer-name)))
        (other-window 1)
        (run-scheme "gxi")
        (switch-to-buffer-other-window "*scheme*" nil)
        (switch-to-buffer buf))))

  (global-set-key (kbd "C-c C-g") 'gerbil-setup-buffers)

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (visit-tags-table (concat gerbil "/src/TAGS"))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))))

(ensure-packages-present '(clojure-mode cider))
(add-to-list 'magic-mode-alist '("\\.clj$" . clojure-mode))

(setq cider-lein-parameters "repl :headless :host localhost")
(setq nrepl-hide-special-buffers t)

(ensure-packages-present 'geiser)
(when (eq system-type 'berkeley-unix)
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-guile-binary "guile2"))

;;;; PHP programming

(ensure-packages-present 'php-mode)
(add-to-list 'magic-mode-alist '("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))
(with-eval-after-load 'php-mode
  (defun my/php-mode-hook ()
    (setq php-site-url "http://fi2.php.net/")
    (php-enable-symfony2-coding-style)
    (define-abbrev php-mode-abbrev-table "ex" "extends")
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq c-basic-offset 4))
  (add-hook 'php-mode-hook 'my/php-mode-hook))

;;;; C programming

(with-eval-after-load 'cc-mode
  (let ((m c-mode-map))
    (define-key m (kbd "C-h M") 'man-follow)
    (define-key m (kbd "C-c C-d") 'gdb)
    (define-key m (kbd "C-m") 'c-context-line-break)
    (define-key m (kbd "C-c o") 'ff-find-other-file))

  (add-hook 'c-mode-common-hook 'which-function-mode)
  (add-hook 'c-mode-common-hook 'cwarn-mode)
  (add-hook 'c-mode-hook 'my/c-mode)
  (add-hook 'c++-mode-hook 'my/c-mode)

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

;;; Perl
(if (version<= emacs-version "29")
    (defalias 'perl-mode 'cperl-mode)
  (add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode)))

(defvar cperl-font-lock)
(defvar cperl-info-on-command-no-prompt)
(defvar cperl-clobber-lisp-bindings)
(defvar cperl-lazy-help-time)
(defvar cperl-indent-level)
(defvar cperl-invalid-face)
(with-eval-after-load 'cperl-mode
  (defun my/cperl-mode-hook ()
    "Default CPerl settings."
    (setq cperl-font-lock t)
    (setq cperl-info-on-command-no-prompt t)
    (setq cperl-clobber-lisp-bindings t)
    (setq cperl-lazy-help-time 5)
    (setq cperl-indent-level 4)
    (setq cperl-invalid-face 'default))
  (add-hook 'cperl-mode-hook 'my/cperl-mode-hook))

;; (zmg/package-install 'elpy)
;; ;; init
;; (advice-add 'python-mode :before 'elpy-enable)

;; (setq elpy-rpc-python-command "python3")
;; ;; Use IPython for REPL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter"))

;; (zmg/package-install 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; integrate with jupyter

;; (zmg/package-install 'ein)
;; (setq ein:jupyter-server-command "jupyter-notebook")
;; (setq ein:jupyter-server-use-subcommand nil)

(ensure-packages-present 'web-mode)
(dolist (m '(("\\.jsp\\'" . web-mode)
             ("\\.ap[cp]x\\'" . web-mode)
             ("\\.erb\\'" . web-mode)
             ("\\.rhtml\\'" . web-mode)
             ("\\.mustache\\'" . web-mode)
             ("\\.djhtml\\'" . web-mode)
             ("\\.tsx\\'" . web-mode)
             ("\\.jsx\\'" . web-mode)))
  (add-to-list 'magic-mode-alist m))

(defun my/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  (when (and (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
             (require 'eglot nil 'noerror))
      (eglot-ensure)))

(add-hook 'web-mode-hook 'my/web-mode-hook)
(add-hook 'web-mode-hook 'flymake-eslint-enable)

(ensure-packages-present 'typescript-mode)
(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'flymake-eslint-enable)
  (setq whitespace-line-column 120))

(provide 'init-programming)

;; init-programming.el ends here
