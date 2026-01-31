;;; init-programming.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Programming related settings

;;; Code:

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;;; project

(setq project-vc-ignores '("target/" "bin/" "obj/"))
(setq project-vc-extra-root-markers '("pom.xml" "*.csproj" "*.asd"))
(setq project-vc-include-untracked nil)
(setq project-mode-line t)
(setq project-file-history-behavior 'relativize)
(setq project-key-prompt-style t)

(defun project-show-todos ()
  "Function shows all found TODO notes in given project in single buffer."
  (interactive)
  (project-find-regexp "\\(TODO:\\|HACK:\\|XXX:\\)"))

(define-key project-prefix-map (kbd "t") 'project-show-todos)

(setq project-mode-line t)
(setq project-file-history-behavior 'relativize)

(defun list-npm-package-files ()
  "List of all package.json files within a project."
  (seq-filter (lambda (filepath)
                (string-match "package\.json$" filepath))
              (project-files (project-current))))

(defun %parse-package-names (file)
  "Parse the names of npm packages."
  (unless (json-available-p)
    (error "JSON parsing not available"))
  (let ((packages nil))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((json-ht (json-parse-buffer))
             (dev-dependencies (gethash "devDependencies" json-ht))
             (dependencies (gethash "dependencies" json-ht)))
        (when dev-dependencies
          (maphash (lambda (k v)
                     (push k packages))
                   dev-dependencies))
        (when dependencies
          (maphash (lambda (k v)
                     (push k packages))
                   dependencies))))
    (delete-dups packages)))

(defun valid-npm-sem-ver-p (version-string)
  "Return t if given valid NPM semver string."
  (and (string-match "[\\^~]?[0-9\\.]+" version-string)
       t))

(defun %update-package-version (file package-name new-version)
  "Update the PACKAGE-NAME version in FILE to NEW-VERSION."
  (with-temp-file file
    (insert-file-contents file)
    (when-let* ((package-end (re-search-forward (format "^\s+\"\\%s\": \"\\([~\\.\\^0-9]+\\)\"" package-name) nil t)))
      (delete-region (search-backward ":") package-end)
      (insert (format ": \"%s\"" new-version)))))

(defun project-update-npm-package-version ()
  "Quickly update the npm package versions inside a project."
  (interactive)
  (let* ((package-files (list-npm-package-files))
         (existing-packages (mapcan #'%parse-package-names package-files))
         (package-name (completing-read "Give package name to update: " existing-packages))
         (new-version (read-from-minibuffer "Give new package version to set: ")))
    (unless (valid-npm-sem-ver-p new-version)
      (error "invalid version string: %s" new-version))
    (dolist (file package-files)
      (%update-package-version file package-name new-version))
    (message "updated package %s to version %s in project package.json files" package-name new-version)))

(defun project-update-npm-project-version ()
  "Quickly update the npm project version inside a project."
  (interactive)
  (let* ((package-files (list-npm-package-files))
         (new-version (read-from-minibuffer "Give new project version to set: ")))b
    (unless (valid-npm-sem-ver-p new-version)
      (error "invalid version string: %s" new-version))
    (dolist (file package-files)
      (%update-package-version file "version" new-version))
    (message "updated project version to %s in project package.json files" new-version)))

;; TODO: keybindings:
;; {C-x v t} - prefix for tag commands
;; {C-x v b} - prefix for branch commands
;; {C-x v b} prefix for branch commands: l, c s
;; M-x project-list-buffers {C-x p C-b} (change to ibuffer?)
;; M-x project-kill-buffers {C-x p k}
;; {C-x v !} -> edit next vc command
;; {C-x v v} in diffs, commit only part of changes
;; M-x vc-pull-push
;; M-x vc-prepare-patch
;; M-x vc-prepare-patches-separately
;; M-x vc-default-patch-adressee

(when (eq system-type 'berkeley-unix)
  (setenv "CVSROOT" "anoncvs.eu.openbsd.org:/cvs"))
;; config

(setq vc-suppress-confirm t)
(setq vc-command-messages t)
(setq vc-find-revision-no-save t)
(setq vc-annotate-display-mode 'fullscale)
(setq add-log-keep-changes-together t)
(setq vc-display-status 'no-backend)
(setq vc-annotate-use-short-revision t)

;; allow reverting changes in vc-dir
(with-eval-after-load 'vc-dir-mode
  (define-key vc-dir-mode-map (kbd "k") 'vc-revert))

;;; vc-git
(setq vc-git-annotate-switches "-w")
(setq vc-git-diff-switches '("--patch-with-stat"))
(setq vc-git-revision-complete-only-branches t)
(setq vc-git-print-log-follow nil)
(setq vc-git-shortlog-switches nil)
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

(defun vc-copy-revision-as-kill ()
  "Copy the revision string under point to the kill-ring."
  (when-let ((revision (cadr (log-view-current-entry))))
    (kill-new revision)
    (message "%s" revision)))

(defun my-vc-git-log-view-hook ()
  (define-key vc-git-log-view-mode-map "w" 'vc-copy-revision-as-kill))

(defun my-vc-got-log-view-hook ()
  (define-key vc-got-log-view-mode-map "w" 'vc-copy-revision-as-kill))

(add-hook 'vc-git-log-view-mode-hook #'my-vc-git-log-view-hook)
(add-hook 'vc-got-log-view-mode-hook #'my-vc-got-log-view-hook)


;; see: https://ane.iki.fi/emacs/patches.html
(defun vc-git-send-patch ()
  "Prepare git commits to be sent via email using Git CLI tools.

Perhaps useful to set global option: `git config --global
sendemail.annotate yes'."
  (interactive)
  (when (executable-find "git")
    (with-editor-async-shell-command "git send-email -1")))

(defun vc-git-apply-patch (patch project)
  "path to patch and project (default current if in one)"
  (interactive)
  (when (executable-find "git")
    (shell-command-on-region (point-min) (point-max) "git am")))

;; project-vc-dir or vc-dir {C-x p v} or {C-x v d}
;; vc-dir binds:
;; {z p}, {z s}, {z c} for stashing

;; From a diff buffer {C-x v =}
;; - drop a hunk: k
;; - split a hunk: C-c C-s
;; - commit remaining diff: C-x v v
;; TODO: how to get diff to update after dropping a hunk: was this solved in emacs-master?
;;
(ensure-packages-present 'vc-got)

(when (file-directory-p "~/git/vc-got")
  (add-to-list 'load-path "~/git/vc-got"))
(add-to-list 'vc-handled-backends 'Got)
(add-to-list 'vc-directory-exclusion-list ".got")

;;; xref
;; M-x xref-query-replace-in-results
;; M-x xref-find-references-and-replace

;; bug-reference
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(add-hook 'text-mode-hook 'bug-reference-mode)

;; compile
(setq compilation-save-buffers-predicate nil)
(setq compilation-scroll-output 'first-error)
(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
(setq compilation-window-height 12)
(setq ansi-color-for-compilation-mode t)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; or use smerge-ediff to resolve conflicts
;; smerge-mode
(setq smerge-command-prefix (kbd "C-c v")) ;; XXX: check this

;; diff-mode
(setq diff-advance-after-apply-hunk t)
(setq diff-default-read-only t)
(setq diff-font-lock-prettify nil)
(setq diff-font-lock-syntax 'hunk-also)
(setq diff-refine 'font-lock) ; 'navigation
(setq diff-update-on-the-fly t)
(setq diff-add-log-use-relative-names t)
(setq diff-refine-nonmodified t)
(setq diff-ignore-whitespace-switches "-b")
;; {C-c RET a} 'diff-apply-buffer'

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
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; enable which-func on programming modes NOTE: enable which-func only
;; on prog-mode instead of globally. This is to avoid having it enabled in diff-mode,
;; which causes cpu use due to looping in git remote call
(setq which-func-modes '(prog-mode))
(which-function-mode)

(ensure-packages-present 'magit)
(setq magit-repository-directories
      '(("~/git" . 1)
        ("~/quicklisp/local-projects" . 1)))
;; XXX: Symbol's value as variable is void: project-switch-commands
(add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
(global-set-key (kbd "C-c g") 'magit-status)

;; magit-gitflow
(when (string= (system-name) "ws-1127")
  (ensure-packages-present 'magit-gitflow)
  (require 'magit-gitflow nil t)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(ensure-packages-present 'eglot)
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)
  (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c z") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))

;; TODO: check this, should speed up eglot use
(fset #'jsonrpc--log-event #'ignore)

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
(setq eldoc-echo-area-use-multiline-p nil) ;; test t, 'truncate-sym-name-if-fit
(setq eldoc-idle-delay 0.1) ;; default 0.5

(ensure-packages-present 'sly)

(setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
                                 (ecl ("ecl"))
                                 (clisp ("clisp" "-ansi"))
                                 (chicken ("csi"))
                                 (abcl ("abcl"))))

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

;; compile and add sly info manual to emacs info-directory alist
(when-let* ((sly-doc-dirs (file-expand-wildcards (concat (locate-user-emacs-file "elpa") "/sly-*/doc"))))
  (let ((sly-doc-dir (car sly-doc-dirs)))
    (when (file-directory-p sly-doc-dir)
      ;; if no Info file found, generate it
      (unless (file-exists-p (concat sly-doc-dir "/sly.info"))
        (let ((default-directory (car sly-doc-dirs)))
          (async-shell-command "make sly.info")))
      (add-to-list 'Info-directory-list (car sly-doc-dirs)))))

;; if we have log4cl dist use it to set global logging
(let ((ql-software-dir (expand-file-name "~/quicklisp/dists/quicklisp/software/")))
  (when (file-exists-p ql-software-dir)
    (let ((default-directory ql-software-dir))
      (when-let* (log4cl-dirs (file-expand-wildcards "log4cl-*-git"))
        (display-warning 'warning "log4cl dirs: %s" log4cl-dirs)
        (add-to-list 'load-path (concat default-directory (car (last log4cl-dirs)) "/elisp"))
        (require 'log4sly nil t)
        (global-log4sly-mode 1)))))


(setq sly-mrepl-prevent-duplicate-history 'move)

;; (ensure-packages-present 'sly-repl-ansi-color)
;; (sly-enable-contrib 'sly-repl-ansi-color)

(ensure-packages-present 'quack)
(setq quack-default-program "csi")
(setq quack-dir (locate-user-emacs-file "quack"))
(setq quack-fontify-style nil)
(setq quack-newline-behavior 'indent-newline-indent)
(setq quack-pretty-lambda-p nil)
(setq quack-remap-find-file-bindings-p nil)
(setq quack-run-scheme-always-prompts-p nil)
(setq quack-run-scheme-prompt-defaults-to-last-p t)
(setq quack-smart-open-paren-p t)
(setq quack-switch-to-scheme-method 'other-window)

(when (and (not (getenv "GERBIL_HOME"))
           (file-exists-p "/usr/local/gerbil"))
  (setenv "GERBIL_HOME" "/usr/local/gerbil")
  (defvar gerbil-home (getenv "GERBIL_HOME"))
  (prepend-to-exec-path "/usr/local/gerbil/bin")

  (autoload 'gerbil-mode
    (concat gerbil-home "/etc/gerbil-mode.el") "Gerbil editing mode." t)

  (add-to-list 'auto-mode-alist '("\\.ss\\'"  . gerbil-mode))
  (add-to-list 'auto-mode-alist '("\\.pkg\\'"  . gerbil-mode))

  (let ((m comint-mode-map))
    (define-key m (kbd "C-S-n") 'comint-next-input)
    (define-key m (kbd "C-S-p") 'comint-previous-input)
    (define-key m (kbd "C-S-l") 'clear-comint-buffer))

  (with-eval-after-load 'gerbil-mode
    (setq scheme-program-name (concat gerbil "/bin/gxi"))
    (when (require 'gambit nil t)
      (setq scheme-program-name (concat gerbil-home "/bin/gxi"))
      (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)))

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (let ((gerbil-tags (concat gerbil-home "/src/TAGS")))
    (when (file-exists-p gerbil-tags)
      (visit-tags-table gerbil-tags)))

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
  (setq geiser-guile-binary "guile3.0"))

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
             ("\\.jsx\\'" . web-mode)))
  (add-to-list 'magic-mode-alist m))

(defun my/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  (when (and (member (file-name-extension buffer-file-name) '("jsx"))
             (require 'eglot nil 'noerror))
      (eglot-ensure)))

(add-hook 'web-mode-hook 'my/web-mode-hook)
(add-hook 'web-mode-hook 'flymake-eslint-enable)

(ensure-packages-present 'typescript-mode)
(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'flymake-eslint-enable)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'flymake-eslint-enable)
  (setq whitespace-line-column 120))

(when (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . json-ts-mode)))

(ensure-packages-present 'prettier)
(add-hook 'typescript-ts-hook #'prettier-mode)

(ensure-packages-present 'ts-comint)
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq typescript-ts-mode-indent-offset 4)
            (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'ts-send-buffer)
            (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))

;;; treesit
(when (featurep 'treesit)
  (setq treesit-language-source-alist
        '((clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (css . ("https://github.com/tree-sitter/tree-sitter-css" ))
          (html . ("https://github.com/tree-sitter/tree-sitter-html" ))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" ))
          (json . ("https://github.com/tree-sitter/tree-sitter-json" ))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src" ))
          (toml "https://github.com/tree-sitter/tree-sitter-toml" )
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  ;; color all the things
  (setq treesit-font-lock-level 4)
  ;; refresh font lock on buffers
  (treesit-font-lock-recompute-features)

  ;; Setup tree-sitter modes over the default ones.
  (dolist (m '((python-mode . python-ts-mode)
               (css-mode . css-ts-mode)
               (typescript-mode . typescript-ts-mode)
               (js2-mode . js-ts-mode)
               (bash-mode . bash-ts-mode)
               (css-mode . css-ts-mode)
               (json-mode . json-ts-mode)
               (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist m))

  (defun install-treesit-grammars ()
    "Install all Tree-sitter grammars which are not present on current system."
    (interactive)
    (dolist (grammar (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p grammar)
        (treesit-install-language-grammar grammar)))
    t))

;; TODO: replace with `llm' package from ELPA.
(ensure-packages-present 'llm)
(setq llm-warn-on-nonfree nil)

(ensure-packages-present 'gptel)
(with-eval-after-load 'gptel
  (gptel-make-openai "local-deepcoder"
    :stream t
    :protocol "http"
    :host "localhost:8080"
    :models '("coder"))
  (setq gptel-model "gemini-pro"
        gptel-backend (gptel-make-gemini "Gemini"
                        :key "YOUR_GEMINI_API_KEY"
                        :stream t)))

(ensure-packages-present 'vcl-mode)

(defun my-sql-mode-hook ()
  "Functions to run when entering SQL-MODE."
  (setq-local truncate-lines t))
(add-hook 'sql-interactive-mode-hook #'my-sql-mode-hook)

(provide 'init-programming)

;; init-programming.el ends here
