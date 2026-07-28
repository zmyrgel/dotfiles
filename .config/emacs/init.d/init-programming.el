;;; init-programming.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Generic Programming related settings

;;; Code:

(setopt eldoc-help-at-pt t)
(setopt eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(setopt eldoc-idle-delay 0.1) ;; default 0.5
(add-hook 'after-init-hook 'global-eldoc-mode)

(defun my-sql-mode-hook ()
  "Functions to run when entering SQL-MODE."
  (setq-local truncate-lines t))
(add-hook 'sql-interactive-mode-hook #'my-sql-mode-hook)

;;; vc and general programming configuration

;;; treesit
(when (featurep 'treesit)
  (setopt treesit-enabled-modes t)
  (setopt treesit-auto-install-grammar 'always)

  ;; color all the things
  (setopt treesit-font-lock-level 4)
  ;; refresh font lock on buffers
  (treesit-font-lock-recompute-features))

;;; project

(setopt project-vc-ignores '("target/" "bin/" "obj/"))
(setopt project-vc-extra-root-markers '("pom.xml" "*.csproj" "*.asd"))
(setopt project-vc-include-untracked nil)
(setopt project-mode-line t)
(setopt project-file-history-behavior 'relativize)
(setopt project-key-prompt-style t)

(defun project-show-todos ()
  "Function shows all found TODO notes in given project in single buffer."
  (interactive)
  (project-find-regexp "\\(TODO:\\|HACK:\\|XXX:\\)"))

(keymap-set project-prefix-map "t" 'project-show-todos)

(setopt project-mode-line t)
(setopt project-file-history-behavior 'relativize)

;;; vc

(setopt vc-use-incoming-outgoing-prefixes t)
(setopt vc-dir-show-outgoing-count t) ;; default

;; (setq vc-async-checkin t) ;; check this
;; (setq vc-display-failed-async-commands t)

;; 'vc-rename-file' is now bound to 'C-x v R'.

;; TODO: add vc-got
;; *** 'C-u C-x v +' and 'C-u C-x v P' for Git have an input history.
;; This was already in place for Mercurial.

(add-hook 'log-edit-hook #'log-edit-maybe-show-diff)

(when (eq system-type 'berkeley-unix)
  (setenv "CVSROOT" "anoncvs.eu.openbsd.org:/cvs"))

(setopt vc-suppress-confirm t)
(setopt vc-command-messages t)
(setopt vc-find-revision-no-save t)
(setopt vc-annotate-display-mode 'fullscale)
(setopt add-log-keep-changes-together t)
(setopt vc-display-status 'no-backend)
(setopt vc-annotate-use-short-revision t)
;;(setopt vc-dir-show-key-binding-hints t) ; 32

;; (setopt vc-dir-auto-hide-up-to-date 'revert) t nil
(setopt vc-dir-save-some-buffers-on-revert t)

;;; vc-git
(setopt vc-git-show-stash 0) ;; hide stash by default
(setopt vc-git-annotate-switches "-w")
(setopt vc-git-diff-switches '("--patch-with-stat"))
(setopt vc-git-revision-complete-only-branches t)
(setopt vc-git-print-log-follow nil)
(setopt vc-git-shortlog-switches nil)

;;; vc-got
(let ((vc-got-repo-dir (expand-file-name "~/got/vc-got")))
  (if (file-directory-p vc-got-repo-dir)
      (add-to-list 'load-path vc-got-repo-dir)
    (ensure-packages-present 'vc-got))
  (add-to-list 'vc-handled-backends 'Got)
  (add-to-list 'vc-directory-exclusion-list ".got"))

;; project-vc-dir or vc-dir {C-x p v} or {C-x v d}
;; vc-dir binds:
;; {z p}, {z s}, {z c} for stashing

;; From a diff-mode  {C-x v =}
;; - drop a hunk: k
;; - reverse direction: {C-c C-r}
;; - apply hunk {C-c C-a}
;; - split a hunk: C-c C-s
;; - commit remaining diff: C-x v v
;; TODO: how to get diff to update after dropping a hunk: was this solved in emacs-master?

;;; xref
;; M-x xref-query-replace-in-results
;; M-x xref-find-references-and-replace

;;; compile
(setopt compilation-save-buffers-predicate nil)
(setopt compilation-scroll-output 'first-error)
(setopt compilation-ask-about-save nil)
(setopt compilation-always-kill t)
(setopt compilation-window-height 12)
(setopt ansi-color-for-compilation-mode t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Add biome lint support
(add-to-list 'compilation-error-regexp-alist-alist
             '(biome
               "^\\(\\(?:[[:alpha:]]:\\)?[^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:[[:space:]]+.*\\)?$"
               1 2 3))
(add-to-list 'compilation-error-regexp-alist 'biome)

;;; smerge-mode
;; or use smerge-ediff to resolve conflicts
(setopt smerge-command-prefix (kbd "C-c v")) ;; XXX: check this

;;; diff
;; {C-c RET a} 'diff-apply-buffer'
(setopt diff-advance-after-apply-hunk t)
(setopt diff-default-read-only t)
(setopt diff-font-lock-prettify nil)
(setopt diff-font-lock-syntax 'hunk-also)
(setopt diff-refine 'font-lock) ; 'navigation
(setopt diff-update-on-the-fly t)
(setopt diff-add-log-use-relative-names t)
(setopt diff-refine-nonmodified t)
(setopt diff-ignore-whitespace-switches "-b")
(setopt diff-switches '("-u"))

;; 'diff-revert-and-kill-hunk' bound to 'u' and 'C-c M-u'.
;; 'v' is now bound to 'vc-next-action' in read-only Diff mode buffers.
;; 's' is now bound to 'diff-split-hunk' in read-only Diff mode buffers.

;;; ediff
(setopt ediff-window-setup-function 'ediff-setup-windows-plain)
(setopt ediff-split-window-function 'split-window-horizontally)
(setopt ediff-diff-options "-w")
(setopt ediff-keep-variants nil)
(setopt ediff-make-buffers-readonly-at-startup nil)
(add-hook 'ediff-after-quit-hook-internal-hook 'winner-undo)

;;; prog-mode

(defun my/prog-mode-hook ()
  "Hook to run when entering generic prog-mode."
  (setq-local which-func-unknown "TOP LEVEL")
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                                 1 font-lock-warning-face prepend))))

(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; enable which-func on programming modes
;; NOTE: enable which-func only on prog-mode instead of globally.
;; This is to avoid having it enabled in diff-mode, which causes cpu use
;; due to looping in git remote call
(setopt which-func-modes '(prog-mode))
(which-function-mode)

(with-eval-after-load 'eglot
  (setopt eglot-autoshutdown t)
  (setopt eglot-extend-to-xref t)
  (setopt eglot-events-buffer-config '(:size 0 :format full))
  (setopt eglot-prefer-plaintext t)
  (setq jsonrpc-event-hook nil)
  ;; Check these:
  ;;(setopt eglot-report-progress nil)
  ;;(setopt eglot-code-action-indications nil) ;; emacs31
  (keymap-set eglot-mode-map "C-c e h" 'eglot-help-at-point)
  (keymap-set eglot-mode-map "C-c e a" 'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c e o" 'eglot-action-organize-imports)
  (keymap-set eglot-mode-map "C-c e f" 'eglot-format)
  (keymap-set eglot-mode-map "C-c e r" 'eglot-rename))

;; (add-hook 'eglot-managed-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;;; editorconfig

(add-hook 'after-init-hook 'editorconfig-mode)

;; Java: checkout lombok annotations
;; JAVA_TOOL_OPTIONS="-javaagent:<lombok>"

(defun list-lombok-jars ()
  "List the lombok files in current project."
  (interactive)
  ;; ensure we are in correct work dir
  (with-temp-buffer
    (shell-command "env -i mvn dependency:list" t)
    (keep-lines "module lombok")
    (search-forward "jar") ;; TODO: fail if not found
    (forward-char)
    (zap-to-char 1 ":")
    (concat "lombok-" (yank-pop) ".jar")))

;; flymake
(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error))

;;; Go programming
(setenv "GOPATH" (expand-file-name "workspace" "~"))

;; (add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook 'eglot-ensure)
;; (ensure-packages-present '(go-mode go-eldoc))
;; (with-eval-after-load 'go-mode
;;   (let ((m go-mode-map))
;;     (define-key m (kbd "M-.") 'godef-jump)
;;     (define-key m (kbd "C-c C-r") 'go-remove-unused-imports)
;;     (define-key m (kbd "C-c g i") 'go-goto-imports)
;;     (define-key m (kbd "C-c C-k") 'godoc))

  ;; (require 'go-eldoc nil t)
  ;; (add-hook 'go-mode-hook 'go-eldoc-setup))

;;; Ruby
(dolist (m '(("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-ts-mode)
             ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"  . ruby-ts-mode)))
  (add-to-list 'magic-mode-alist m))

(defun my/ruby-ts-mode-hook ()
  (setopt ruby-deep-arglist t)
  (setopt ruby-deep-indent-paren nil)
  (setq-local c-tab-always-indent nil))
(add-hook 'ruby-mode-hook 'my/ruby-ts-mode-hook)

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
    (setq-local indent-tabs-mode t))

  (defun my/c++-mode ()
    "My C++ programming options."
    (setq fill-column 100)
    (c-set-style "stroustrup")
    (setq-local whitespace-line-column 100
                whitespace-style '(face lines-tail))))

;;; Perl
(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))

(setopt cperl-font-lock t)
(setopt cperl-info-on-command-no-prompt t)
(setopt cperl-clobber-lisp-bindings t)
(setopt cperl-lazy-help-time 5)
(setopt cperl-indent-level 4)
(setopt cperl-invalid-face 'default)

;;; magit

(ensure-packages-present 'magit)
(setopt magit-repository-directories
        '(("~/git" . 1)
          ("~/quicklisp/local-projects" . 1)
          ("~/common-lisp" . 1)))
(add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
;; Or use C-RET when in magit diff to go to actual file
(setopt magit-diff-visit-prefer-worktree t)

;; use magit recommended key bindings
(keymap-global-set "C-c g" 'magit-status)
(keymap-global-set "C-c f" 'magit-file-dispatch)
(keymap-global-set "C-c F" 'magit-dispatch)

;;; magit-gitflow

(when (is-work-laptop-p)
  (ensure-packages-present 'magit-gitflow)
  (require 'magit-gitflow nil t)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))


(provide 'init-programming)

;; init-programming.el ends here
