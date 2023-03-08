;;; init-completion.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Completion related configuration
;;; - finish corfu, consult?
;;; - hippie-expand use?

;;; Code:

(ensure-packages-present '(orderless marginalia embark corfu vertico))

(setq marginalia-max-relative-age 0)
(marginalia-mode)

(setq prefix-help-command #'embark-prefix-help-command)
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
	       nil
	       (window-parameters (mode-line-format . none))))
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-.") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

(require 'corfu nil t)
(global-corfu-mode)

(setq corfu-cycle nil)                      ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto nil)                       ;; Enable auto completion
(setq corfu-separator ?\s)                  ;; Orderless field separator
(setq corfu-quit-at-boundary 'separator)    ;; Never quit at completion boundary
(setq corfu-quit-no-match 'separator)       ;; Never quit, even if there is no match
(setq corfu-preview-current 'insert)        ;; Disable current candidate preview
(setq corfu-preselect-first t)              ;; Disable candidate preselection
(setq corfu-on-exact-match 'insert)         ;; Configure handling of exact matches
(setq corfu-echo-documentation '(1.0 . 0.2) ;; Disable documentation in the echo area
(setq corfu-scroll-margin 2)                ;; Use scroll margin

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(require 'vertico nil t)
(vertico-mode)

;; minibuffer
(setq completion-styles '(orderless))
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles . (basic partial-completion orderless)))
        (project-file (styles . (basic substring partial-completion orderless)))
        (imenu (styles . (basic substring orderless)))
        (kill-ring (styles . (basic substring orderless)))))
(setq completion-cycle-threshold 2)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters nil)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completions-detailed t)
(when (version<= "28" emacs-version)
  (setq completions-group t)
(setq completions-group-sort 'alphabetical)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt)))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq minibuffer-beginning-of-buffer-movement t)
(setq completions-format 'one-column)
(setq completion-show-help nil)
(setq minibuffer-eldef-shorten-default t)
(setq echo-keystrokes 0.5)
(setq read-answer-short t)
(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

(when (version<= "29" emacs-version)
  (setq completion-auto-select t)
  (setq completion-auto-wrap t)
  (setq completions-sort 'alphabetical)
  ;;(setq completions-header-format %s / nil)

  ;; (setq completion-auto-help 'visible) ;; t lazy always visible
  ;; (setq completion-auto-select 'second-tab) ;; nil t second-tab

  ;; Up/down when completing in the minibuffer
  (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

  ;; Up/down when competing in a normal buffer
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion))

;; imenu: M-g i
(setq imenu-auto-rescan t)
(setq imenu-max-item-length 100)
(setq imenu-space-replacement ".")
(setq imenu-level-separator ":")

;; hippie-exp
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
(setq dabbrev-backward-only nil)
(setq dabbrev-case-distinction 'case-replace)
(setq dabbrev-case-fold-search 'case-fold-search)
(setq dabbrev-case-replace 'case-replace)
(setq dabbrev-check-other-buffers t)
(setq dabbrev-eliminate-newlines nil)
(setq dabbrev-upcase-means-case-search t)

(provide 'init-completion)

;;; init-completion.el ends here
