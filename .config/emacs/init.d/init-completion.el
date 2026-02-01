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
(setq corfu-echo-documentation '(1.0 . 0.2));; Disable documentation in the echo area
(setq corfu-scroll-margin 2)                ;; Use scroll margin

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(setq suggest-key-bindings t)

(require 'vertico nil t)
(vertico-mode 1)

;; minibuffer
(setq completion-styles '(orderless))

;; First it checks the category defaults in completion-category-defaults.
;; Next, it checks if there are any overrides in completion-category-overrides. If there are, it uses them instead of #1.
;; Then, it merges the styles from #1 or #2 with completion-styles. The category-specific styles in #1 or #2 take precedence over the styles in completion-styles.

(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles . (basic partial-completion orderless)))
        (project-file (styles . (basic substring partial-completion orderless)))
        (imenu (styles . (basic substring orderless)))
        (kill-ring (styles . (basic substring orderless)))))
(setq completion-cycle-threshold nil)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters nil)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-ignore-case t)
(setq completion-auto-wrap t)
(setq completion-auto-help 'visible)  ; 'visible 'always 'lazy t nil

(setq completion-auto-deselect t) ;; check
;; (setq completion-auto-select 'second-tab) ;; nil t second-tab ;; check

(setq completions-detailed t)
(setq completions-group t)
(setq completions-group-sort 'alphabetical)
(setq completions-sort 'historical)
(setq completions-header-format #("%s possible completions:
" 0 25 (face shadow)))

(setq completions-format 'one-column)
(setq completion-show-help t)

(setq echo-keystrokes 0.5)
(setq read-answer-short t)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)
(minibuffer-regexp-mode 1)
(setq minibuffer-beginning-of-buffer-movement t)
(setq minibuffer-eldef-shorten-default t)
(setq minibuffer-visible-completions t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
(add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines t)))

  ;;; Following should be used when corfu/vertico is not used:
(unless (or (featurep 'corfu) (featurep 'vertico))
  ;; previous-line-completion, next-line-completion commands

  ;; Select completion options with up/down when completing in the minibuffer
  (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

  ;; Up/down when competing in a normal buffer
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion))

;; icomplete

;; show only icomplete in-buffer display and not *Completions* buffer
;; (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
;; (setq icomplete-in-buffer t)

;; completion preview
;; (global-completion-preview-mode)
(setq completion-preview-exact-match-only nil)
(setq completion-preview-minimum-symbol-length 3)
(setq completion-preview-idle-delay nil)

;; imenu: M-g i
(setq imenu-auto-rescan t)
(setq imenu-max-item-length 100)
(setq imenu-space-replacement ".")
(setq imenu-level-separator ":")

;; hippie-exp
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; TODO: pabbrev?
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
