;;; init-completion.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Completion related configuration
;;; - finish corfu, consult?
;;; - hippie-expand use?

;;; Code:

;; Hide commands in M-x which do not apply to the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setopt tab-always-indent 'complete)

;;; minibuffer
(setopt completion-styles '(basic partial-completion flex))
(setopt completion-category-overrides
        '((file (styles . (basic partial-completion flex)))
          (project-file (styles . (basic substring partial-completion flex)))
          (imenu (styles . (basic substring flex)))
          (kill-ring (styles . (basic substring flex)))))

(setopt completion-auto-deselect t)
(setopt completion-auto-help 'visible)
(setopt completion-auto-select 'second-tab)
(setopt completion-auto-wrap t)
(setopt completion-cycle-threshold nil)
(setopt completion-eager-display 'auto)
(setopt completion-eager-update 'auto)
(setopt completion-flex-nospace nil)
(setopt completion-ignore-case t)
(setopt completion-pcm-complete-word-inserts-delimiters nil)
(setopt completion-pcm-leading-wildcard t)
(setopt completion-pcm-word-delimiters "-_./:| ")
(setopt completion-show-help nil)

(setopt completions-format 'one-column)
(setopt completions-max-height 20)
(setopt completions-detailed t)
(setopt completions-group t)
(setopt completions-group-sort 'alphabetical)
(setopt completions-sort 'historical)
(setopt completions-header-format #("%s possible completions:
" 0 25 (face shadow)))

(setopt echo-keystrokes 1)
(setopt suggest-key-bindings t)
(setopt read-answer-short t)

(setopt minibuffer-completion-auto-choose t) ;; was nil
(setopt minibuffer-beginning-of-buffer-movement t)
(setopt minibuffer-default-prompt-format " [%s]")
(setopt minibuffer-visible-completions t)
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'after-init-hook 'file-name-shadow-mode)
(add-hook 'after-init-hook 'minibuffer-depth-indicate-mode)
(add-hook 'after-init-hook 'minibuffer-electric-default-mode)
(add-hook 'after-init-hook 'minibuffer-regexp-mode)

;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
(defun my/truncate-lines ()
  (setq truncate-lines t))

(add-hook 'minibuffer-setup-hook 'my/truncate-lines)

;; Select completion options with up/down when completing in the
;; minibuffer or normal buffer
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

;;; completion preview
(global-completion-preview-mode)
(setopt completion-preview-exact-match-only nil)
(setopt completion-preview-minimum-symbol-length 3)
(setopt completion-preview-idle-delay 0.3)
(define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate)

;;; imenu: M-g i
(setopt imenu-auto-rescan t)
(setopt imenu-max-item-length 100)
(setopt imenu-space-replacement ".")
(setopt imenu-level-separator ":")

;;; hippie-exp
(define-key global-map [remap dabbrev-expand] 'hippie-expand)

(setopt dabbrev-abbrev-skip-leading-regexp "[$*/=']")
(setopt dabbrev-backward-only nil)
(setopt dabbrev-case-distinction 'case-replace)
(setopt dabbrev-case-fold-search 'case-fold-search)
(setopt dabbrev-case-replace 'case-replace)
(setopt dabbrev-check-other-buffers t)
(setopt dabbrev-eliminate-newlines nil)
(setopt dabbrev-upcase-means-case-search t)

;; todo:
;; ecomplete: add mail entries?

(provide 'init-completion)

;;; init-completion.el ends here
