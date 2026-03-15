;;; init-completion.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Completion related configuration
;;; - finish corfu, consult?
;;; - hippie-expand use?

;;; Code:

(ensure-packages-present '(marginalia))

(setq marginalia-max-relative-age 0)
(marginalia-mode)

;; Hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; minibuffer
;;(setq completion-styles '(basic partial-completion emacs22))
(add-to-list 'completion-styles 'flex t)

(setq completion-auto-deselect t)
(setq completion-auto-help 'visible)  ; 'visible 'always 'lazy t nil
(setq completion-auto-select 'second-tab) ;; nil t second-tab ;; check
(setq completion-auto-wrap t)
(setq completion-cycle-threshold nil)
(setq completion-eager-display 'auto) ; 'auto
(setq completion-eager-update 'auto) ; 'auto
(setq completion-flex-nospace nil)
(setq completion-ignore-case t)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq completion-pcm-leading-wildcard t)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-show-help nil)

(setq completions-format 'one-column)
(setq completions-max-height 20) ; nil
(setq completions-detailed t)
(setq completions-group t)
(setq completions-group-sort 'alphabetical)
(setq completions-sort 'historical)
(setq completions-header-format #("%s possible completions:
" 0 25 (face shadow)))

(setq echo-keystrokes 1)
(setq suggest-key-bindings t)
(setq read-answer-short t)

(setq minibuffer-completion-auto-choose t) ;; was nil
(setq minibuffer-beginning-of-buffer-movement t)
(setq minibuffer-default-prompt-format " [%s]")
(setq minibuffer-visible-completions t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)
(minibuffer-regexp-mode 1)

  ;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
(add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines t)))

;; previous-line-completion, next-line-completion commands
;; Select completion options with up/down when completing in the minibuffer
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

;; icomplete

;;(icomplete-vertical-mode -1)
;; Do not show completions buffer when also showing in-buffer options
;;(advice-add 'completion-at-point :after #'minibuffer-hide-completions)
;; show only icomplete in-buffer display and not *Completions* buffer
;;(setq icomplete-in-buffer nil) ;; t

;; completion preview
(global-completion-preview-mode)
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

(setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
(setq dabbrev-backward-only nil)
(setq dabbrev-case-distinction 'case-replace)
(setq dabbrev-case-fold-search 'case-fold-search)
(setq dabbrev-case-replace 'case-replace)
(setq dabbrev-check-other-buffers t)
(setq dabbrev-eliminate-newlines nil)
(setq dabbrev-upcase-means-case-search t)

;; todo:
;; ecomplete: add mail entries?

(provide 'init-completion)

;;; init-completion.el ends here
