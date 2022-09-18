;;; ------------------------------
;;; Completion
;;; ------------------------------

(ensure-packages-present '(orderless marginalia embark corfu vertico))

(setq marginalia-max-relative-age 0)
(marginalia-mode)

;;(zmg/with-package 'embark
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

;; imenu
(setq imenu-auto-rescan t)
(setq imenu-max-item-length 100)
(setq imenu-space-replacement ".")
(setq imenu-level-separator ":")

;; hippie-exp
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;(zmg/with-package 'dabbrev
(setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
(setq dabbrev-backward-only nil)
(setq dabbrev-case-distinction 'case-replace)
(setq dabbrev-case-fold-search 'case-fold-search)
(setq dabbrev-case-replace 'case-replace)
(setq dabbrev-check-other-buffers t)
(setq dabbrev-eliminate-newlines nil)
(setq dabbrev-upcase-means-case-search t)

(provide 'zmg-emacs-completion)
