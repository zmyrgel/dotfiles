;;; ------------------------------
;;; Completion
;;; ------------------------------

(use-package orderless
  :ensure t)

(use-package marginalia
  :ensure t
  :config
  (setq marginalia-max-relative-age 0)
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package corfu
  :ensure t
  :config (global-corfu-mode))

(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package minibuffer
  :after (orderless)
  :config
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
  (minibuffer-electric-default-mode 1))

(use-package imenu
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-max-item-length 100)
  (setq imenu-space-replacement ".")
  (setq imenu-level-separator ":"))

(use-package hippie-exp
  :config
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(provide 'zmg-emacs-completion)
