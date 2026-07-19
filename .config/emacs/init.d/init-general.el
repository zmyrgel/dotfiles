;;; init-general.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - General emacs init settings

;;; Code:

(defun prepend-to-exec-path (path)
  "Add given PATH to beginning of exec-path if it exists."
  (let ((full-path (expand-file-name path)))
    (when (file-directory-p full-path)
      (add-to-list 'exec-path full-path))))

(defun password-lookup (&rest keys)
  "Lookup password from auth-sources filtered by given KEYS."
  (when-let* ((result (apply #'auth-source-search keys)))
    (funcall (plist-get (car result) :secret))))

(dolist (p '("~/bin" "~/.local/bin" "~/workspace/bin" "~/opt/bin"))
  (prepend-to-exec-path p))

;; XXX: does not work in 31.1?
(add-hook 'after-init-hook 'delete-selection-mode)

(setq mode-line-collapse-minor-modes
      '(abbrev-mode flyspell-mode flyspell-prog-mode eldoc-mode
                    subword-mode auto-revert-mode whitespace-mode
                    completion-preview-mode))

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | C-s C-w   | Search char or word at point |
;; | M-s .     | Similar, but broader match   |
;; | M-s o     | Run `occur' on regexp        |
;; | M-s h r   | Highlight regexp             |
;; | M-s h u   | Undo the highlight           |
;; | C-s M-r   | Toggle regexp search         |
;; | M-%       | Run `query-replace'          |
;; | C-M-%     | `query-replace-regexp'       |

(setq isearch-highlight t)
(setq isearch-lazy-highlight t)
(setq isearch-lazy-count t)
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)
(setq search-whitespace-regexp ".*?")
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " (%s/%s)")
(setq isearch-yank-on-move 'shift)
(setq isearch-allow-scroll 'unlimited)
(setq query-replace-highlight t)

;; Clipboard stuff
;; (setq save-interprogram-paste-before-kill t)
;; (setq yank-pop-change-selection t)

;; mouse options
(setq mouse-wheel-scroll-amount
      '(1
        ((shift) . 5)
        ((meta) . 0.5)
        ((control) . text-scale)))
(setq mouse-drag-copy-region nil)
(setq make-pointer-invisible t)
(setq mouse-yank-at-point t)
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse t)
(when (fboundp 'context-menu-mode)
  (context-menu-mode 1))
(add-hook 'after-init-hook 'mouse-wheel-mode)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(add-hook 'before-save-hook 'time-stamp)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(global-so-long-mode 1)

(setq repeat-on-final-keystroke t)
(setq set-mark-command-repeat-pop t)
(repeat-mode 1)

;; bump undo limits a bit
(setq undo-limit (* 13 160000))
(setq undo-strong-limit (* 13 240000))
(setq undo-outer-limit (* 13 24000000))

;;; ------------------------------
;;; Buffer management
;;; ------------------------------

;; M-x rename-visited-file

;; uniquify
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-separator ":")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-expert t)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-human-readable-size t)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

;; Buffer-menu
(setq Buffer-menu-group-by '(Buffer-menu-group-by-root))

;; buffer switching
(setq switch-to-prev-buffer-skip-regexp nil)
(define-key ctl-x-x-map "p" #'switch-to-prev-buffer)
(define-key ctl-x-x-map "n" #'switch-to-next-buffer)

;; delete pair
(setopt delete-pair-blink-delay 0)
(setopt delete-pair-push-mark t)
(global-set-key (kbd "M-s d") #'delete-pair)

;; buffers to registers, C-x r j m
;; {C-u 99 C-x e} run macro for 99 times
;; {C-u 0 C-x e} run macro until bell
(set-register ?m '(buffer . "*Messages*"))

(setq Man-prefer-synchronous-call t)
(setq Man-support-remote-systems t)

(setq remember-data-file (expand-file-name "~/Documents/notes")
      remember-notes-initial-major-mode 'org-mode
      remember-notes-auto-save-visited-file-name t)

;;; PROCED
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))


(provide 'init-general)

;; init-general.el ends here
