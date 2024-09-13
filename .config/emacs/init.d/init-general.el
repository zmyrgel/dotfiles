;;; init-general.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - General emacs init settings

;;; Code:

(defun prepend-to-exec-path (path)
  "Add given PATH to beginning of exec-path if it exists."
  (let ((full-path (expand-file-name path)))
    (when (file-exists-p full-path)
      (add-to-list 'exec-path full-path))))

(defun password-lookup (&rest keys)
  "Lookup password from auth-sources filtered by given KEYS."
  (when-let ((result (apply #'auth-source-search keys)))
    (funcall (plist-get (car result) :secret))))

(dolist (p '("~/bin" "~/.local/bin" "~/workspace/bin" "~/opt/bin"))
  (prepend-to-exec-path p))

(add-hook 'after-init-hook 'delete-selection-mode)

(ensure-packages-present 'diminish)
(diminish 'eldoc-mode)
(diminish 'whitespace-mode)
(diminish 'subword-mode)

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

(when (version<= "28" emacs-version)
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  (repeat-mode 1))

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
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

;; buffer switching
(setq switch-to-prev-buffer-skip-regexp nil)
(define-key ctl-x-x-map "p" #'switch-to-prev-buffer)
(define-key ctl-x-x-map "n" #'switch-to-next-buffer)

;; buffers to registers, C-x r j m
(when (version<= "29" emacs-version)
  (set-register ?m '(buffer . "*Messages*")))

(setq Man-prefer-synchronous-call t)
(setq Man-support-remote-systems t)

(provide 'init-general)

;; init-general.el ends here
