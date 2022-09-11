;;; ------------------------------
;;; General
;;; ------------------------------

(defun prepend-to-exec-path (path)
  "Add given PATH to beginning of exec-path if it exists."
  (let ((full-path (expand-file-name path)))
    (when (file-exists-p full-path)
      (add-to-list 'exec-path full-path))))

(dolist (p '("~/bin" "~/.local/bin" "~/workspace/bin"))
  (prepend-to-exec-path p))

(add-hook 'after-init-hook 'delete-selection-mode)

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

  ;; :commands (ispell-change-dictionary
  ;;            ispell-word
  ;;            flyspell-buffer
  ;;            flyspell-mode
  ;;            flyspell-region)

  ;;:hook text-mode-hook
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_US")

(zmg/package-install 'wcheck-mode)
(let ((map (make-sparse-keymap)))
  (define-key map "w" 'wcheck-mode)
  (define-key map "l" 'wcheck-change-language)
  (define-key map "a" 'wcheck-actions)
  (define-key map "f" 'wcheck-jump-forward)
  (define-key map "b" 'wcheck-jump-backward)
  (define-key ctl-x-x-map "w" map))

(setq wcheck-language-data
      `(("British English"
         (program . ,(or (executable-find "ispell") "ispell"))
         (args "-l" "-d" "british")
         (action-program . ,(or (executable-find "ispell") "ispell"))
         (action-args "-a" "-d" "british")
         (action-parser . wcheck-parser-ispell-suggestions))
        ("Finnish"
         (program . ,(or (executable-find "enchant-2")
                         (executable-find "enchant")))
         (args "-l" "-d" "fi")
         (syntax . my-finnish-syntax-table)
         (action-program . "/usr/bin/enchant")
         (action-args "-a" "-d" "fi")
         (action-parser . wcheck-parser-ispell-suggestions))))

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

(provide 'zmg-emacs-general)
