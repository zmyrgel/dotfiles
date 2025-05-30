;;; init-shell.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Shell related settings

;;; Code:

;; FIXME: rely on system config, use man command flags instead of env?
;; TODO: really need to check dir exists?
(let ((home-man (expand-file-name "~/share/man"))
      (man-path (split-string (or (getenv "MANPATH") "") ":")))
  (when (file-exists-p home-man)
    (add-to-list 'man-path home-man)
    (setenv "MANPATH" (string-join man-path ":"))))

(setq shell-kill-buffer-on-exit t)

(defun my/sh-mode-hook ()
  (set (make-local-variable 'indent-tabs-mode) t)
  ;; ensure this matches tab-width
  (set (make-local-variable 'sh-basic-offset) 8))
(add-hook 'sh-mode-hook 'my/sh-mode-hook)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-scroll-show-maximum-output t)
(setq comint-completion-autolist t)
(setq comint-input-ignoredups t)
(setq comint-completion-addsuffix t)
(setq comint-prompt-read-only t)
(add-hook 'comint-mode-hook #'(lambda ()
                                (define-key comint-mode-map [remap kill-region]
                                  'comint-kill-region)
                                (define-key comint-mode-map [remap kill-whole-line]
                                  'comint-kill-whole-line)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
	 (height (/ (window-total-height) 3))
	 (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))
(global-set-key (kbd "C-!") 'eshell-here)

;; smart shell
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
(setq eshell-save-history-on-exit t)
(setq eshell-scroll-show-maximum-output t)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-cmpl-autolist t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-cmpl-cycle-cutoff-length 2)
(setq eshell-cmpl-ignore-case t)
(setq eshell-cp-overwrite-files nil)
(setq eshell-default-target-is-dot t)
(setq eshell-destroy-buffer-when-process-dies t)
(setq eshell-hist-ignoredups t)
(setq eshell-list-files-after-cd t)
(setq eshell-review-quick-commands t)
(setq eshell-save-history-on-exit t)
(setq eshell-scroll-show-maximum-output nil)
(setq eshell-visual-subcommands '(("git" "log" "diff" "show" "branch")))
(setq eshell-visual-options '(("git" "--help" "--paginate")))
(setq eshell-hist-ignoredups t)
(setq eshell-history-append t)

(provide 'init-shell)

;; init-shell.el ends here
