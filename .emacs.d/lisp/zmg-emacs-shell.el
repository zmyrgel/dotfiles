;;; ------------------------------
;;; Shell settings
;;; ------------------------------

;; FIXME: rely on system config
(when (file-exists-p "~/share/man")
  (setenv "MANPATH" "~/share/man:/usr/share/man:/usr/local/share/man"))

(defun my/sh-mode-hook ()
  (set (make-local-variable 'indent-tabs-mode) t))
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

(zmg/with-package 'eshell
  
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
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (setq eshell-visual-options '(("git" "--help" "--paginate"))))

(provide 'zmg-emacs-shell)
