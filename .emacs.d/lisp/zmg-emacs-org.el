;;; zmg-emacs-org.el --- Org-mode settings  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; -

;;; Code:

(setq org-list-allow-alphabetical t)

(with-eval-after-load 'org
  (setq org-directory "/ssh:tmy@mars.bittivirhe.fi:Org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-file-regexp "\\(school\\|todo\\|work\\)\\.org")
  (setq org-outline-path-complete-in-steps nil)
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'note)
  (setq org-startup-indented t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                            (sequence "|" "CANCELED(c@/!)")
                            (sequence "STALLED(s@/!)" "|")
                            (sequence "PENDING(p@/!)" "|")))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?B)
  (setq org-tag-alist ;; use these or set file tags?
	'(("work" . ?w)
          ("emacs" . ?e)
          ("study" . ?s)
          ("mail" . ?m)))
  (setq org-confirm-babel-evaluate t)
  (setq org-log-done 'note)
  (setq org-log-note-clock-out t)
  (setq org-read-date-prefer-future t)
  (setq org-adapt-indentation nil)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 3)
  ;; allow shell execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)

  ;; Use fixed-pitch font to keep tables aligned
  (defun set-buffer-variable-pitch ()
    (interactive)
    (variable-pitch-mode t)
    (setq line-spacing 3)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

  (add-hook 'org-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'Info-mode-hook 'set-buffer-variable-pitch)

  ;;  ol
  (setq org-link-keep-stored-after-insertion t)
  (define-key org-mode-map (kbd "C-c L") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c C-y") 'org-insert-last-stored-link)

  ;;  org-capture
  (let ((todo-template (concat "* TODO %^{Title}\n"
                               ":PROPERTIES:\n"
                               ":CAPTURED: %U\n"
                               ":END:\n\n"
                               "%i%l")))
    ;; "* TODO %?\n  %i\n  %a"
    (setq org-capture-templates
	  `(("t" "Todo" entry (file+headline "todo.org" "Tasks")
             ,todo-template)
            ("s" "Study" entry (file+headline "study.org" "Study stuff")
             ,todo-template)
            ("w" "Work tasks" entry (file+headline "work.org" "Work tasks")
             ,todo-template)
            ("n" "Notes" entry (file+datetree "notes.org")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("j" "Journal" entry (file+datetree "journal.org")
             "* %?\nEntered on %U\n  %i\n  %a"))))

  (setq org-capture-templates-contexts
	'(("r" ((in-mode . "gnus-article-mode")
		(in-mode . "gnus-summary-mode")))))

  ;; org-agenda
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-follow-indirect t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 7)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)

  ;; org-src
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)

  ;; ox
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 3)
  (setq org-export-dispatch-use-expert-ui nil)

  ;; ox-latex
  (setq org-latex-pdf-process
	'("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

  ;; ox-publish
  (setq org-publish-project-alist
	'(("blog"
           :base-directory "/ssh:tmy@mars.bittivirhe.fi:Org/blog/posts/"
           :base-extension "org"
           :publishing-directory "/ssh:tmy@mars.bittivirhe.fi:public/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :auto-sitemap t)
          ("all" :components ("blog")))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	       t)
  (add-to-list 'org-latex-classes
	       '("koma-article" "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	       t))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'zmg-emacs-org)

;; zmg-emacs-org.el ends here
